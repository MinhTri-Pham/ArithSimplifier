import java.util.concurrent.atomic.AtomicLong
import scala.collection.mutable.ListBuffer
import NotEvaluableException.NotEvaluable

abstract sealed class ArithExpr {
  // Addition operator
  def +(that: ArithExpr) : ArithExpr = SimplifySum(this, that)

  // Subtraction operator (x-y = x+(-1)*y)
  def -(that : ArithExpr) : ArithExpr = SimplifySum(this, Cst(-1)*that)

  // Multiply operator
  def *(that: ArithExpr) : ArithExpr = SimplifyProd(this, that)

  // Ordinal division operator (x/y = x*(y pow -1))
  def /^(that: ArithExpr) : ArithExpr = SimplifyProd(this, that pow -1)

  // Integer division
  def /(that: ArithExpr) : ArithExpr = SimplifyIntDiv(this, that)

  // Exponentiation operator
  def pow(that: Int) : ArithExpr = SimplifyPow(this, that)

  // Returns list of terms or factors for Sum and Prod simplification
  def getSumProdSimplify : List[ArithExpr]

  // Similarly as above but for sums returns the whole sum
  def getSumProdFactorise : List[ArithExpr]

  // Convert expression to variable if possible
  lazy val toVar : Option[Var] = this match {
    case x: Var => Some(x)
    case _ => None
  }

  // Convert expression to sum if possible
  lazy val toSum : Option[Sum] = this match {
    case x:Sum => Some(x)
    case x:Prod => x.asExpandedSum
    case x:Pow => x.asSum
    case _ => None
  }

  // Convert expression to product if possible
  lazy val toProd : Option[Prod] = this match {
    case x:Prod => Some(x)
    case x:Sum => x.asProd
    case x:Pow => x.asProd
    case _ => None
  }

  // Convert expression to power if possible
  lazy val toPow : Option[Pow] = this match {
    case x:Prod => x.asPow
    case x:Pow => Some(x)
    case _ => None
  }

  lazy val sign: Sign.Value = Sign(this)

  lazy val evalDouble: Double = ArithExpr.evalDouble(this)

  lazy val (min: ArithExpr, max: ArithExpr) = _minmax()

  private def _minmax(): (ArithExpr, ArithExpr) = this match {
    case c:Cst => (c,c)
    case v:Var => (v.range.intervalMin, v.range.intervalMax)
    case Sum(terms) => (terms.map(_.min).reduce[ArithExpr](_ + _), terms.map(_.max).reduce[ArithExpr](_ + _))
    case Prod(factors) =>
      val prodInterval = ArithExpr.computeIntervalProd(factors)
      (prodInterval.intervalMin, prodInterval.intervalMax)

    case Pow(b,e) =>
      if (e == 0) (Cst(1), Cst(1))
      // Odd exponent - easy
      else if (e > 0 && e % 2 == 1) (b.min pow e, b.max pow e)
      // Even exponent, consider sign of min/max of b
      else if (e > 0 && e % 2 == 0) {
        if (b.min.sign.equals(Sign.Positive)) (b.min pow e, b.max pow e)
        else if (b.max.sign.equals(Sign.Negative)) (b.max pow e, b.min pow e)
        // (0, max(x1^e,x2^e)), x1 = min(b), x2 = max(b)
        else {
          val x1 = b.min pow e
          val x2 = b.max pow e
          val comp = ArithExpr.isSmaller(x1,x2)
          if (comp.isDefined) {
            if (comp.get) (Cst(0), x2)
            else (Cst(0),x1)
          }
          else {
            (Cst(0), ?)
          }
        }
      }
      else {
        // Both positive or both negative easy
        if (b.min.sign.equals(Sign.Positive)) (b.max pow e, b.min pow e)
        else if (b.max.sign.equals(Sign.Negative)) (b.min pow e, b.max pow e)
        // Minimum negative and maximum positive
        else {
          // Even negative exponent
          if (e % 2 == 0) {
            // Try to evaluate min and max of base
            // Then use absolute value
            try {
              val minAbs = abs(b.min)
              val maxAbs = abs(b.max)
              val comp = ArithExpr.isSmaller(maxAbs,minAbs)
              if (comp.isDefined) {
                if (comp.get) (b.min pow e, b.max pow e)
                else (b.max pow e, b.min pow e)
              }
              else (?,?)
            } catch {
              case NotEvaluableException() => (?,?)
            }
          }
          // Odd negative easy
          else (b.min pow e, b.max pow e)
        }
      }
    case ? => (?,?)
    case _ => (?,?)
  }

  lazy val isEvaluable: Boolean = {
    !ArithExpr.visitUntil(this, x => {
      x.isInstanceOf[Var] || x == ?
    })
  }

}

// Class for (int) constants
case class Cst(value : Int) extends ArithExpr {

  // Prime decomposition
  lazy val asProd : Prod = Factorise(this).get.toProd.get

  override def getSumProdSimplify: List[ArithExpr] = List[ArithExpr](this)

  override def getSumProdFactorise: List[ArithExpr] = List[ArithExpr](this)

  override def equals(that: Any): Boolean = that match {
    case Cst(n) => n == value
    case _ => false
  }
  override def toString: String = {
    if (value >= 0) value.toString
    else s"(${value.toString})"
  }
}

// Class for variables
case class Var (name : String, range: Interval = Interval(), fixedId: Option[Long] = None) extends ArithExpr {

  val id: Long = {
    if (fixedId.isDefined)
      fixedId.get
    else {
      Var.incCnt
    }
  }

  override def getSumProdSimplify: List[ArithExpr] = List[ArithExpr](this)

  override def getSumProdFactorise: List[ArithExpr] = List[ArithExpr](this)

  override def equals(that: Any): Boolean = that match {
    case v: Var => this.id == v.id
    case _ => false
  }

  override def toString: String = name
}

// Companion object for Var class
object Var {
  private val cnt = new AtomicLong(-1) // Instance counter

  def incCnt : Long = {
    var _id: Long = 0
    do {
      _id = Var.cnt.incrementAndGet()
      if (_id < 0) {
        Var.cnt.compareAndSet(_id,0)
      }
    } while (_id < 0)
      _id
  }

  def apply(name: String): Var = new Var(name)

  def apply(name: String, fixedId: Option[Long]): Var = new Var(name, Interval(), fixedId)
}

// Class for sums
case class Sum(terms: List[ArithExpr]) extends ArithExpr {

  // Returns list of terms converted into products when possible for factorisation
  // The one exception are prime numbers
  lazy val asProds : List[ArithExpr] = {
    var prods = ListBuffer[ArithExpr]()
    for (t <- terms) t match {
      case c:Cst =>
        prods += c
      case v:Var => prods += v
      case p:Prod =>
        prods += p.primitiveProd

      case pow:Pow =>
        if (pow.asProdPows.isDefined) prods += pow.asProdPows.get.primitiveProd
        else prods += pow.asProd.get
    }
    prods.toList
  }

  lazy val asProd : Option[Prod] = {
    val factorisation = Factorise(this)
    if (factorisation.isDefined) factorisation.get.toProd
    else None
  }

  override def getSumProdSimplify: List[ArithExpr] = terms

  override def getSumProdFactorise: List[ArithExpr] = List[ArithExpr](this)

  override def equals(that: Any): Boolean = that match {
    case Sum(terms2) => terms.length == terms2.length && terms.intersect(terms2).length == terms.length
    case _ => false
  }

  override def toString: String = s"(${terms.mkString(" + ")})"
}

// Class for products
case class Prod(factors: List[ArithExpr]) extends ArithExpr {


  override def getSumProdSimplify: List[ArithExpr] = factors

  override def getSumProdFactorise: List[ArithExpr] = factors

  lazy val cstFactor : Int = factors.head match {
    case Cst(c) => c
    case _ => 1
  }

  lazy val nonCstList : List[ArithExpr] = {
    val nonCstFactors = ListBuffer[ArithExpr]()
    for (factor <- factors) {
      if (!factor.isInstanceOf[Cst]) nonCstFactors += factor
    }
    nonCstFactors.toList
  }

  lazy val nonCstFactor : ArithExpr = {
    if (cstFactor == 1) this
    else this.nonCstList.reduce((x, y) => x*y)
  }

  lazy val asNonCstFactorsSum : Option[Sum] = if (factors.length < 2 || cstFactor < 2) None else {
    val terms = ListBuffer[ArithExpr]()
    for (_ <- 0 until cstFactor) {
      terms += nonCstFactor
    }
    Some(Sum(terms.toList))
  }

  lazy val asExpandedSum : Option[Sum] = {
    if (factors.length <= 1) None
    else {
      var expanded = false
      var accum : ArithExpr = Cst(1)
      for (f <- factors) f match {
        case x @ (_:Cst | _:Var) =>
          if (accum.isInstanceOf[Sum]) {
            accum = ArithExpr.expand(accum, x).get
            expanded = true
          }
          else accum = x * accum
        case s:Sum =>
          accum = ArithExpr.expand(accum,s).get
          expanded = true
        case p:Pow =>
          val pSum = p.asSum
          if (pSum.isDefined) {
            accum = ArithExpr.expand(accum,pSum.get).get
            expanded = true
          }
          else if (accum.isInstanceOf[Sum]) {
            accum = ArithExpr.expand(accum,p).get
            expanded = true
          }
          else {
            if (p.e > 0) accum = accum * p
            else accum /^= p.b
          }
      }
      if (expanded) accum.getSumProdSimplify.reduce((x, y)=>x+y).toSum
      else None
    }
  }

  lazy val asPow : Option[Pow] = if (factors.length < 2) None else {
    if (nonCstList.distinct.length == 1) Some (Pow(nonCstList.head,nonCstList.length))
    else None
  }

  lazy val primitiveProd : Prod = {
    var primitiveFactors = ListBuffer[ArithExpr]()
    for (f <- factors) f match {
      case p:Pow =>
        if (p.asProdPows.isDefined) {
          val pProdPows = p.asProdPows.get
          primitiveFactors = primitiveFactors ++ pProdPows.primitiveProd.factors
        }
        else {
          val pProd = p.asProd.get
          primitiveFactors = primitiveFactors ++ pProd.factors
        }

      case _ => primitiveFactors += f
    }
    Prod(primitiveFactors.toList)
  }

  override def equals(that: Any): Boolean = that match {
    case Prod(factors2) => factors.length == factors2.length && factors.intersect(factors2).length == factors.length
    case _ => false
  }

  override def toString: String = factors.mkString(" * ")
}

// Class for powers, for now just integer exponents
case class Pow(b: ArithExpr, e: Int) extends ArithExpr {
  override def getSumProdSimplify: List[ArithExpr] = List[ArithExpr](this)

  override def getSumProdFactorise: List[ArithExpr] = this.asProd.get.factors

  // Expansion into sum
  lazy val asSum : Option[Sum] = if (e < 0 || !b.isInstanceOf[Sum]) None else {
    var combined : ArithExpr = Cst(0)
    for (n <- 1 to e) {
      if (n == 1)
        combined = b
      else
        combined = ArithExpr.expand(combined,b).get
    }
    Some(Sum(combined.getSumProdSimplify))
  }

  // Representation as a product (a^n = a*a...*a (n times))
  lazy val asProd : Option[Prod] = {
    //assume(e > 1)
    val factors = ListBuffer[ArithExpr]()
    if (e > 1) {
      for (_ <- 1 to e) {
        factors += b
      }
    }
    else if (e < 1) {
      for (_ <- e to -1) {
        factors += Pow(b,-1)
      }

    }
    // Exponent is 1 or -1
    else {
      factors += this
    }
    Some(Prod(factors.toList))
  }

  lazy val asProdPows : Option[Prod]= b match {
    case Prod(_) =>
      val bfacts = b.getSumProdSimplify
      val pfacts = ListBuffer[ArithExpr]()
      for (bfact <- bfacts) {
        pfacts += bfact pow e
      }
      Some(Prod(pfacts.toList))
    case Sum(_) =>
      if (b.toProd.isDefined) {
        val bfacts = b.toProd.get.factors
        val pfacts = ListBuffer[ArithExpr]()
        for (bfact <- bfacts) {
          pfacts += bfact pow e
        }
        Some(Prod(pfacts.toList))
      }
      else None
    case _ => None
  }

  override def equals(that: Any): Boolean = that match {
    case Pow(b2,e2) => b == b2 && e == e2
    case _ => false
  }

  override def toString: String = {
    if (e == -1) s"1/${b.toString}"
    else s"pow(${b.toString},$e)"
  }
}

object abs {
  def apply(ae: ArithExpr): ArithExpr = SimplifyAbs(ae)
}

case class AbsFunction(ae: ArithExpr) extends ArithExpr {

  override def toString: String = "Abs(" + ae + ")"

  override def getSumProdSimplify: List[ArithExpr] = List[ArithExpr](this)

  override def getSumProdFactorise: List[ArithExpr] = List[ArithExpr](this)
}

object ArithExpr {

  // Used for sorting terms of Sum or factors of Prod
  // For ease of simplification
  val isCanonicallySorted: (ArithExpr, ArithExpr) => Boolean = (x: ArithExpr, y: ArithExpr) =>
    (x, y) match {
    case (?, _) => true
    case (_, ?) => false
    case (Cst(a), Cst(b)) => a < b
    case (_: Cst, _) => true // constants first
    case (_, _: Cst) => false
    case (x: Var, y: Var) => x.id < y.id // order variables based on id

    // Want na (where n is a constant) < b (assuming a < b) for sum simplification
    case (p : Prod, x: Var) =>
      val nonCst = p.nonCstFactor
      if (nonCst.isInstanceOf[Var]) {
        if (nonCst == x) false
        else isCanonicallySorted(nonCst, x)
      }
      else false
    case (x: Var, p : Prod) =>
      val nonCst = p.nonCstFactor
      if (nonCst.isInstanceOf[Var]) {
        if (nonCst == x) true
        else isCanonicallySorted(x, nonCst)
      }
      else true

    // Want na (where n is a constant) < b (assuming a < b) for sum simplification
    case (p : Prod, x: Pow) =>
      val nonCst = p.nonCstFactor
      if (nonCst.isInstanceOf[Pow]) {
        if (nonCst == x) true
        else isCanonicallySorted(nonCst, x)
      }
      else true

    case (x: Pow, p : Prod) =>
      val nonCst = p.nonCstFactor
      if (nonCst.isInstanceOf[Var]) {
        if (nonCst == x) false
        else isCanonicallySorted(x, nonCst)
      }
      else false

    // Want a^n (where n is a constant) < b (assuming a < b) for product simplification
    case (p : Pow, x: Var) =>
      val base = p.b
      if (base.isInstanceOf[Var]) {
        if (base == x) false
        else isCanonicallySorted(p.b, x)
      }
      else false
    case (x: Var, p : Pow) =>
      val base = p.b
      if (base.isInstanceOf[Var]) {
        if (base == x) true
        else isCanonicallySorted(x, base)
      }
      else true

    case (_: Var, _) => true
    case (_, _: Var) => false

    case (p1:Prod, p2:Prod) =>
      val p1nonCst = p1.nonCstList
      val p2nonCst = p2.nonCstList
      if (p1nonCst.length == p2nonCst.length) {
        val numFactors = p1nonCst.length
        var i = 0
        var result = p1.cstFactor < p2.cstFactor
        while (i < numFactors) {
          if (p1nonCst(i) != p2nonCst(i)) {
            result = isCanonicallySorted(p1nonCst(i),p2nonCst(i))
            i = numFactors-1
          }
          i+=1
        }
        result
      }
      // Shorter products first
      else {
        p1nonCst.length < p2nonCst.length
      }

    case (s1:Sum, s2:Sum) =>
      // Sorting based on non-constant terms
      val s1Terms = s1.terms
      val s2Terms = s2.terms
      if (s1Terms.length == s2Terms.length) {
        val numTerms = s1Terms.length
        var result = false
        var i = 0
        while (i < numTerms) {
          if (s1Terms(i) != s2Terms(i)) {
            result = isCanonicallySorted(s1Terms(i),s2Terms(i))
            i = numTerms-1
          }
          i+=1
        }
        result
      }
      // Shorter sums first
      else {
        s1Terms.length < s2Terms.length
      }
    case (Pow(b1,e1), Pow(b2,e2)) =>
      if (b1 == b2) e1 < e2
      else isCanonicallySorted(b1,b2)
    case (_, _: Pow) => true
    case (_: Pow, _) => false
    case _ => true
  }

  // Evaluates an expression given substitutions for variables
  // So far maps variables to constants
  def evaluate(expr: ArithExpr, subs : scala.collection.Map[Var, Cst]) : Int = expr match {
    case Cst(c) => c
    case v: Var => findSubstitute(v, subs)
    case Sum(terms) => terms.foldLeft(0) { (accumulated, term) => accumulated + evaluate(term, subs)}
    case Prod(factors) => factors.foldLeft(1) { (accumulated, factor) => accumulated * evaluate(factor, subs)}
    case Pow(b,e) => scala.math.pow(evaluate(b,subs),e).toInt
  }

  private def findSubstitute(variable: Var, replacements : scala.collection.Map[Var, Cst]) : Int = {
    for ((varSub, Cst(n)) <- replacements) {
      if (variable == varSub) return n
    }
    throw NotEvaluable
  }

  // Expands product of two expressions into a sum if possible
  def expand(e1: ArithExpr, e2: ArithExpr) : Option[Sum] = (e1,e2) match {
    case (s1: Sum, s2:Sum) =>
      val lts = s1.terms
      val rts = s2.terms
      var combined: ArithExpr = Cst(0)
      for (lt <- lts) {
        for (rt <- rts) {
          combined += lt * rt
        }
      }
      Some(Sum(combined.getSumProdSimplify))

    case (s: Sum, _) =>
      val lts = s.terms
      var combined: ArithExpr = Cst(0)
      for (lt <- lts) {
        combined += lt * e2
      }
      Some(Sum(combined.getSumProdSimplify))


    case (_, s: Sum) =>
      val rts = s.terms
      var combined: ArithExpr = Cst(0)
      for (rt <- rts) {
        combined += e1 * rt
      }
      Some(Sum(combined.getSumProdSimplify))

    case _ => None
  }

  // Check if an expression is a multiple of another expression
  def isMultipleOf(ae1: ArithExpr, ae2: ArithExpr) : Boolean = (ae1, ae2) match {
    // Check multiple of constants
    case (Cst(c1), Cst(c2)) => c1 % c2 == 0
    case (p:Prod, c:Cst) => p.cstFactor % c.value == 0
    case (p:Prod, _) => p.factors.contains(ae2) ||
      p.factors.foldLeft(false){(accum,factor) => accum || isMultipleOf(factor,ae2)}
    case (x, y) => x == y
  }

  // Check if an expression is a smaller than another expression
  def isSmaller(ae1: ArithExpr, ae2: ArithExpr) : Option[Boolean] = {
    try {
      // we check to see if the difference can be evaluated
      val diff = ae2 - ae1
      if (diff.isEvaluable)
        return Some(diff.evalDouble > 0)
    } catch {
      case NotEvaluableException() =>
    }

    try {
      Some(ae1.max.evalDouble < ae2.min.evalDouble)
    } catch {
      case NotEvaluableException() =>
    }

    if (ae1 == ? | ae2 == ?)
      return None
    None
  }

  // Check if an expression is a bigger than another expression
  def isBigger(ae1: ArithExpr, ae2: ArithExpr) : Option[Boolean] = {
    try {
      // we check to see if the difference can be evaluated
      val diff = ae2 - ae1
      if (diff.isEvaluable)
        return Some(diff.evalDouble < 0)
    } catch {
      case NotEvaluableException() =>
    }

    try {
      Some(ae1.min.evalDouble > ae2.max.evalDouble)
    } catch {
      case NotEvaluableException() =>
    }

    if (ae1 == ? | ae2 == ?)
      return None
    None
  }


  // Minimum of expressions (if possible)
  def minList(aes: List[ArithExpr]) : ArithExpr = {
    aes.reduce((ae1,ae2) => {
      val comp = isSmaller(ae2,ae1)
      if (comp.isDefined) {
        if (comp.get) ae2
        else ae1
      }
      else ?
    })
  }

  // Minimum of expressions (if possible)
  def maxList(aes: List[ArithExpr]) : ArithExpr = {
    aes.reduce((ae1,ae2) => {
      val comp = isBigger(ae2,ae1)
      if (comp.isDefined) {
        if (comp.get) ae2
        else ae1
      }
      else ?
    })
  }

  def visitUntil(e: ArithExpr, f: (ArithExpr) => Boolean): Boolean = if (f(e)) true
  else e match {
    case Pow(base, _) =>
      visitUntil(base, f)
    case Sum(terms) =>
      terms.foreach(t => if (visitUntil(t, f)) return true)
      false
    case Prod(terms) =>
      terms.foreach(t => if (visitUntil(t, f)) return true)
      false
    case _:Var | Cst(_) => false
    case x if x.getClass == ?.getClass => false
  }

  private def evalDouble(e: ArithExpr): Double = e match {
    case Cst(c) => c

    case Pow(base, exp) => scala.math.pow(evalDouble(base),exp)

    case Sum(terms) => terms.foldLeft(0.0)((result, expr) => result + evalDouble(expr))
    case Prod(terms) => terms.foldLeft(1.0)((result, expr) => result * evalDouble(expr))

//    case FloorFunction(expr) => scala.math.floor(evalDouble(expr))
//    case CeilingFunction(expr) => scala.math.ceil(evalDouble(expr))

    case `?` => throw NotEvaluable

  }

  private def computeIntervalProd(factors: List[ArithExpr]) : Interval = {
    val minMax = factors.map(x => Interval(x.min, x.max))
    minMax.reduce((x,y) => x*y)
  }
}

case object ? extends ArithExpr {
  override def getSumProdFactorise: List[ArithExpr] = List(?)

  override def getSumProdSimplify: List[ArithExpr] = List(?)
}

