import java.util.concurrent.atomic.AtomicLong

import scala.collection.mutable.ListBuffer

abstract sealed class ArithExpr {
  // Addition operator
  def +(that: ArithExpr) : ArithExpr = SimplifySum(this, that)

  // Subtraction operator (x-y = x+(-1)*y)
  def -(that : ArithExpr) : ArithExpr = SimplifySum(this, Cst(-1)*that)

  // Multiply operator
  def *(that: ArithExpr) : ArithExpr = SimplifyProd(this, that)

  // Ordinal division operator (x/y = x*(y pow -1))
  def /^(that: ArithExpr) : ArithExpr = SimplifyProd(this, that pow -1)

  // Exponentiation operator
  def pow(that: Int) : ArithExpr = SimplifyPow(this, that)

  // Returns terms and factors for Sum and Prod resp
  // Returns singleton list with object itself for Cst and Var
  def getTermsFactors : List[ArithExpr]
}

// Class for (int) constants
case class Cst(value : Int) extends ArithExpr {
  override def getTermsFactors: List[ArithExpr] = List[ArithExpr](this)


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
case class Var (name : String, fixedId: Option[Long] = None) extends ArithExpr {

  override def getTermsFactors: List[ArithExpr] = List[ArithExpr](this)

  //var cstMult = 1 // Scalar multiplicity (useful for summing)

  val id: Long = {
    if (fixedId.isDefined)
      fixedId.get
    else {
      Var.incCnt
    }
  }

  // Convert variable to product of multiplicity constant and variable (with mult. 1)
//  lazy val asProd : Prod = Prod(List[ArithExpr](Cst(cstMult), this.copy(1)))

  // Make copy with different multiplicity (
//  def copy(mult : Int): ArithExpr = {
//    if (mult == 0) Cst(0)
//    else {
//      val v = new Var(name,Some(this.id))
//      v.cstMult = mult
//      v
//    }
//  }

  override def equals(that: Any): Boolean = that match {
    case v: Var => this.id == v.id
    case _ => false
  }

//  override def toString : String = {
//    if (cstMult == 1) name
//    else if (cstMult >= 0) s"$cstMult$name"
//    else s"($cstMult)$name"
//  }

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

  def apply(name: String, fixedId: Option[Long]): Var = new Var(name, fixedId)
}

// Class for sums
case class Sum(terms: List[ArithExpr]) extends ArithExpr {

  override def getTermsFactors: List[ArithExpr] = terms

  override def equals(that: Any): Boolean = that match {
    case Sum(terms2) => terms.length == terms2.length && terms.intersect(terms2).length == terms.length
    case _ => false
  }

  override def toString: String = s"(${terms.mkString(" + ")})"
}

// Class for products
case class Prod(factors: List[ArithExpr]) extends ArithExpr {

  override def getTermsFactors: List[ArithExpr] = factors

  val cstFactor : Int = factors.head match {
    case Cst(c) => c
    case _ => 1
  }

  lazy val asSum : Option[Sum] = {
    assume(factors.length > 1)
    var sumEncountered = false
    val dist = factors.foldLeft(Cst(1) : ArithExpr) {(acc,f) => distribute(acc,f) match
      {
      case None => acc * f
      case x : Some[Sum]  =>
        sumEncountered = true
        x.get
      }
    }
    if (sumEncountered) {
      Some(Sum(dist.getTermsFactors))
    }
    else {
      None
    }
  }

  // Distributes product of two expressions into a sum if possible
  def distribute(e1: ArithExpr, e2: ArithExpr) : Option[Sum] = (e1,e2) match {
    case (s1: Sum, s2:Sum) =>
      val lts = s1.terms
      val rts = s2.terms
      var combined: ArithExpr = Cst(0)
      for (lt <- lts) {
        for (rt <- rts) {
          combined += lt * rt
        }
      }
      Some(Sum(combined.getTermsFactors))

    case (s: Sum, _) =>
      val lts = s.terms
      //val rts = s.terms
      var combined: ArithExpr = Cst(0)
      for (lt <- lts) {
        combined += lt * e2
      }
      Some(Sum(combined.getTermsFactors))


    case (_, s: Sum) =>
      //val lts = e1.getTermsFactors
      val rts = s.terms
      var combined: ArithExpr = Cst(0)
      for (rt <- rts) {
        combined += e1 * rt
      }
      Some(Sum(combined.getTermsFactors))

    case _ => None
  }

  def withoutCst : ArithExpr = {
    val nonCstFactors = ListBuffer[ArithExpr]()
    for (factor <- factors) {
      if (!factor.isInstanceOf[Cst]) nonCstFactors += factor
    }
    if (nonCstFactors.length == 1) nonCstFactors.head
    else Prod(nonCstFactors.toList)
  }

  def withoutCstList : List[ArithExpr] = {
    val nonCstFactors = ListBuffer[ArithExpr]()
    for (factor <- factors) {
      if (!factor.isInstanceOf[Cst]) nonCstFactors += factor
    }
    nonCstFactors.toList
  }


  override def equals(that: Any): Boolean = that match {
    case Prod(factors2)=> factors.length == factors2.length && factors.intersect(factors2).length == factors.length
    case _ => false
  }

  override def toString: String = factors.mkString(" * ")
}

// Class for powers, for now just integer exponents
case class Pow(b: ArithExpr, e: Int) extends ArithExpr {
  override def getTermsFactors: List[ArithExpr] = List[ArithExpr](this)

  override def equals(that: Any): Boolean = that match {
    case Pow(b2,e2) => b == b2 && e == e2
    case _ => false
  }

  override def toString: String = s"pow(${b.toString},$e)"
}

object ArithExpr {

  // Used for sorting terms of Sum or factors of Prod
  // For ease of simplification
  val isCanonicallySorted: (ArithExpr, ArithExpr) => Boolean = (x: ArithExpr, y: ArithExpr) => (x, y) match {
    //case (Cst(a), Cst(b)) => a < b
    case (_: Cst, _) => true // constants first
    case (_, _: Cst) => false
    case (x: Var, y: Var) => x.id < y.id // order variables based on id

    // Want na (where n is a constant) < b (assuming a < b) for sum simplification
    case (p : Prod, x: Var) =>
      val nonCst = p.withoutCst
      if (nonCst.isInstanceOf[Var]) isCanonicallySorted(nonCst, x)
      else false
    case (x: Var, p : Prod) =>
      val nonCst = p.withoutCst
      if (nonCst.isInstanceOf[Var]) isCanonicallySorted(x, nonCst)
      else true

    case (_: Var, _) => true
    case (_, _: Var) => false

    case (p1:Prod, p2:Prod) =>
      val p1nonCst = p1.withoutCstList
      val p2nonCst = p2.withoutCstList
      if (p1nonCst.length == p2nonCst.length) {
        p1nonCst.zip(p2nonCst).map(x => isCanonicallySorted(x._1, x._2)).foldLeft(false)(_ || _)
      }
      else {
        p1nonCst.length < p2nonCst.length
      }

    case (Pow(b1,_), Pow(b2,_)) => isCanonicallySorted(b1,b2)
    case (_, _: Pow) => true
    case (_: Pow, _) => false
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
    throw new NotEvaluableException(s"Didn't find a substitution for variable $variable")
  }
}

class NotEvaluableException(msg : String) extends Exception(msg)


