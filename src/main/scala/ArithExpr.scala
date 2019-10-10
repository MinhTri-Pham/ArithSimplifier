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

  // Returns list of terms or factors for Sum and Prod simplification
  def getSumProdList : List[ArithExpr]

}

// Class for (int) constants
case class Cst(value : Int) extends ArithExpr {

  // Prime decomposition
  lazy val asProd : Prod = {
    val decomposition = ArithExpr.primeDecomposition(value)
    Prod(decomposition.map(x => Cst(x)))
  }

  override def getSumProdList: List[ArithExpr] = List[ArithExpr](this)

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

  override def getSumProdList: List[ArithExpr] = List[ArithExpr](this)

  val id: Long = {
    if (fixedId.isDefined)
      fixedId.get
    else {
      Var.incCnt
    }
  }

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

  def apply(name: String, fixedId: Option[Long]): Var = new Var(name, fixedId)
}

// Class for sums
case class Sum(terms: List[ArithExpr]) extends ArithExpr {

  // Returns list of terms converted into products when possible for factorisation
  // The one exception are prime numbers
  lazy val asProds : List[ArithExpr] = {
    var prods = ListBuffer[ArithExpr]()
    for (t <- terms) t match {
      case c:Cst =>
        val cPrimeFactorisation = c.asProd
        if (cPrimeFactorisation.getSumProdList.length == 1) prods += c
        else prods += cPrimeFactorisation
      case v:Var => prods += Prod(v.getSumProdList)
      case p:Prod => prods += p.primitiveProd
      case pow:Pow => prods += pow.asProd.get
    }
    prods.toList
  }

  lazy val asProd : Option[Prod] = FactoriseSum(this)

  override def getSumProdList: List[ArithExpr] = terms

  override def equals(that: Any): Boolean = that match {
    case Sum(terms2) => terms.length == terms2.length && terms.intersect(terms2).length == terms.length
    case _ => false
  }

  override def toString: String = s"(${terms.mkString(" + ")})"
}

// Class for products
case class Prod(factors: List[ArithExpr]) extends ArithExpr {

  override def getSumProdList: List[ArithExpr] = factors

  lazy val cstFactor : Int = factors.head match {
    case Cst(c) => c
    case _ => 1
  }

  lazy val asSum : Option[Sum] = {
    assume(factors.length > 1)

    var expanded = false
    var accum : ArithExpr = Cst(1)
    for (f <- factors) f match {
      case x @ (_:Cst | _:Var) => accum = x * accum
      case s:Sum =>
        accum = ArithExpr.expand(accum,s).get
        expanded = true
      case p:Pow =>
        val pSum = p.asSum
        if (pSum.isDefined) {
          accum = ArithExpr.expand(accum,pSum.get).get
          expanded = true
        }
        else {
          if (p.e > 0) accum = accum * p
          else None
        }
    }
    if (expanded) Some(Sum(accum.getSumProdList))
    else None
  }

  lazy val primitiveProd : Prod = {
    var primitiveFactors = ListBuffer[ArithExpr]()
    for (f <- factors) f match {
      case c:Cst =>
        val cPrimeFactorisation = c.asProd
        if (cPrimeFactorisation.getSumProdList.length == 1) primitiveFactors += c
        else primitiveFactors = primitiveFactors ++ cPrimeFactorisation.factors
      case _ @ (_:Var | _:Sum) => primitiveFactors += f
      case p:Pow =>
        val pProd = p.asProd.get
        primitiveFactors = primitiveFactors ++ pProd.factors
    }
    Prod(primitiveFactors.toList)
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
  override def getSumProdList: List[ArithExpr] = List[ArithExpr](this)

  // Expansion into sum
  lazy val asSum : Option[Sum] = if (e < 0 || !b.isInstanceOf[Sum]) None else {
    var combined : ArithExpr = Cst(0)
    for (n <- 1 to e) {
      if (n == 1)
        combined = b
      else
        combined = ArithExpr.expand(combined,b).get
    }
    Some(Sum(combined.getSumProdList))
  }

  // Representation as a product (a^n = a*a...*a (n times))
  lazy val asProd : Option[Prod] = {
    assume(e > 1)
    val factors = ListBuffer[ArithExpr]()
    for (_ <-1 to e) {
      factors += b
    }
    Some(Prod(factors.toList))
  }

  lazy val asProdPows : Option[Prod]= if (!b.isInstanceOf[Prod]) None else {
    val bfacts = b.getSumProdList
    val pfacts = ListBuffer[ArithExpr]()
    for (bfact <- bfacts) {
      pfacts += bfact pow e
    }
    Some(Prod(pfacts.toList))
  }

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
      if (nonCst.isInstanceOf[Var]) {
        if (nonCst == x) false
        else isCanonicallySorted(nonCst, x)
      }
      else false
    case (x: Var, p : Prod) =>
      val nonCst = p.withoutCst
      if (nonCst.isInstanceOf[Var]) {
        if (nonCst == x) true
        else isCanonicallySorted(x, nonCst)
      }
      else true

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
    case _ => false
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
      Some(Sum(combined.getSumProdList))

    case (s: Sum, _) =>
      val lts = s.terms
      var combined: ArithExpr = Cst(0)
      for (lt <- lts) {
        combined += lt * e2
      }
      Some(Sum(combined.getSumProdList))


    case (_, s: Sum) =>
      val rts = s.terms
      var combined: ArithExpr = Cst(0)
      for (rt <- rts) {
        combined += e1 * rt
      }
      Some(Sum(combined.getSumProdList))

    case _ => None
  }

  // Finds all primes up to (and including) n using sieve algorithm
  private def sieve(n : Int) : List[Int] = {
    val isPrime = Array.fill[Boolean](n+1)(true)
    isPrime(0) = false
    isPrime(1) = false
    for (i <- 2 to n) {
      if (isPrime(i) && i*i <= n) {
        for (j <- i*i to n by i) {
          isPrime(j) = false
        }
      }
    }
    val primes = ListBuffer[Int]()
    for (i <- 2 until n+1) {
      if (isPrime(i)) primes += i
    }
    primes.toList
  }

  // Gives decomposition of an integer as prime
  // If n < 0, include -1 in the decomposition
  // Repeated factors repeated in the product
  def primeDecomposition(n : Int) : List[Int] = {
    var r = scala.math.abs(n)
    val primes = sieve(r)
    val factorisation = ListBuffer[Int]()
    if (n < 0) factorisation += -1
    var i = 0
    var lessSquare = true
    while (i < primes.length && lessSquare) {
      val p = primes(i)
      if (p*p > r) lessSquare = false
      else {
        while(r % p == 0) {
          factorisation += p
          r/= p
        }
      }
      i+=1
    }
    if (r > 1) factorisation += r
    factorisation.toList
  }
}

class NotEvaluableException(msg : String) extends Exception(msg)


