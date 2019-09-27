import java.util.concurrent.atomic.AtomicLong

import scala.collection.mutable.ListBuffer

abstract sealed class ArithExpr {
  // Addition operator
  def +(that: ArithExpr) : ArithExpr = SimplifySum(this, that)

  // Subtraction operator
  def -(that : ArithExpr) : ArithExpr = SimplifySum(this, Cst(-1)*that)

  // Multiply operator
  def *(that: ArithExpr) : ArithExpr = SimplifyProd(this, that)

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

  var cstMult = 1 // Scalar multiplicity (useful for summing)

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
  def copy(mult : Int): ArithExpr = {
    if (mult == 0) Cst(0)
    else {
      val v = new Var(name,Some(this.id))
      v.cstMult = mult
      v
    }
  }

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

  override def toString: String = s"(${terms.mkString(" + ")})"
}

// Class for products
case class Prod(factors: List[ArithExpr]) extends ArithExpr {

  override def getTermsFactors: List[ArithExpr] = factors

  val cstFactor : Int = factors.head match {
    case Cst(c) => c
    case _ => 1
  }

  def withoutCst : ArithExpr = {
    val nonCstFactors = ListBuffer[ArithExpr]()
    for (factor <- factors) {
      if (!factor.isInstanceOf[Cst]) nonCstFactors += factor
    }
    if (nonCstFactors.length == 1) nonCstFactors.head
    else Prod(nonCstFactors.toList)
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

  override def toString: String = s"pow(${b.toString},$e)"
}

object ArithExpr {

  // Used for sorting terms of Sum or factors of Prod
  // For ease of simplification
  val isCanonicallySorted: (ArithExpr, ArithExpr) => Boolean = (x: ArithExpr, y: ArithExpr) => (x, y) match {
    case (Cst(a), Cst(b)) => a < b
    case (_: Cst, _) => true // constants first
    case (_, _: Cst) => false
    case (x: Var, y: Var) => x.id < y.id // order variables based on id

    // Want 2a before b (assuming a.id < b.id) to achieve more efficient sum simplification
    case (p : Prod, x: Var) =>
      val nonCst = p.withoutCst
      if (nonCst.isInstanceOf[Var]) isCanonicallySorted(nonCst, x)
      else false
    case (x: Var, p : Prod) =>
      val nonCst = p.withoutCst
      if (nonCst.isInstanceOf[Var]) isCanonicallySorted(x, nonCst)
      else false

    case (_: Var, _) => true
    case (_, _: Var) => false

    case (Prod(factors1), Prod(factors2)) => factors1.zip(factors2).map(x => isCanonicallySorted(x._1, x._2)).foldLeft(false)(_ || _)
    case (Pow(e1,_), Pow(e2,_)) => isCanonicallySorted(e1,e2)
    //case _ => false
  }

  // Evaluates an expression given substitutions for variables
  // So far maps variables to constants
  // Assumes constant maps to multiple of one of variable
  def evaluate(expr: ArithExpr, subs : scala.collection.Map[Var, Cst]) : Int = expr match {
    case Cst(c) => c
    case v: Var => v.cstMult * findSubstitute(v, subs)
    case Sum(terms) => terms.foldLeft(0) { (accumulated, term) => accumulated + evaluate(term, subs)}
    case Prod(terms) => terms.foldLeft(1) { (accumulated, term) => accumulated * evaluate(term, subs)}
  }

  private def findSubstitute(variable: Var, replacements : scala.collection.Map[Var, Cst]) : Int = {
    for ((varSub, Cst(n)) <- replacements) {
      if (variable == varSub) return n
    }
    throw new NotEvaluableException(s"Didn't find a substitution for variable $variable")
  }
}

class NotEvaluableException(msg : String) extends Exception(msg)


