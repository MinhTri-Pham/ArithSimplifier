import java.util.concurrent.atomic.AtomicLong

abstract sealed class ArithExpr {
  // Addition operator
  def +(that: ArithExpr) : ArithExpr = SimplifySum(this, that)

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
// Includes a scalar multiple
case class Var (cstMult : Int, name : String, val fixedId: Option[Long] = None) extends ArithExpr {

  override def getTermsFactors: List[ArithExpr] = List[ArithExpr](this)

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

  override def toString : String = {
    if (cstMult == 1) name
    else if (cstMult >= 0) s"$cstMult$name"
    else s"($cstMult)$name"
  }
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

  // If the constant multiple is 0, convert to Cst(0) immediately
  def apply(cstMult: Int, name: String): ArithExpr = cstMult match {
    case 0 => Cst(0)
    case _ => new Var(cstMult, name)
  }

  // If the constant multiple is 0, convert to Cst(0) immediately
  def apply(cstMult: Int, name: String, fixedId: Option[Long]): ArithExpr = cstMult match {
    case 0 => Cst(0)
    case _ => new Var(cstMult, name, fixedId)
  }

}

// Class for Sums
case class Sum(terms: List[ArithExpr]) extends ArithExpr {

  override def getTermsFactors: List[ArithExpr] = terms

  override def toString: String = s"(${terms.mkString(" + ")})"
}

case class Prod(factors: List[ArithExpr]) extends ArithExpr {
  override def getTermsFactors: List[ArithExpr] = factors
  override def toString: String = factors.mkString(" * ")
}


object ArithExpr {

  // Used for sorting terms of Sum or factors of Prod
  // For ease of simplification
  val isCanonicallySorted: (ArithExpr, ArithExpr) => Boolean = (x: ArithExpr, y: ArithExpr) => (x, y) match {
    case (Cst(a), Cst(b)) => a < b
    case (_: Cst, _) => true // constants first
    case (_, _: Cst) => false
    case (x: Var, y: Var) => x.id < y.id // order variables based on id
    case (_: Var, _) => true // variables always after constants second
    case (_, _: Var) => false
  }

  // Evaluates an expression given substitutions for variables
  // So far maps variables to constants
  // Assumes constant maps to multiple of one of variable
  def evaluate(expr: ArithExpr, subs : scala.collection.Map[Var, Cst]) : Int = expr match {
    case Cst(c) => c
    case v: Var => v.cstMult * findSubstitute(v, subs)
    case Sum(terms) => terms.foldLeft(0) { (accumulated, term) => accumulated + evaluate(term, subs)}
    case Prod(terms) => terms.foldLeft(1) { (accumulated, term) => accumulated + evaluate(term, subs)}
  }

  private def findSubstitute(variable: Var, replacements : scala.collection.Map[Var, Cst]) : Int = {
    for ((varSub, Cst(n)) <- replacements) {
      if (variable == varSub) return n
    }
    throw new NotEvaluableException(s"Didn't find a substitution for variable $variable")
  }

}

class NotEvaluableException(msg : String) extends Exception(msg)


