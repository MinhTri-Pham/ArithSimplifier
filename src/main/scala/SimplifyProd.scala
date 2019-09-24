import scala.collection.mutable.ListBuffer

object SimplifyProd {

  def apply(lhs: ArithExpr, rhs: ArithExpr): ArithExpr = multExprs(lhs,rhs)

  // Multiplies two expressions
  def multExprs(lhs: ArithExpr, rhs: ArithExpr) : ArithExpr = {
    // Extract and canonically sort factors of both sides and merge
    val lhsFactors = lhs.getTermsFactors
    val rhsFactors = rhs.getTermsFactors
    mergeFactors(lhsFactors, rhsFactors)
  }

  //  def varCstFold(factors : List[ArithExpr]) : List[ArithExpr] = {
  //    val sorted = factors.sortWith(ArithExpr.isCanonicallySorted)
  //    val folded = sorted.reduce()
  //  }

  // Merges factors of expressions to be multiplied
  // Assumes both factor lists are canonically sorted
  def mergeFactors(lhsFactors:List[ArithExpr], rhsFactors:List[ArithExpr]) : ArithExpr = {
    val merged = ListBuffer[ArithExpr]()
    val lhsSize = lhsFactors.length
    val rhsSize = rhsFactors.length
    var i,j = 0
    while (i < lhsSize && j < rhsSize) {
      val lhsTerm = lhsFactors(i)
      val rhsTerm = rhsFactors(j)
      val combinedTerm = combineFactors(lhsTerm, rhsTerm)
      if (combinedTerm.isDefined) {
        merged += combinedTerm.get
        i+=1
        j+=1
      }
      else {
        if (ArithExpr.isCanonicallySorted(lhsTerm, rhsTerm)) {
          merged += lhsTerm
          i+=1
        }
        else {
          merged += rhsTerm
          j+=1
        }
      }
    }
    while (i < lhsSize) {
      merged += lhsFactors(i)
      i+=1
    }
    while (j < rhsSize) {
      merged += rhsFactors(j)
      j+=1
    }
    convert(merged.toList)
  }

  // Tries to combine a pair of factors
  def combineFactors(lhs: ArithExpr, rhs: ArithExpr) : Option[ArithExpr] = (lhs, rhs) match {
    case (Cst(x), Cst(y)) => Some(Cst(x * y))
    case (Cst(0), _) => Some(Cst(0))
    case (_, Cst(0)) => Some(Cst(0))
    case (Cst(1), _) => Some(rhs)
    case (_, Cst(1)) => Some(lhs)
    case (Cst(c), v: Var) => Some(v.copy(c))
    case (v:Var,Cst(c)) => Some(v.copy(c))
    case _ => None
  }

  // Given list of factors, determine resulting expression
  // Remove unnecessary one terms that could arise from combining terms
  // I.e. Prod(Cst(1),Var(1,a),Var(2,b)) -> Prod(Var(1,a),Var(2,b)) but Prod(Cst(1),Var(2,a)) -> Var(2,a)
  def convert(factors: List[ArithExpr]): ArithExpr = {
    val nonOne = factors.filter(_ != Cst(1))
    if (nonOne.isEmpty) Cst(1) // Eliminated everything, so result is 1
    else if (nonOne.length == 1) nonOne.head // Result is a Var or Cst
    else Prod(nonOne) // Have a sum of Csts and Vars
  }
}
