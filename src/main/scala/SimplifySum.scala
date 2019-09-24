import scala.collection.mutable.ListBuffer

object SimplifySum {
  def apply(lhs : ArithExpr, rhs : ArithExpr) : ArithExpr = addExprs(lhs, rhs)

  // Adds two expressions
  // To Do: Cases which involve Prod as terms
  def addExprs(lhs : ArithExpr, rhs : ArithExpr) : ArithExpr = {
    // Extract and canonically sort terms of both sides and merge
    val lhsTerms = lhs.getTermsFactors.sortWith(ArithExpr.isCanonicallySorted)
    val rhsTerms = rhs.getTermsFactors.sortWith(ArithExpr.isCanonicallySorted)
    mergeTerms(lhsTerms, rhsTerms)
  }

  // Merges terms of expressions to be added
  // Assumes both term lists are canonically sorted
  def mergeTerms(lhsTerms : List[ArithExpr], rhsTerms : List[ArithExpr]) : ArithExpr = {
    val merged = ListBuffer[ArithExpr]()
    val lhsSize = lhsTerms.length
    val rhsSize = rhsTerms.length
    var i,j = 0
    while (i < lhsSize && j < rhsSize) {
      val lhsTerm = lhsTerms(i)
      val rhsTerm = rhsTerms(j)
      val combinedTerm = combineTerms(lhsTerm, rhsTerm)
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
      merged += lhsTerms(i)
      i+=1
    }
    while (j < rhsSize) {
      merged += rhsTerms(j)
      j+=1
    }
    // Make adjustment for possible Cst(0) terms in the result
    convert(merged.toList)
  }

  // Tries to combine a pair of terms
  def combineTerms(lhs: ArithExpr, rhs: ArithExpr) : Option[ArithExpr] = (lhs, rhs) match {
    case (Cst(x), Cst(y)) => Some(Cst(x + y))
    case (Cst(0), _) => Some(rhs)
    case (_, Cst(0)) => Some(lhs)
    case (x:Var, y:Var) =>
      if (x == y)  Some(x.copy(x.cstMult + y.cstMult))
      else None
    case _ => None
  }

  // Given list of terms, determine resulting expression
  // Remove unnecessary zero terms that could arise from combining terms
  // I.e. Sum(0,Var(1,a),Var(2,b)) -> Sum(Var(1,a),Var(2,b)) but Sum(0,Var(2,a)) -> Var(2,a)
  def convert(terms: List[ArithExpr]): ArithExpr = {
    val nonZero = terms.filter(_ != Cst(0))
    if (nonZero.isEmpty) Cst(0) // Eliminated everything, so result is 0
    else if (nonZero.length == 1) nonZero.head // Result is a Var or Cst
    else Sum(nonZero) // Have a sum of Csts and Vars
  }
}
