import scala.collection.mutable.ListBuffer

object SimplifySum {
  def apply(lhs : ArithExpr, rhs : ArithExpr) : ArithExpr = addExprs(lhs, rhs)

  // Adds two expressions
  def addExprs(lhs : ArithExpr, rhs : ArithExpr) : ArithExpr = (lhs, rhs) match {
    // If either side is a product, try to convert to a sum and work with sums instead
    case (p1:Prod, p2:Prod) =>
      val lhsSum = p1.asSum
      val rhsSum = p2.asSum
      (lhsSum, rhsSum ) match
      {
        case (Some(_), Some(_)) =>
          val lhsTerms = lhsSum.get.getTermsFactors.sortWith(ArithExpr.isCanonicallySorted)
          val rhsTerms = rhsSum.get.getTermsFactors.sortWith(ArithExpr.isCanonicallySorted)
          mergeTerms(lhsTerms, rhsTerms)

        case (Some(_), None)  =>
          val lhsTerms = lhsSum.get.getTermsFactors.sortWith(ArithExpr.isCanonicallySorted)
          val rhsTerms = List[ArithExpr](p2)
          mergeTerms(lhsTerms, rhsTerms)

        case (None, Some(_))  =>
          val lhsTerms = List[ArithExpr](p1)
          val rhsTerms = rhsSum.get.getTermsFactors.sortWith(ArithExpr.isCanonicallySorted)
          mergeTerms(lhsTerms, rhsTerms)

        case (None, None)  =>
          val lhsTerms = List[ArithExpr](p1)
          val rhsTerms = List[ArithExpr](p2)
          mergeTerms(lhsTerms, rhsTerms)
      }

    case (p:Prod, _) =>
      val pSum = p.asSum
      if (pSum.isDefined) {
        val lhsTerms = pSum.get.getTermsFactors.sortWith(ArithExpr.isCanonicallySorted)
        val rhsTerms = rhs.getTermsFactors.sortWith(ArithExpr.isCanonicallySorted)
        mergeTerms(lhsTerms, rhsTerms)
      }
      else {
        val lhsTerms = List[ArithExpr](p)
        val rhsTerms = rhs.getTermsFactors.sortWith(ArithExpr.isCanonicallySorted)
        mergeTerms(lhsTerms, rhsTerms)
      }

    case (_, p:Prod) =>
      val pSum = p.asSum
      if (pSum.isDefined) {
        val lhsTerms = lhs.getTermsFactors.sortWith(ArithExpr.isCanonicallySorted)
        val rhsTerms = pSum.get.getTermsFactors.sortWith(ArithExpr.isCanonicallySorted)
        mergeTerms(lhsTerms, rhsTerms)
      }
      else {
        val lhsTerms = lhs.getTermsFactors.sortWith(ArithExpr.isCanonicallySorted)
        val rhsTerms = List[ArithExpr](p)
        mergeTerms(lhsTerms, rhsTerms)
      }

    // Neither side is a product, decompose into smaller terms and merge
    case _ =>
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
//    case (x:Var, y:Var) =>
//      if (x == y)  Some(x.copy(x.cstMult + y.cstMult))
//      else None
    case (x, y) if x == y => Some(Cst(2) * x)
    case (p1:Prod, p2:Prod) if p1.withoutCst == p2.withoutCst =>
      Some(Cst(p1.cstFactor + p2.cstFactor) * p1.withoutCst)
    case (p:Prod,x) if p.withoutCst == x => Some(Cst(1 + p.cstFactor) * x)
    case (x, p:Prod) if p.withoutCst == x => Some(Cst(1 + p.cstFactor) * x)
    case _ => None
  }

  // Given list of terms, determine resulting expression
  // Remove unnecessary zero terms that could arise from combining terms
  // I.e. Sum(0,Var(a),Var(b)) -> Sum(Var(a),Var(b)) but Sum(0,Var(a)) -> Var(a)
  def convert(terms: List[ArithExpr]): ArithExpr = {
    val nonZero = terms.filter(_ != Cst(0))
    if (nonZero.isEmpty) Cst(0) // Eliminated everything, so result is 0
    else if (nonZero.length == 1) nonZero.head // Result is a Var or Cst
    else Sum(nonZero) // Have a sum of expressions
  }
}
