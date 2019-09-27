import scala.collection.mutable.ListBuffer

object SimplifyProd {

  def apply(lhs: ArithExpr, rhs: ArithExpr): ArithExpr = multExprs(lhs,rhs)

  // Multiplies two expressions
  def multExprs(lhs: ArithExpr, rhs: ArithExpr) : ArithExpr = (lhs, rhs) match {
    // If either side is a sum, introduce that as a standalone term
    // Later want to use factorisation
    case (s1:Sum, s2:Sum) =>
      val lhsTerms = List[ArithExpr](s1)
      val rhsTerms = List[ArithExpr](s2)
      mergeFactors(lhsTerms, rhsTerms)

    case (s:Sum, _) =>
      val lhsTerms = List[ArithExpr](s)
      val rhsTerms = rhs.getTermsFactors.sortWith(ArithExpr.isCanonicallySorted)
      mergeFactors(lhsTerms, rhsTerms)

    case (_, s:Sum) =>
      val lhsTerms = lhs.getTermsFactors.sortWith(ArithExpr.isCanonicallySorted)
      val rhsTerms = List[ArithExpr](s)
      mergeFactors(lhsTerms, rhsTerms)

    // Neither side is a sum, decompose into smaller terms and merge
    case _ =>
      // Extract and canonically sort factors of both sides and merge
      val lhsFactors = lhs.getTermsFactors.sortWith(ArithExpr.isCanonicallySorted)
      val rhsFactors = rhs.getTermsFactors.sortWith(ArithExpr.isCanonicallySorted)
      mergeFactors(lhsFactors, rhsFactors)
  }

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
   // Trivial cases
    case (Cst(x), Cst(y)) => Some(Cst(x * y))
    case (Cst(0), _) => Some(Cst(0))
    case (_, Cst(0)) => Some(Cst(0))
    case (Cst(1), _) => Some(rhs)
    case (_, Cst(1)) => Some(lhs)
//    case (Cst(c), v: Var) => Some(v.copy(c*v.cstMult))
//    case (v:Var,Cst(c)) => Some(v.copy(c*v.cstMult))

    // Compute powers when all bases and exponents are positive constants
    case (Pow(Cst(b1), e1), Pow(Cst(b2), e2)) if e1 > 0 && e2 > 0 =>
      Some(Cst((Math.pow(b1, e1) * Math.pow(b2, e2)).toInt))

    // More general cases
    case (Pow(b1,e1), Pow(b2,e2)) =>
      if (b1 == b2) Some(Pow(b1, e1+e2))
      else if (e1 == e2) Some(Pow(b1 * b2,e1))
      else None
    case (x, Pow(b,e)) =>
      if (x == b) Some(Pow(b, e+1))
      else None
    case (Pow(b,e),x) =>
      if (x == b) Some(Pow(b, e+1))
      else None
    case (x,y) =>
      if (x==y) Some(Pow(x,2))
      else None
    case _ => None
  }

  // Given list of factors, determine resulting expression
  // Remove unnecessary one terms that could arise from combining terms
  // I.e. Prod(1,Var(a),Var(b)) -> Prod(Var(a),Var(b)) but Prod(1, Var(a)) -> Var(a)
  def convert(factors: List[ArithExpr]): ArithExpr = {
    val nonOne = factors.filter(_ != Cst(1))
    if (nonOne.isEmpty) Cst(1) // Eliminated everything, so result is 1
    else if (nonOne.length == 1) nonOne.head // Result is a Var or Cst
    else Prod(nonOne) // Have a product of expressions
  }
}
