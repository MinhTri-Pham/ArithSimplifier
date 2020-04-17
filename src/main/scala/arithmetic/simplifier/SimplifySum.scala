package arithmetic
package simplifier

import scala.collection.mutable.ListBuffer

object SimplifySum {
  def apply(lhs : ArithExpr, rhs : ArithExpr) : ArithExpr = addExprs(lhs, rhs)

  /**
   * Adds together two expression
   *
   * @param lhs   Left-hand-side expression
   * @param rhs   Right-hand-side expression
   * @return      Result of adding the two expressions
   */

  def addExprs(lhs : ArithExpr, rhs : ArithExpr) : ArithExpr = {
    var lhsTerms, rhsTerms: List[ArithExpr] = List[ArithExpr]()
    (lhs,rhs) match {

      // Work with sum representation of both sides if possible
      case (Sum(lhsTs), Sum(rhsTs)) =>
        lhsTerms = lhsTs
        rhsTerms = rhsTs
      case (Sum(lhsTs), _) =>
        lhsTerms = lhsTs
        rhsTerms = List[ArithExpr](rhs)
      case (_, Sum(rhsTs)) =>
        lhsTerms = List[ArithExpr](lhs)
        rhsTerms = rhsTs
      case _ =>
        lhsTerms = List[ArithExpr](lhs)
        rhsTerms = List[ArithExpr](rhs)
    }
    if (lhsTerms.contains(?) || rhsTerms.contains(?)) return ?
    mergeTerms(lhsTerms, rhsTerms)
  }

  /**
   * Merges together term lists of the two expressions to be added
   *
   * @param lhsTerms   Term list of left-hand-side expression
   * @param rhsTerms   Term list of right-hand-side expression
   * @return           Result of adding the two corresponding expressions
   */

  def mergeTerms(lhsTerms : List[ArithExpr], rhsTerms : List[ArithExpr]) : ArithExpr = {
    // The terms of the result of addition, initialise with lhs terms
    var merged = ListBuffer[ArithExpr]()
    merged = merged.addAll(lhsTerms)
    var i = 0
    var simplified = false
    // Try to combine rhs terms with a lhs term, one by one
    for (rhsTerm <- rhsTerms) {
      var combined = false
      val n = merged.length
      i = 0
      while (i < n) {
        val term = merged(i)
        val newTerm = combineTerms(term, rhsTerm)
        // Successfully combined a rhs term with a lhs term
        if (newTerm.isDefined) {
          merged = Helper.replaceAt(i,newTerm.get,merged)
          combined = true
          simplified = true
          i = n
        }
        i += 1
      }
      // Didn't manage to combine a rhs term, so just add it to result term list
      if (!combined) merged += rhsTerm
    }
    // Recursively add new terms together in case more simplification can be done on them
    if (simplified) merged.reduce(_ + _)
    else {
      if (merged.isEmpty) Cst(0) // The terms cancel each other out completely, result is 0
      else if (merged.size == 1) merged.head // Only one term left after simplifying, this is the result
      else Sum(merged.toList.sortWith(ArithExpr.isCanonicallySorted))
    }
  }

  /**
   * Try to combine a pair of terms.
   *
   * @param lhs The first term
   * @param rhs The second term
   * @return An option containing an expression if the terms can be combined, None otherwise
   */
  def combineTerms(lhs: ArithExpr, rhs: ArithExpr) : Option[ArithExpr] = (lhs, rhs) match {

    // Special values
    case (arithmetic.?,_) | (_,arithmetic.?) => Some(?)
    case (PosInf, NegInf) | (NegInf, PosInf) => Some(?)
    case (PosInf, _) | (_, PosInf) => Some(PosInf)
    case (NegInf, _) | (_, NegInf) => Some(NegInf)

    // Trivial rules
    case (Cst(x), Cst(y)) => Some(Cst(x + y))
    case (Cst(0), _) => Some(rhs)
    case (_, Cst(0)) => Some(lhs)
    case (x, y) if x == y => Some(Cst(2) * x)

    // Products - combine terms with different constant factor
    case (p1:Prod, p2:Prod) if p1.nonCstFactor == p2.nonCstFactor =>
      Some(Cst(p1.cstFactor + p2.cstFactor) * p1.nonCstFactor)
    case (p:Prod,x) if p.nonCstFactor == x => Some(Cst(1 + p.cstFactor) * x)
    case (x, p:Prod) if p.nonCstFactor == x => Some(Cst(1 + p.cstFactor) * x)
    case _ => None
  }
}
