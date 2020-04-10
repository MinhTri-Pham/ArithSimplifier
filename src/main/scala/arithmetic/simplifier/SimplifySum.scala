package arithmetic
package simplifier

import scala.collection.mutable.ListBuffer

object SimplifySum {
  def apply(lhs : ArithExpr, rhs : ArithExpr) : ArithExpr = addExprs(lhs, rhs)

  // Adds two expressions
  def addExprs(lhs : ArithExpr, rhs : ArithExpr) : ArithExpr = {
    var lhsTerms, rhsTerms: List[ArithExpr] = List[ArithExpr]()
    (lhs,rhs) match {

      // Work with sum representation if possible
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

  // Determine result of addition given the terms of expressions to be added
  def mergeTerms(lhsTerms : List[ArithExpr], rhsTerms : List[ArithExpr]) : ArithExpr = {
    var merged = ListBuffer[ArithExpr]()
    merged = merged.addAll(lhsTerms)
    var i = 0
    var simplified = false
    for (rhsTerm <- rhsTerms) {
      var combined = false
      val n = merged.length
      i = 0
      while (i < n) {
        val term = merged(i)
        val newTerm = combineTerms(term, rhsTerm)
        if (newTerm.isDefined) {
//          if (newTerm.get == Cst(0)) merged = Helper.removeAt(i,merged)
//          else merged = Helper.replaceAt(i,newTerm.get,merged)
          merged = Helper.replaceAt(i,newTerm.get,merged)
          combined = true
          simplified = true
          i = n
        }
        i += 1
      }
      if (!combined) merged += rhsTerm
    }
    if (simplified) merged.reduce(_ + _)
    else convert(merged.toList)
  }

  // Tries to combine a pair of terms
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

  // Given list of terms of simplified result, determine resulting expression
  // If result is a sum, sort terms in canonical order
  def convert(terms: List[ArithExpr]): ArithExpr = {
    if (terms.isEmpty) Cst(0) // Eliminated everything, so result is 0
    else if (terms.length == 1) terms.head // Simplifies to a primitive expression
    else Sum(terms.sortWith(ArithExpr.isCanonicallySorted)) // Have a sum, sort terms for subsequent use
  }
}
