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
        lhsTerms = lhsTs.sortWith(ArithExpr.isCanonicallySorted)
        rhsTerms = rhsTs.sortWith(ArithExpr.isCanonicallySorted)
      case (Sum(lhsTs), _) =>
        lhsTerms = lhsTs.sortWith(ArithExpr.isCanonicallySorted)
        rhsTerms = List[ArithExpr](rhs)
      case (_, Sum(rhsTs)) =>
        lhsTerms = List[ArithExpr](lhs)
        rhsTerms = rhsTs.sortWith(ArithExpr.isCanonicallySorted)
      case _ =>
        lhsTerms = List[ArithExpr](lhs)
        rhsTerms = List[ArithExpr](rhs)
    }
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
    case (x, y) if x == y => Some(Cst(2) * x)

    // Products
    case (p1:Prod, p2:Prod) if p1.nonCstFactor == p2.nonCstFactor =>
      Some(Cst(p1.cstFactor + p2.cstFactor) * p1.nonCstFactor)
    case (p:Prod,x) if p.nonCstFactor == x => Some(Cst(1 + p.cstFactor) * x)
    case (x, p:Prod) if p.nonCstFactor == x => Some(Cst(1 + p.cstFactor) * x)
    case _ => None
  }

  // Given list of terms, determine resulting expression
  // Remove unnecessary zero terms that could arise from combining terms
  // I.e. Sum(0,a,b) -> Sum(a,b) but Sum(0) -> 0
  def convert(terms: List[ArithExpr]): ArithExpr = {
    val nonZero = terms.filter(_ != Cst(0))
    if (nonZero.isEmpty) Cst(0) // Eliminated everything, so result is 0
    else if (nonZero.length == 1) nonZero.head // Simplifies to a primitive expression
    else Sum(nonZero)
  }
}
