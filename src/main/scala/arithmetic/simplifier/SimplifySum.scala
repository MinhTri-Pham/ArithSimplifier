package arithmetic
package simplifier

import scala.collection.mutable.ListBuffer

object SimplifySum {
  def apply(lhs : ArithExpr, rhs : ArithExpr) : ArithExpr = addExprs(lhs, rhs)

  // Adds two expressions
  def addExprs(lhs : ArithExpr, rhs : ArithExpr) : ArithExpr = {
    var lhsTerms, rhsTerms: List[ArithExpr] = List[ArithExpr]()
    (lhs,rhs) match {

      // If either side is a product/power, try to convert to a sum and work with sums instead
      case (p1:Prod,p2: Prod) =>
        val lhsSum = p1.asExpandedSum
        val rhsSum = p2.asExpandedSum
        (lhsSum, rhsSum) match {
          case (Some(_), Some(_)) =>
            lhsTerms = lhsSum.get.terms.sortWith(ArithExpr.isCanonicallySorted)
            rhsTerms = rhsSum.get.terms.sortWith(ArithExpr.isCanonicallySorted)

          case (Some(_), None) =>
            lhsTerms = lhsSum.get.terms.sortWith(ArithExpr.isCanonicallySorted)
            rhsTerms = List[ArithExpr](p2)

          case (None, Some(_)) =>
            lhsTerms = List[ArithExpr](p1)
            rhsTerms = rhsSum.get.terms.sortWith(ArithExpr.isCanonicallySorted)

          case (None, None) =>
            lhsTerms = List[ArithExpr](p1)
            rhsTerms = List[ArithExpr](p2)
        }

      case (p1:Prod, p2:Pow) =>
        val lhsSum = p1.asExpandedSum
        val rhsSum = p2.asSum
        (lhsSum, rhsSum) match {
          case (Some(_), Some(_)) =>
            lhsTerms = lhsSum.get.terms.sortWith(ArithExpr.isCanonicallySorted)
            rhsTerms = rhsSum.get.terms.sortWith(ArithExpr.isCanonicallySorted)

          case (Some(_), None) =>
            lhsTerms = lhsSum.get.terms.sortWith(ArithExpr.isCanonicallySorted)
            rhsTerms = List[ArithExpr](p2)

          case (None, Some(_)) =>
            lhsTerms = List[ArithExpr](p1)
            rhsTerms = rhsSum.get.terms.sortWith(ArithExpr.isCanonicallySorted)

          case (None, None) =>
            lhsTerms = List[ArithExpr](p1)
            rhsTerms = List[ArithExpr](p2)
        }

      case (p1:Pow,p2:Prod) =>
        val lhsSum = p1.asSum
        val rhsSum = p2.asExpandedSum
        (lhsSum, rhsSum) match {
          case (Some(_), Some(_)) =>
            lhsTerms = lhsSum.get.terms.sortWith(ArithExpr.isCanonicallySorted)
            rhsTerms = rhsSum.get.terms.sortWith(ArithExpr.isCanonicallySorted)

          case (Some(_), None) =>
            lhsTerms = lhsSum.get.terms.sortWith(ArithExpr.isCanonicallySorted)
            rhsTerms = List[ArithExpr](p2)

          case (None, Some(_)) =>
            lhsTerms = List[ArithExpr](p1)
            rhsTerms = rhsSum.get.terms.sortWith(ArithExpr.isCanonicallySorted)

          case (None, None) =>
            lhsTerms = List[ArithExpr](p1)
            rhsTerms = List[ArithExpr](p2)
        }

      case (p1:Pow,p2:Pow) =>
        val lhsSum = p1.asSum
        val rhsSum = p2.asSum
        (lhsSum, rhsSum) match {
          case (Some(_), Some(_)) =>
            lhsTerms = lhsSum.get.terms.sortWith(ArithExpr.isCanonicallySorted)
            rhsTerms = rhsSum.get.terms.sortWith(ArithExpr.isCanonicallySorted)

          case (Some(_), None) =>
            lhsTerms = lhsSum.get.terms.sortWith(ArithExpr.isCanonicallySorted)
            rhsTerms = List[ArithExpr](p2)

          case (None, Some(_)) =>
            lhsTerms = List[ArithExpr](p1)
            rhsTerms = rhsSum.get.terms.sortWith(ArithExpr.isCanonicallySorted)

          case (None, None) =>
            lhsTerms = List[ArithExpr](p1)
            rhsTerms = List[ArithExpr](p2)
        }

      case (p: Prod, _) =>
        val pSum = p.asExpandedSum
        if (pSum.isDefined) lhsTerms = pSum.get.terms.sortWith(ArithExpr.isCanonicallySorted)
        else lhsTerms = List[ArithExpr](p)
        rhsTerms = rhs.getSumProdSimplify.sortWith(ArithExpr.isCanonicallySorted)

      case (p: Pow, _) =>
        val pSum = p.asSum
        if (pSum.isDefined) lhsTerms = pSum.get.terms.sortWith(ArithExpr.isCanonicallySorted)
        else lhsTerms = List[ArithExpr](p)
        rhsTerms = rhs.getSumProdSimplify.sortWith(ArithExpr.isCanonicallySorted)

      case (_, p: Prod) =>
        val pSum = p.asExpandedSum
        if (pSum.isDefined) rhsTerms = pSum.get.terms.sortWith(ArithExpr.isCanonicallySorted)
        else rhsTerms = List[ArithExpr](p)
        lhsTerms = lhs.getSumProdSimplify.sortWith(ArithExpr.isCanonicallySorted)

      case (_, p: Pow) =>
        val pSum = p.asSum
        if (pSum.isDefined) rhsTerms = pSum.get.terms.sortWith(ArithExpr.isCanonicallySorted)
        else rhsTerms = List[ArithExpr](p)
        lhsTerms = lhs.getSumProdSimplify.sortWith(ArithExpr.isCanonicallySorted)

      // Neither side is a product/power, decompose into smaller terms and merge
      case _ =>
        // Extract and sort terms of both sides and merge
        lhsTerms = lhs.getSumProdSimplify.sortWith(ArithExpr.isCanonicallySorted)
        rhsTerms = rhs.getSumProdSimplify.sortWith(ArithExpr.isCanonicallySorted)
        if (lhsTerms.head == ? || rhsTerms.head == ?) return ?

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
    // Modulo Identity: a = a / b * b + a % b
    case (Prod(factors), Mod(a, b)) if factors.reduce(_*_) == (a / b) * b => Some(a)
    case (Mod(a, b), Prod(factors)) if factors.reduce(_*_) == (a / b) * b => Some(a)
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
    else Sum(nonZero) // Have a sum of expressions
  }
}
