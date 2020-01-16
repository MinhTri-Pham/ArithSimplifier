package arithmetic
package simplifier

import scala.collection.mutable.ListBuffer

object SimplifyProd {

  def apply(lhs: ArithExpr, rhs: ArithExpr): ArithExpr = multExprs(lhs, rhs)

  // Multiplies two expressions
  def multExprs(lhs: ArithExpr, rhs: ArithExpr): ArithExpr = {
    var lhsFactors, rhsFactors: List[ArithExpr] = List[ArithExpr]()
    (lhs, rhs) match {
      // If there's a power (with product as base), expand it into a product
      // To do: If either side is a sum, try to factorise and then collect terms as usual
      case (p1: Pow, p2:Pow) =>
        val lhsProd = p1.asProdPows
        val rhsProd = p2.asProdPows
        (lhsProd,rhsProd) match {
          case (Some(_), Some(_)) =>
            lhsFactors = lhsProd.get.factors.sortWith(ArithExpr.isCanonicallySorted)
            rhsFactors = rhsProd.get.factors.sortWith(ArithExpr.isCanonicallySorted)

          case (Some(_), None) =>
            lhsFactors = lhsProd.get.factors.sortWith(ArithExpr.isCanonicallySorted)
            rhsFactors = List[ArithExpr](p2)

          case (None, Some(_)) =>
            lhsFactors = List[ArithExpr](p1)
            rhsFactors = rhsProd.get.factors.sortWith(ArithExpr.isCanonicallySorted)

          case (None, None) =>
            lhsFactors = List[ArithExpr](p1)
            rhsFactors = List[ArithExpr](p2)
        }

      case (p: Pow, s:Sum) =>
        val lhsProd = p.asProdPows
        val rhsProd = s.asProd
        (lhsProd,rhsProd) match {
          case (Some(_), Some(_)) =>
            lhsFactors = lhsProd.get.factors.sortWith(ArithExpr.isCanonicallySorted)
            rhsFactors = rhsProd.get.factors.sortWith(ArithExpr.isCanonicallySorted)

          case (Some(_), None) =>
            lhsFactors = lhsProd.get.factors.sortWith(ArithExpr.isCanonicallySorted)
            rhsFactors = List[ArithExpr](s)

          case (None, Some(_)) =>
            lhsFactors = List[ArithExpr](p)
            rhsFactors = rhsProd.get.factors.sortWith(ArithExpr.isCanonicallySorted)

          case (None, None) =>
            lhsFactors = List[ArithExpr](p)
            rhsFactors = List[ArithExpr](s)
        }

      case (s:Sum, p:Pow) =>
        val lhsProd = s.asProd
        val rhsProd = p.asProdPows
        (lhsProd,rhsProd) match {
          case (Some(_), Some(_)) =>
            lhsFactors = lhsProd.get.factors.sortWith(ArithExpr.isCanonicallySorted)
            rhsFactors = rhsProd.get.factors.sortWith(ArithExpr.isCanonicallySorted)

          case (Some(_), None) =>
            lhsFactors = lhsProd.get.factors.sortWith(ArithExpr.isCanonicallySorted)
            rhsFactors = List[ArithExpr](p)

          case (None, Some(_)) =>
            lhsFactors = List[ArithExpr](s)
            rhsFactors = rhsProd.get.factors.sortWith(ArithExpr.isCanonicallySorted)

          case (None, None) =>
            lhsFactors = List[ArithExpr](s)
            rhsFactors = List[ArithExpr](p)
        }

      case (s1: Sum, s2: Sum) =>
        val lhsProd = s1.asProd
        val rhsProd = s2.asProd
        (lhsProd,rhsProd) match {
          case (Some(_), Some(_)) =>
            lhsFactors = lhsProd.get.factors.sortWith(ArithExpr.isCanonicallySorted)
            rhsFactors = rhsProd.get.factors.sortWith(ArithExpr.isCanonicallySorted)

          case (Some(_), None) =>
            lhsFactors = lhsProd.get.factors.sortWith(ArithExpr.isCanonicallySorted)
            rhsFactors = List[ArithExpr](s2)

          case (None, Some(_)) =>
            lhsFactors = List[ArithExpr](s1)
            rhsFactors = rhsProd.get.factors.sortWith(ArithExpr.isCanonicallySorted)

          case (None, None) =>
            lhsFactors = List[ArithExpr](s1)
            rhsFactors = List[ArithExpr](s2)
        }

      case (p: Pow, _) =>
        val pProd = p.asProdPows
        if (pProd.isDefined) lhsFactors = pProd.get.factors.sortWith(ArithExpr.isCanonicallySorted)
        else lhsFactors = List[ArithExpr](p)
        rhsFactors = rhs.getSumProdSimplify.sortWith(ArithExpr.isCanonicallySorted)

      case (_, p: Pow) =>
        val pProd = p.asProdPows
        if (pProd.isDefined) rhsFactors = pProd.get.factors.sortWith(ArithExpr.isCanonicallySorted)
        else rhsFactors = List[ArithExpr](p)
        lhsFactors = lhs.getSumProdSimplify.sortWith(ArithExpr.isCanonicallySorted)

      case (s: Sum, _) =>
        val sProd = s.asProd
        if (sProd.isDefined) lhsFactors = sProd.get.factors.sortWith(ArithExpr.isCanonicallySorted)
        else lhsFactors = List[ArithExpr](s)
        rhsFactors = rhs.getSumProdSimplify.sortWith(ArithExpr.isCanonicallySorted)

      case (_, s: Sum) =>
        val sProd = s.asProd
        if (sProd.isDefined) rhsFactors = sProd.get.factors.sortWith(ArithExpr.isCanonicallySorted)
        else rhsFactors = List[ArithExpr](s)
        lhsFactors = lhs.getSumProdSimplify.sortWith(ArithExpr.isCanonicallySorted)

      // Neither side is a sum or power, decompose into smaller terms and merge
      case _ =>
        // Extract and sort factors of both sides and merge
        lhsFactors = lhs.getSumProdSimplify.sortWith(ArithExpr.isCanonicallySorted)
        rhsFactors = rhs.getSumProdSimplify.sortWith(ArithExpr.isCanonicallySorted)
        if (lhsFactors.head == ? || rhsFactors.head == ?) return ?
        if (lhsFactors.head == Cst(0) || rhsFactors.head == Cst(0)) return Cst(0)
    }
    mergeFactors(lhsFactors,rhsFactors)
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
    case (Cst(1), _) => Some(rhs)
    case (_, Cst(1)) => Some(lhs)
    case (Cst(0), _) => Some(lhs)
    case (_, Cst(0)) => Some(rhs)

    // Constant cases
    // Compute powers when all bases and exponents are positive constants
    case (Pow(Cst(b1), e1), Pow(Cst(b2), e2)) if e1 > 0 && e2 > 0 =>
      Some(Cst((Math.pow(b1, e1) * Math.pow(b2, e2)).toInt))
    // Compute powers when all bases and exponents are negative constants
    case (Pow(Cst(b1), e1), Pow(Cst(b2), e2)) if e1 < 0 && e2 < 0 =>
      Some(SimplifyPow(Cst((Math.pow(b1, -e1) * Math.pow(b2, -e2)).toInt), -1))

    case (Cst(x), Pow(Cst(y), e2)) if e2 < 0 && x % y == 0 =>
      Some(Cst(x / y) * SimplifyPow(Cst(y), e2 + 1))
    case (Pow(Cst(y), e1), Cst(x)) if e1 < 0 && x % y == 0 =>
      Some(Cst(x / y) * SimplifyPow(Cst(y), e1 + 1))

    case (Cst(x), Pow(Cst(y), -1)) if y % x == 0 && x != -1 =>
      Some(SimplifyPow(Cst(y / x), -1))
    case (Pow(Cst(y), -1), Cst(x)) if y % x == 0  && x != -1 =>
      Some(SimplifyPow(Cst(y / x), -1))

    // Non-constant cases
    case (Pow(b1,e1), Pow(b2,e2)) =>
      if (b1 == b2) Some(b1 pow (e1+e2))
      else None
    case (x, Pow(b,e)) =>
      if (x == b) Some(b pow (e+1))
      else None
    case (Pow(b,e),x) =>
      if (x == b) Some(b pow (e+1))
      else None
    case (x,y) =>
      if (x==y) Some(x pow 2)
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
