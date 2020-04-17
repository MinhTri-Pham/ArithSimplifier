package arithmetic
package simplifier

import scala.collection.mutable.ListBuffer

object SimplifyProd {

  def apply(lhs: ArithExpr, rhs: ArithExpr): ArithExpr = multExprs(lhs, rhs)

  /**
   * Multiples together two expression
   *
   * @param lhs   Left-hand-side expression
   * @param rhs   Right-hand-side expression
   * @return      Result of multiplying the two expressions
   */
  def multExprs(lhs: ArithExpr, rhs: ArithExpr): ArithExpr = {
    // Special cases
    if (lhs == Cst(0) || rhs == Cst(0)) return Cst(0)
    if (lhs == ? || rhs == ?) return ?
    var lhsFactors, rhsFactors: List[ArithExpr] = List[ArithExpr]()

    // Work with product or power representation of both sides if possible
    (lhs, rhs) match {
      case (Prod(lhsFs), Prod(rhsFs)) =>
        lhsFactors = lhsFs
        rhsFactors = rhsFs
      case (Prod(lhsFs), _) =>
        lhsFactors = lhsFs
        // Try power representation of rhs (since product one doesn't exist)
        if (rhs.isInstanceOf[Sum]) {
          val rhsSum = rhs.toSum.get
          if (rhsSum.asPow.isDefined) rhsFactors = List[ArithExpr](rhsSum.asPow.get)
          else rhsFactors = List[ArithExpr](rhs)
        }
        else rhsFactors = List[ArithExpr](rhs)
      case (_, Prod(rhsFs)) =>
        // Try power representation of ;hs (since product one doesn't exist)
        if (lhs.isInstanceOf[Sum]) {
          val lhsSum = lhs.toSum.get
          if (lhsSum.asPow.isDefined) lhsFactors = List[ArithExpr](lhsSum.asPow.get)
          else lhsFactors = List[ArithExpr](lhs)
        }
        else lhsFactors = List[ArithExpr](lhs)
        rhsFactors = rhsFs
      case _ =>
        // Try power representation of lhs (since product one doesn't exist)
        if (lhs.isInstanceOf[Sum]) {
          val lhsSum = lhs.toSum.get
          if (lhsSum.asPow.isDefined) lhsFactors = List[ArithExpr](lhsSum.asPow.get)
          else lhsFactors = List[ArithExpr](lhs)
        }
        else lhsFactors = List[ArithExpr](lhs)

        // Try power representation of rhs (since product one doesn't exist)
        if (rhs.isInstanceOf[Sum]) {
          val rhsSum = rhs.toSum.get
          if (rhsSum.asPow.isDefined) rhsFactors = List[ArithExpr](rhsSum.asPow.get)
          else rhsFactors = List[ArithExpr](rhs)
        }
        else rhsFactors = List[ArithExpr](rhs)
    }
    mergeFactors(lhsFactors,rhsFactors)
  }

  /**
   * Merges together factor lists of the two expressions to be multiplied
   *
   * @param lhsFactors   Factor list of left-hand-side expression
   * @param rhsFactors   Factor list of right-hand-side expression
   * @return             Result of multiplying the two corresponding expressions
   */
  def mergeFactors(lhsFactors : List[ArithExpr], rhsFactors : List[ArithExpr]) : ArithExpr = {
    var merged = ListBuffer[ArithExpr]()
    merged = merged.addAll(lhsFactors)
    var simplified = false
    var i = 0
    for (rhsFactor <- rhsFactors) {
      var combined = false
      val n = merged.length
      i = 0
      while (i < n) {
        val factor = merged(i)
        val newFactor = combineFactors(rhsFactor, factor)
        if (newFactor.isDefined) {
          merged = Helper.replaceAt(i,newFactor.get,merged)
          combined = true
          simplified = true
          i = n
        }
        i += 1
      }
      if (!combined) merged += rhsFactor
    }
    if (simplified) merged.reduce(_ * _)
    else {
      if (merged.isEmpty) Cst(1)
      else if (merged.size == 1) merged.head
      else Prod(merged.toList.sortWith(ArithExpr.isCanonicallySorted))
    }

  }
  /**
   * Try to combine a pair of factors.
   *
   * @param lhs The first factor
   * @param rhs The second factor
   * @return An option containing an expression if the factors can be combined, None otherwise
   */
  def combineFactors(lhs: ArithExpr, rhs: ArithExpr) : Option[ArithExpr] = (lhs, rhs) match {
    // Special values
    case (arithmetic.?,_) | (_,arithmetic.?) => Some(?)
    case (PosInf, NegInf) | (NegInf, PosInf)  => Some(NegInf)
    case (PosInf, PosInf) | (NegInf, NegInf)  => Some(PosInf)
    case (PosInf, y) => y.sign match {
      case Sign.Unknown => Some(?)
      case Sign.Positive => Some(PosInf)
      case Sign.Negative => Some(NegInf)
    }
    case (x, PosInf) =>  x.sign match {
      case Sign.Unknown => Some(?)
      case Sign.Positive => Some(PosInf)
      case Sign.Negative => Some(NegInf)
    }
    case (NegInf, y) =>  y.sign match {
      case Sign.Unknown => Some(?)
      case Sign.Positive => Some(NegInf)
      case Sign.Negative => Some(PosInf)
    }
    case (x, NegInf) =>  x.sign match {
      case Sign.Unknown => Some(?)
      case Sign.Positive => Some(NegInf)
      case Sign.Negative => Some(PosInf)
    }

    // Trivial cases
    case (Cst(x), Cst(y)) => Some(Cst(x * y))
    case (Cst(1), _) => Some(rhs)
    case (_, Cst(1)) => Some(lhs)

    // Constant cases
    // Compute powers when all bases and exponents are positive constants
    case (Pow(Cst(b1), e1), Pow(Cst(b2), e2)) if e1 > 0 && e2 > 0 =>
      Some(Cst((Math.pow(b1, e1) * Math.pow(b2, e2)).toInt))
    // Compute powers when all bases and exponents are negative constants
    case (Pow(Cst(b1), e1), Pow(Cst(b2), e2)) if e1 < 0 && e2 < 0 =>
      Some(SimplifyPow(Cst((Math.pow(b1, -e1) * Math.pow(b2, -e2)).toInt), -1))

    // Common factor
    case (Cst(x), Pow(Cst(y), -1)) if ComputeGCD.gcdLong(x,y) != 1 =>
      val gcd = ComputeGCD.gcdLong(x,y)
      Some(Cst(x / gcd) * SimplifyPow(Cst(y / gcd), -1))
    case (Pow(Cst(y), -1), Cst(x)) if ComputeGCD.gcdLong(x,y) != 1 =>
      val gcd = ComputeGCD.gcdLong(x,y)
      Some(Cst(x / gcd) * SimplifyPow(Cst(y / gcd), -1))


    // Couple more special cases
    case (Cst(x), Pow(Cst(y), e2)) if e2 < 0 && x % y == 0 =>
      Some(Cst(x / y) * SimplifyPow(Cst(y), e2 + 1))
    case (Pow(Cst(y), e1), Cst(x)) if e1 < 0 && x % y == 0 =>
      Some(Cst(x / y) * SimplifyPow(Cst(y), e1 + 1))

    case (Cst(x), Pow(Cst(y), -1)) if y % x == 0 && x != -1 =>
      Some(SimplifyPow(Cst(y / x), -1))
    case (Pow(Cst(y), -1), Cst(x)) if y % x == 0  && x != -1 =>
      Some(SimplifyPow(Cst(y / x), -1))

    case (Pow(Cst(x), e1), Pow(Cst(y), e2)) if e1 == e2 =>
      Some(Pow(Cst(x*y), e1))

    // Non-constant cases
    case (Pow(b1,e1), Pow(b2,e2)) if b1 == b2 => Some(b1 pow (e1+e2))
    case (x, Pow(b,e)) if x == b => Some(b pow (e+1))
    case (Pow(b,e),x) if x == b => Some(b pow (e+1))
    case (x,y) if x==y => Some(x pow 2)
    case _ => None
  }
}
