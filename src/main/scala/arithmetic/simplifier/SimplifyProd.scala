package arithmetic
package simplifier

import scala.collection.mutable.ListBuffer

object SimplifyProd {

  def apply(lhs: ArithExpr, rhs: ArithExpr): ArithExpr = multExprs(lhs, rhs)

  // Multiplies two expressions
  def multExprs(lhs: ArithExpr, rhs: ArithExpr): ArithExpr = {
    if (lhs == Cst(0) || rhs == Cst(0)) return Cst(0)
    if (lhs == ? || rhs == ?) return ?
    var lhsFactors, rhsFactors: List[ArithExpr] = List[ArithExpr]()
    (lhs, rhs) match {
      // Work with product representation if possible
      case (Prod(lhsFs), Prod(rhsFs)) =>
        lhsFactors = lhsFs
        rhsFactors = rhsFs
      case (Prod(lhsFs), _) =>
        lhsFactors = lhsFs
        if (rhs.isInstanceOf[Sum]) {
          val rhsSum = rhs.toSum.get
          if (rhsSum.asPow.isDefined) {
            rhsFactors = List[ArithExpr](rhsSum.asPow.get)

          }
          else rhsFactors = List[ArithExpr](rhs)
        }
        else rhsFactors = List[ArithExpr](rhs)
      case (_, Prod(rhsFs)) =>
        if (lhs.isInstanceOf[Sum]) {
          val lhsSum = lhs.toSum.get
          if (lhsSum.asPow.isDefined) {
            lhsFactors = List[ArithExpr](lhsSum.asPow.get)

          }
          else lhsFactors = List[ArithExpr](lhs)
        }
        else lhsFactors = List[ArithExpr](lhs)
        rhsFactors = rhsFs
      case _ =>
        if (lhs.isInstanceOf[Sum]) {
          val lhsSum = lhs.toSum.get
          if (lhsSum.asPow.isDefined) {
            lhsFactors = List[ArithExpr](lhsSum.asPow.get)

          }
          else lhsFactors = List[ArithExpr](lhs)
        }
        else lhsFactors = List[ArithExpr](lhs)

        if (rhs.isInstanceOf[Sum]) {
          val rhsSum = rhs.toSum.get
          if (rhsSum.asPow.isDefined) {
            rhsFactors = List[ArithExpr](rhsSum.asPow.get)

          }
          else rhsFactors = List[ArithExpr](rhs)
        }
        else rhsFactors = List[ArithExpr](rhs)
    }
    mergeFactors(lhsFactors,rhsFactors)
  }

  // Determine result of multiplication given the factors of expressions to be multiplied
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
        val term = merged(i)
        val newTerm = combineFactors(rhsFactor, term)
        if (newTerm.isDefined) {
          merged = Helper.replaceAt(i,newTerm.get,merged)
          combined = true
          simplified = true
          i = n
        }
        i += 1
      }
      if (!combined) merged += rhsFactor
    }
    if (simplified) merged.reduce(_ * _)
    else convert(merged.toList)

  }
  // Tries to combine a pair of factors
  def combineFactors(lhs: ArithExpr, rhs: ArithExpr) : Option[ArithExpr] = (lhs, rhs) match {
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

  // Given list of factors of simplified result, determine resulting expression
  // If result is a product, sort factors in canonical order
  def convert(factors: List[ArithExpr]): ArithExpr = {
    if (factors.isEmpty) Cst(1) // Everything simplified with each other
    else if (factors.length == 1) factors.head // Simplified expression is primitive
    else Prod(factors.sortWith(ArithExpr.isCanonicallySorted))
  }
}
