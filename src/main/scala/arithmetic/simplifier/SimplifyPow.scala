package arithmetic
package simplifier

object SimplifyPow {

  def apply(b : ArithExpr, e : Int) : ArithExpr = simplify(b,e)

  /**
   * Try to promote a power to a different expression.
   * @param base The base.
   * @param exp The exponent.
   * @return The simplified expression, a Pow object if simplification isn't possible
   */

  def simplify(base: ArithExpr, exp: Int) : ArithExpr = (base, exp) match {
    case (_,0) => Cst(1)
    case (_,1) => base
    // Negative base to the power of minus one
    // (-b)^-1 => -1 * b^-1
    case (Cst(b), -1) if b < 0 => SimplifyProd(-1,SimplifyPow(Cst(-b), -1))
    case (Cst(c),_) if c == 0 || c == 1 => base
    // Constant positive exponent
    case (Cst(b), e) if e > 1 => Cst(scala.math.pow(b,e).toLong)
    // Constant negative exponents: pow(x,-y) = pow(pow(x,y), -1)
    case (Cst(b), e) if e < -1 => Cst(scala.math.pow(b, -e).toLong) pow -1
    case (Pow(b,e1),e2) => b pow (e1*e2)
    // Special values
    case (PosInf, e) =>
      if (e > 0) PosInf
      else Cst(0)
    case (NegInf, e) =>
      if (e > 0 && e % 2 == 0) PosInf
      else if (e > 0) NegInf
      else Cst(0)
    case (?, _) => ?
    case _ => Pow(base,exp) // Can't simplify
  }
}
