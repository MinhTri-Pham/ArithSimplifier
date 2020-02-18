package arithmetic
package simplifier

object SimplifyPow {

  def apply(b : ArithExpr, e : Int) : ArithExpr = simplifyPow(b,e)

  // Try to simplify to another expression if possible
  def simplifyPow(base: ArithExpr, exp: Int) : ArithExpr = (base, exp) match {
    case (_,0) => Cst(1)
    case (_,1) => base
    case (Cst(c),_) if c == 0 || c == 1 => base
    case (Cst(c),_) if c == -1 => if (exp % 2 == 0) Cst(1) else base
    // Constant positive exponent
    case (Cst(b), e) if e > 0 => Cst(scala.math.pow(b,e).toInt)
    // Constant negative exponents: pow(x,-y) = pow(pow(x,y), -1)  (closed form)
    case (Cst(b), e) if e < -1 => Cst(scala.math.pow(b, -e).toInt) pow -1
    case (Pow(b,e1),e2) => b pow (e1*e2)
    case (?, _) => ?
    case _ => Pow(base,exp) // Can't simplify further
  }
}
