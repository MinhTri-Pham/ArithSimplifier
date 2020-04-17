package arithmetic
package simplifier

object SimplifyLog {
  /**
   * Tries to promote a logarithm to a different expression
   *
   * @param b            Base of logarithm
   * @param ae           Argument of the logarithm
   * @return             Promoted object if possible, otherwise just leave as Log_b(ae)
   */
  def apply(b:Long, ae:ArithExpr) : ArithExpr = ae match {
    // Constant cases
    case Cst(c) =>
      if (b <= 0 || c <= 0 || (b == 1 && c != 1)) throw new ArithmeticException()
      if (c == 1) return Cst(0)
      if (c == b) return Cst(1)
      if (b > c) return log(c,Cst(b)) pow -1
      val res1 = scala.math.log(c) / scala.math.log(b)
      if (res1.isValidInt) Cst(res1.toInt)
      else LogFunction(b,ae)
    // log_b(x^n) = n * log_b(x)
    case Pow(base, e) => e * log(b,base)
    // log of product is sum of logs
    case Prod(factors) => factors.reduce((x,y)=> log(b,x) + log(b,y))
    case _ => LogFunction(b,ae) // Can't promote
  }
}