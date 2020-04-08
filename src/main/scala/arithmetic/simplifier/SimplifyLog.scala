package arithmetic
package simplifier

object SimplifyLog {
  // Log simplifier - promote log_b(ae) into a simpler expression
  def apply(b:Long, ae:ArithExpr) : ArithExpr = ae match {
    case Cst(c) =>
      if (c <= 0) throw new ArithmeticException()
      if (c == 1) return Cst(0)
      if (c == b) return Cst(1)
      if (b > c) return log(c,Cst(b)) pow -1
      val res1 = scala.math.log(c) / scala.math.log(b)
      if (res1.isValidInt) Cst(res1.toInt)
      else LogFunction(b,ae)
    case _:Var => LogFunction(b,ae)
    case Pow(base, e) => e * log(b,base)
    case Prod(factors) => factors.reduce((x,y)=> log(b,x) + log(b,y))
    case _ => LogFunction(b,ae)
  }
}