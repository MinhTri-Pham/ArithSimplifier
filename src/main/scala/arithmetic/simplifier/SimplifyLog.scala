package arithmetic
package simplifier

object SimplifyLog {

  def apply(b:Int, ae:ArithExpr) : ArithExpr = ae match {
    case Cst(c) =>
      if (c == 1) return Cst(0)
      if (c == b) return Cst(1)
      val res1 = scala.math.log(c) / scala.math.log(b)
      if (res1.isValidInt) Cst(res1.toInt)
      else LogFunction(b,ae)
    case _:Var => LogFunction(b,ae)
    case Pow(base, e) => e * log(b,base)
    case Prod(factors) => factors.reduce((x,y)=> log(b,x) + log(b,y))
    case _ => LogFunction(b,ae)
  }
}