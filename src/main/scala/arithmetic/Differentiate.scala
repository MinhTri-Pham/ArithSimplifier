package arithmetic

object Differentiate {

  private def differentiate(expr: ArithExpr, v:Var) : ArithExpr = (expr, v) match {
    case (_: Cst, _) => Cst(0)
    case (w: Var, _) =>
      if (v==w) Cst(1)
      else Cst(0)
    case (Pow(f, e), _) => Cst(e)*(f pow (e-1))*(f diff v)
    case (s: Sum, _) =>
      val derivatives = s.terms.map(t => t diff v)
      derivatives.reduce(_+_)
    // In the future, maybe expand out first if possible and use easier sum rule?
    case (p:Prod, _) =>
      val cstFactor = Cst(p.cstFactor)
      val nonCst = p.nonCstList
      if (nonCst.length == 1) cstFactor * (nonCst.head diff v)
      else if (nonCst.length == 2) cstFactor * productRule(nonCst.head, nonCst.last,v)
      else {
        val summands = p.factors.map(x => (p /^ x) * (x diff v))
        summands.reduce(_ + _)
      }
//    case (LogFunction(b,ae), _) => ... Need to define natural log
    case (AbsFunction(ae), _) => (ae /^ expr) * (ae diff v)
    case _ => ?
  }

  // Derivative of product of two expressions wrt v
  private def productRule(ae1:ArithExpr, ae2:ArithExpr, v:Var): ArithExpr = {
    ae1*(ae2 diff v) + ae2*(ae1 diff v)
  }

  def apply(expr:ArithExpr, v:Var) : ArithExpr = differentiate(expr,v)

  // Higher derivative
  def apply(expr:ArithExpr, v:Var, n:Int) : ArithExpr = {
    if (n < 0) throw new ArithmeticException()
    if (n == 0) return expr // Zeroth derivative is expression itself
    var derivative = expr
    var i = 1;
    while(i <= n) {
      derivative = differentiate(derivative,v)
      if (derivative == Cst(0)) i = n+1 // Avoid doing differentiating uselessly when reached 0
      else i+=1
    }
    derivative
  }
}
