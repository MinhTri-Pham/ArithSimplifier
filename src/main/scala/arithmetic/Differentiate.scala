package arithmetic

object Differentiate {

  def apply(expr: ArithExpr, v:Var) : ArithExpr = (expr, v) match {
    case (_: Cst, _) => Cst(0)
    case (w: Var, _) =>
      if (v==w) Cst(1)
      else Cst(0)
    case (s: Sum, _) =>
      val derivatives = s.terms.map(t => t diff v)
      derivatives.reduce(_+_)
    case (p:Prod, _) =>
      val nonCst = p.nonCstList
      val cst = Cst(p.cstFactor)
      if (nonCst.length == 1) cst * (nonCst.head diff v)
      else if (nonCst.length == 2) productRule(nonCst.head, nonCst.last,v)
      else {
        val ratios = p.factors.map(x => (x diff v) /^ x)
        p * ratios.reduce(_+_)
      }
    case (Pow(f, e), _) => Cst(e)*(f pow (e-1))*(f diff v)
    case (AbsFunction(ae), _) => (ae /^ expr) * (expr diff v)
    case _ => ?
  }

  // Derivative of product of two expressions wrt v
  private def productRule(ae1:ArithExpr, ae2:ArithExpr, v:Var): ArithExpr = {
    ae1*(ae2 diff v) + ae2*(ae1 diff v)
  }
}
