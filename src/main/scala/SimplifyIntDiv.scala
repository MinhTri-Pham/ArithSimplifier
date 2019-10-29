object SimplifyIntDiv {

  def apply(numer : ArithExpr, denom : ArithExpr) : ArithExpr = simplifyIntDiv(numer,denom)

  def simplifyIntDiv(numer: ArithExpr, denom: ArithExpr): ArithExpr = (numer, denom) match {
    case (_, Cst(1)) => numer
    case (Cst(x), Cst(y)) if y != 0 => Cst(x/y)
    case (Cst(0),_) => Cst(0)
    case (p:Prod,Cst(n)) if p.cstFactor >= n => Cst(p.cstFactor / n) * p.nonCstFactor
    case (p:Prod,Cst(n)) if p.cstFactor < n => p.nonCstFactor
    case (s:Sum,c:Cst) => s.terms.map(x => x/c).reduce((a,b) => a+b)
  }

}
