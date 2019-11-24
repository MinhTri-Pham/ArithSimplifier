object SimplifyAbs {

  def apply(ae: ArithExpr): ArithExpr = {
    ae.sign match {
      case Sign.Positive => ae
      case Sign.Negative => Cst(-1) * ae
      case Sign.Unknown => AbsFunction(ae)
    }
  }

}
