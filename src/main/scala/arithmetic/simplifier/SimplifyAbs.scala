package arithmetic
package simplifier

object SimplifyAbs {
  // Tries to promote |ae| into different object depending on its sign
  def apply(ae: ArithExpr): ArithExpr = {
    ae.sign match {
      case Sign.Positive => ae
      case Sign.Negative => Cst(-1) * ae
      case Sign.Unknown => AbsFunction(ae)
    }
  }

}
