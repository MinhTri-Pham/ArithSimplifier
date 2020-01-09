package arithmetic

object Sign extends Enumeration {

  type Sign = Value
  val Positive, Negative, Unknown = Value

  def apply(ae: ArithExpr): Value = {
    ae match {
      case Cst(c)=> if (c >= 0) Sign.Positive else Sign.Negative
      case Var(_,range,_) => signVar(range)
      case Prod(factors) => signProd(factors)
      case Sum(terms) => signSum(terms)
      case Pow(b,e) =>
        if (e % 2 == 0) Sign.Positive
        else b.sign
      case _ => Sign.Unknown
    }
  }

  def reverse(s: Sign): Sign = {
    s match {
      case Sign.Positive => Sign.Negative
      case Sign.Negative => Sign.Positive
      case Sign.Unknown => Sign.Unknown
    }
  }

  private def signVar(range: Interval): Sign.Value = {
    if (range.intervalMin.sign == Sign.Positive)
      Sign.Positive
    else if (range.intervalMax.sign == Sign.Negative)
      Sign.Negative
    else
      Sign.Unknown
  }

  private def signProd(factors: List[ArithExpr]): Sign.Value = {
    factors.foldLeft(Sign.Positive)((s: Sign.Value, factor) =>
      s match {
        case Sign.Positive => factor.sign
        case Sign.Negative => Sign.reverse(factor.sign)
        case Sign.Unknown => return Sign.Unknown
      })
  }

  private def signSum(terms: List[ArithExpr]) : Sign.Value = {
    val mins = terms.map(term => term.min)
    val maxs = terms.map(term => term.max)
    val sumMins = mins.reduce((x,y) => x+y)
    if (sumMins.sign.equals(Sign.Positive)) return Sign.Positive
    val sumMaxs = maxs.reduce((x,y) => x+y)
    if (sumMaxs.sign.equals(Sign.Negative)) return Sign.Negative
    Sign.Unknown
  }

}
