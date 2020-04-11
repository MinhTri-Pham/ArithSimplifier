package arithmetic

object Sign extends Enumeration {

  type Sign = Value
  val Positive, Negative, Unknown = Value

  def apply(ae: ArithExpr): Value = {
    ae match {
      case Cst(c) => if (c >= 0) Sign.Positive else Sign.Negative
      case Var(_,range,_,_) => signVar(range)
      case p:Prod => signProd(p.factors)
      case s:Sum => signSum(s.terms)
      case Pow(b,e) =>
        if (e % 2 == 0) Sign.Positive
        else b.sign
      case AbsFunction(_) => Sign.Positive
      case FloorFunction(e) => e.sign
      case CeilingFunction(e) =>
        ArithExpr.isSmaller(Cst(-1), e) match {
          case Some(true) => Sign.Positive
          case Some(false) => Sign.Negative
          case _ => Sign.Unknown
        }
      case PosInf => Sign.Positive
      case NegInf => Sign.Negative
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
    val unknownSignTerms = terms.filter(_.sign == Sign.Unknown)
    if (unknownSignTerms.nonEmpty) return Sign.Unknown
    val posTerms = terms.filter(_.sign == Sign.Positive)
    val negTerms = terms.filter(_.sign == Sign.Negative)
    if (posTerms.isEmpty) Sign.Negative
    else if (negTerms.isEmpty) Sign.Positive
    else {
      val absSumNegTerms = abs(negTerms.fold(Cst(0))(_ + _))
      val sumPosTerms = posTerms.fold(Cst(0))(_ + _)
      val lhsSmaller = ArithExpr.isSmaller(absSumNegTerms, sumPosTerms)
      val rhsSmaller = ArithExpr.isSmaller(sumPosTerms, absSumNegTerms)
      if (lhsSmaller.isEmpty || rhsSmaller.isEmpty)
        Sign.Unknown
      else if (lhsSmaller.get)
        Sign.Positive
      else
        Sign.Negative
    }
  }
}
