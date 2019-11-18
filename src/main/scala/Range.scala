sealed abstract class Range {

  val min : ArithExpr
  val max : ArithExpr

  override def equals(that: Any): Boolean = that match {
    case r: Range => this.min == r.min && this.max == r.max
    case _ => false
  }
}

case class ClosedRange(start: ArithExpr, end: ArithExpr) extends Range {
  override val min: ArithExpr = start
  override val max: ArithExpr = end
}

case class OpenRightRange(start: ArithExpr, end: ArithExpr) extends Range {
  override val min: ArithExpr = start
  override val max: ArithExpr = ?
}

case class OpenLeftRange(start: ArithExpr, end: ArithExpr) extends Range {
  override val min: ArithExpr = ?
  override val max: ArithExpr = end
}

case class OpenRange(start: ArithExpr, end: ArithExpr) extends Range {
  override val min: ArithExpr = ?
  override val max: ArithExpr = ?
}

case object RangeUnknown extends Range {
  override val min: ArithExpr = ?
  override val max: ArithExpr = ?
}
