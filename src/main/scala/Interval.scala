sealed abstract class Interval {

  val min : ArithExpr
  val max : ArithExpr

  override def equals(that: Any): Boolean = that match {
    case r: Interval => this.min == r.min && this.max == r.max
    case _ => false
  }
}

case class ClosedInterval(start: ArithExpr, end: ArithExpr) extends Interval {
  override val min: ArithExpr = start
  override val max: ArithExpr = end
}

//case class OpenRightInterval(start: ArithExpr, end: ArithExpr) extends Interval {
//  override val min: ArithExpr = start
//  override val max: ArithExpr = ?
//}
//
//case class OpenLeftInterval(start: ArithExpr, end: ArithExpr) extends Interval {
//  override val min: ArithExpr = ?
//  override val max: ArithExpr = end
//}
//
//case class OpenInterval(start: ArithExpr, end: ArithExpr) extends Interval {
//  override val min: ArithExpr = ?
//  override val max: ArithExpr = ?
//}

case object IntervalUnknown extends Interval {
  override val min: ArithExpr = ?
  override val max: ArithExpr = ?
}
