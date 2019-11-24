class Interval(min: ArithExpr, max: ArithExpr) {

  lazy val intervalMin: ArithExpr = min
  lazy val intervalMax: ArithExpr = max

  override def equals(that: Any): Boolean = that match {
    case r: Interval => this.intervalMin == r.intervalMin && this.intervalMax == r.intervalMax
    case _ => false
  }

  def * (that: Interval) : Interval = {
    val x1 = min
    val x2 = max
    val y1 = that.intervalMin
    val y2 = that.intervalMax
    val prods = List(x1*y1,x1*y2,x2*y1,x2*y2)
    val minProd = ArithExpr.minList(prods)
    val maxProd = ArithExpr.maxList(prods)
    if (minProd == ? || maxProd == ?) Interval(?,?)
    else if (minProd == ?) Interval(?,maxProd)
    else if (maxProd == ?) Interval(minProd,?)
    else Interval(minProd, maxProd)
  }
}

object Interval {
  def apply(min: ArithExpr, max: ArithExpr): Interval = new Interval(min, max)

  def apply() : Interval = new Interval(?,?)
}
