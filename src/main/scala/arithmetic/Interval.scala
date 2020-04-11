package arithmetic

class Interval(min: ArithExpr, max: ArithExpr) {

  lazy val intervalMin: ArithExpr = min
  lazy val intervalMax: ArithExpr = max

  override def equals(that: Any): Boolean = that match {
    case r: Interval => this.intervalMin == r.intervalMin && this.intervalMax == r.intervalMax
    case _ => false
  }

  // Product of two intervals
  def * (that: Interval) : Interval = {
    val x1 = min
    val x2 = max
    val y1 = that.intervalMin
    val y2 = that.intervalMax
    val prods = List(x1*y1,x1*y2,x2*y1,x2*y2)
    val minProd = ArithExpr.min(prods)
    val maxProd = ArithExpr.max(prods)
    if (minProd == ? && maxProd == ?) Interval(?,?)
    else if (minProd == ?) Interval(?,maxProd)
    else if (maxProd == ?) Interval(minProd,?)
    else Interval(minProd, maxProd)
  }
}

object Interval {
  def apply(min: ArithExpr, max: ArithExpr): Interval = new Interval(min, max)

  // For now assume everything by default positive
  def apply() : Interval = new Interval(Cst(0),PosInf)

  // Range of a product
  def computeIntervalProd(factors: List[ArithExpr]) : Interval = {
    val minMax = factors.map(x => Interval(x.min, x.max))
    minMax.reduce((x,y) => x*y)
  }
}
