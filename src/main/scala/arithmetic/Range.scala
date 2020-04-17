package arithmetic

class Range(min: ArithExpr, max: ArithExpr) {

  lazy val rangeMin: ArithExpr = min
  lazy val rangeMax: ArithExpr = max

  override def equals(that: Any): Boolean = that match {
    case r: Range => this.rangeMin == r.rangeMin && this.rangeMax == r.rangeMax
    case _ => false
  }

  // Product of two ranges
  def * (that: Range) : Range = {
    val x1 = min
    val x2 = max
    val y1 = that.rangeMin
    val y2 = that.rangeMax
    val prods = List(x1*y1,x1*y2,x2*y1,x2*y2)
    val minProd = ArithExpr.min(prods)
    val maxProd = ArithExpr.max(prods)
    if (minProd == ? && maxProd == ?) Range(?,?)
    else if (minProd == ?) Range(?,maxProd)
    else if (maxProd == ?) Range(minProd,?)
    else Range(minProd, maxProd)
  }
}

object Range {
  def apply(min: ArithExpr, max: ArithExpr): Range = new Range(min, max)

  // Assume everything by default positive
  def apply() : Range = new Range(Cst(0),PosInf)

  // Range of a product - reduce using repeated range product defined above
  def computeIntervalProd(factors: List[ArithExpr]) : Range = {
    val minMax = factors.map(x => Range(x.min, x.max))
    minMax.reduce((x,y) => x*y)
  }
}
