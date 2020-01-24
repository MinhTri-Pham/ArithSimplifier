package arithmetic
package simplifier

object SimplifyFloor {

  def apply(ae: ArithExpr): ArithExpr = {
    if (ae.isInt) return ae
    ae match {
      case _:Var => FloorFunction(ae)
      // Get integer terms out of sum
      case Sum(terms) if terms.exists(_.isInt) =>
        val intTerms = terms.filter(_.isInt)
        val nonIntTerms = terms.filter(!_.isInt)
        val nonCstTerm = nonIntTerms.reduce(_+_)
        intTerms.reduce(_+_) + floor(nonCstTerm)

      // Product - expand out and apply floor to sum
      case p:Prod if p.asExpandedSum.isDefined =>
        floor(p.asExpandedSum.get)
      case _ =>
        try {
          // Try to directly evaluate floor using Scala
          val d = FloorFunction(ae).evalDouble
          assert(d.isValidInt)
          Cst(d.toInt)
        } catch {
          case NotEvaluableException() =>
            // Not possible, let's try to evaluate floor of min and max
            try {
              val min = FloorFunction(ae.min).evalDouble
              val max = FloorFunction(ae.max).evalDouble
              if (min == max) {
                assert(min.isValidInt)
                return Cst(min.toInt)
              }
            } catch {
              case NotEvaluableException() => FloorFunction(ae)
              case e: Throwable => throw e
            }
            FloorFunction(ae)
          case e: Throwable => throw e
        }
    }
  }
}
