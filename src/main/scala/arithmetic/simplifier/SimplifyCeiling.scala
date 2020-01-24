package arithmetic
package simplifier

object SimplifyCeiling {
  def apply(ae: ArithExpr): ArithExpr = {
    if (ae.isInt) return ae
    ae match {
      case c: Cst => c
      case _:Var => CeilingFunction(ae)
      // Get integer terms/factors out of sum/product
      case Sum(terms) if terms.exists(_.isInt) =>
        val intTerms = terms.filter(_.isInt)
        val nonIntTerms = terms.filter(!_.isInt)
        val nonCstTerm = nonIntTerms.reduce(_+_)
        intTerms.reduce(_+_) + ceil(nonCstTerm)
      case _ =>
        try {
          // Try to directly evaluate ceiling using Scala
          val d = CeilingFunction(ae).evalDouble
          assert(d.isValidInt)
          Cst(d.toInt)
        } catch {
          case NotEvaluableException() =>
            // ok let's try to evaluate ceiling of min and max
            try {
              val min = CeilingFunction(ae.min).evalDouble
              val max = CeilingFunction(ae.max).evalDouble
              if (min == max) {
                assert(min.isValidInt)
                return Cst(min.toInt)
              }
            } catch {
              case NotEvaluableException() => CeilingFunction(ae)
              case e: Throwable => throw e
            }
              CeilingFunction(ae)
          case e: Throwable => throw e
        }
    }
  }
}
