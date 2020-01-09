package arithmetic
package simplifier

object SimplifyCeiling {
  def apply(ae: ArithExpr): ArithExpr = {
    ae match {
      case c: Cst => c
      // Get constant term out of sum
      case Sum(terms) if terms.head.isInstanceOf[Cst] =>
        val cst = terms.head
        if (terms.length == 2) cst + ceil(terms.last)
        else cst + ceil(Sum(terms.tail))
      // Nested with floor/ceiling
      case f : FloorFunction => f
      case c : CeilingFunction => c
      case _ =>
        try {
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
