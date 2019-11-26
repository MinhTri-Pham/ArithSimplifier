object SimplifyCeiling {
  def apply(ae: ArithExpr): ArithExpr = {
    ae match {
      case c: Cst => c
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
