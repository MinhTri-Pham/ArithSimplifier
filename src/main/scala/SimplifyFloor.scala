object SimplifyFloor {

  def apply(ae: ArithExpr): ArithExpr = {
    ae match {
      case c: Cst => c
      case _ =>
        try {
          val d = FloorFunction(ae).evalDouble
          assert(d.isValidInt)
          Cst(d.toInt)
        } catch {
          case NotEvaluableException() =>
            // ok let's try to evaluate floor of min and max
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
