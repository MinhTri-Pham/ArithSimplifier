package arithmetic
package simplifier

import scala.collection.mutable.ListBuffer

object SimplifyCeiling {
  def apply(ae: ArithExpr): ArithExpr = {
    if (ae.isInt) return ae
    ae match {
      case c: Cst => c
      case _:Var => CeilingFunction(ae)
      // Get integer terms/factors out of sum/product
      case Sum(terms) =>
        var intTermsNum = 0
        var intTerms = ListBuffer[ArithExpr]()
        var ceilEvalNum = 0
        var ceilEvalTerms = ListBuffer[ArithExpr]()
        var nonEvalNum = 0
        var nonEvalTerms = ListBuffer[ArithExpr]()
        for (t <- terms) {
          if (t.isInt) {
            intTermsNum += 1
            intTerms += t
          }
          else if (floor(t).isInstanceOf[Cst]) {
            ceilEvalNum += 1
            ceilEvalTerms += ceil(t)
          }
          else {
            nonEvalNum += 1
            nonEvalTerms += t
          }
        }
        val intTerm = if (intTermsNum == 0) Cst(0) else intTerms.reduce(_ + _)
        val floorEvalTerm = if (ceilEvalNum == 0) Cst(0) else ceilEvalTerms.reduce(_ + _)
        val nonEvalTerm = if (nonEvalNum == 0) Cst(0) else CeilingFunction(nonEvalTerms.reduce(_ + _))
        intTerm + floorEvalTerm + nonEvalTerm
      // Product - expand out and apply floor to sum
      case p:Prod if p.asSum.isDefined =>
        ceil(p.asSum.get)
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
