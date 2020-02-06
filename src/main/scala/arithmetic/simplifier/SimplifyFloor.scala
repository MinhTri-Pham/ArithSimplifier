package arithmetic
package simplifier

import scala.collection.mutable.ListBuffer

object SimplifyFloor {

  def apply(ae: ArithExpr): ArithExpr = {
    if (ae.isInt) return ae
    ae match {
      case _:Var => FloorFunction(ae)
      // Work with sum representation if possible
      case Sum(terms) =>
        var intTermsNum = 0
        var intTerms = ListBuffer[ArithExpr]()
        var floorEvalNum = 0
        var floorEvalTerms = ListBuffer[ArithExpr]()
        var nonEvalNum = 0
        var nonEvalTerms = ListBuffer[ArithExpr]()
        for (t <- terms) {
          if (t.isInt) {
            intTermsNum += 1
            intTerms += t
          }
          else if (floor(t).isInstanceOf[Cst]) {
            floorEvalNum += 1
            floorEvalTerms += floor(t)
          }
          else {
            nonEvalNum += 1
            nonEvalTerms += t
          }
        }
        val intTerm = if (intTermsNum == 0) Cst(0) else intTerms.reduce(_ + _)
        val floorEvalTerm = if (floorEvalNum == 0) Cst(0) else floorEvalTerms.reduce(_ + _)
        val nonEvalTerm = if (nonEvalNum == 0) Cst(0) else FloorFunction(nonEvalTerms.reduce(_ + _))
        intTerm + floorEvalTerm + nonEvalTerm
      case _ =>
        try {
          // Try to directly evaluate floor using Scala
          val d = FloorFunction(ae).evalDouble
          assert(d.isValidInt)
          Cst(d.toInt)
        } catch {
          case NotEvaluableException() => FloorFunction(ae)
          case e: Throwable => throw e
        }
    }
  }

}
