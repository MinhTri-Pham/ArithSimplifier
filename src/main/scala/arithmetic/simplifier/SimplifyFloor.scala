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
        var evalTermsNum = 0
        var evalTerms = ListBuffer[ArithExpr]()
        var remTermsNum = 0
        var remTerms = ListBuffer[ArithExpr]()
        for (t <- terms) {
          if (t.isInt) {
            intTermsNum += 1
            intTerms += t
          }
          else if (t.isEvaluable) {
            evalTermsNum += 1
            evalTerms += t
          }
          else {
            remTermsNum += 1
            remTerms += t
          }
        }
        val intTerm = if (intTermsNum == 0) Cst(0) else intTerms.reduce(_ + _)
        val evalTerm = if (evalTermsNum == 0) Cst(0) else evalTerms.reduce(_ + _)
        if (remTermsNum == 0) {
          val floorOfEvalTerm = FloorFunction(evalTerm).evalDouble
          intTerm + Cst(floorOfEvalTerm.toInt)
        }
        else {
          val remTerm = remTerms.reduce(_ + _)
          val nonIntTerm = evalTerm + remTerm
          intTerm + FloorFunction(nonIntTerm)
        }
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
