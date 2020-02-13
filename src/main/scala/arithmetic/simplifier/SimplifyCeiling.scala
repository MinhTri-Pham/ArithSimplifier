package arithmetic
package simplifier

import scala.collection.mutable.ListBuffer

object SimplifyCeiling {
  def apply(ae: ArithExpr): ArithExpr = {
    if (ae.isInt) return ae
    ae match {
      case c: Cst => c
      case _:Var => CeilingFunction(ae)
      // Work with sum representation if possible
      case Sum(terms) =>
        var intTermsNum = 0
        var intTerms = ListBuffer[ArithExpr]()
        var evalNum = 0
        var evalTerms = ListBuffer[ArithExpr]()
        var nonEvalNum = 0
        var nonEvalTerms = ListBuffer[ArithExpr]()
        for (t <- terms) {
          if (t.isInt) {
            intTermsNum += 1
            intTerms += t
          }
          else if (t.isEvaluable) {
            evalNum += 1
            evalTerms += t
          }
          else {
            nonEvalNum += 1
            nonEvalTerms += t
          }
        }
        val intTerm = if (intTermsNum == 0) Cst(0) else intTerms.reduce(_ + _)
        val evalTerm = if (evalNum == 0) Cst(0) else evalTerms.reduce(_ + _)
        val nonEvalTerm = if (nonEvalNum == 0) Cst(0) else nonEvalTerms.reduce(_ + _)
        if (nonEvalNum == 0) {
          val floorTerm = ceil(evalTerm)
          intTerm + floorTerm
        }
        else {
          val nonIntTerm = evalTerm + nonEvalTerm
          intTerm + CeilingFunction(nonIntTerm)
        }
      case _ =>
        try {
          // Try to directly evaluate ceiling using Scala
          val d = CeilingFunction(ae).evalDouble
          assert(d.isValidInt)
          Cst(d.toInt)
        } catch {
          case NotEvaluableException() => CeilingFunction(ae)
          case e: Throwable => throw e
        }
    }
  }
}
