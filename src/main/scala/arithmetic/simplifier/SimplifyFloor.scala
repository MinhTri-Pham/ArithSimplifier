package arithmetic
package simplifier

import scala.collection.mutable.ListBuffer

object SimplifyFloor {

  def apply(ae: ArithExpr): ArithExpr = {
    if (ae.isInt) return ae // Floor of integer is the integer itself
    ae match {
      case _:Var => FloorFunction(ae) // The variable can't be an integer so leave input as it is
      // Work with sum representation if possible
      case Sum(terms) =>
        var intTermsNum = 0
        var intTerms = ListBuffer[ArithExpr]()
        var evalTermsNum = 0
        var evalTerms = ListBuffer[ArithExpr]()
        var remTermsNum = 0
        var remTerms = ListBuffer[ArithExpr]()
        // Find all integer terms and how many of them are there
        for (t <- terms) {
          if (t.isInt) {
            intTermsNum += 1
            intTerms += t
          }
          // Find all evaluable terms and how many of them are there
          else if (t.isEvaluable) {
            evalTermsNum += 1
            evalTerms += t
          }
          // All remaining terms and how many are there
          else {
            remTermsNum += 1
            remTerms += t
          }
        }
        // Sums of integer and evaluable terms
        val intTerm = if (intTermsNum == 0) Cst(0) else intTerms.reduce(_ + _)
        val evalTerm = if (evalTermsNum == 0) Cst(0) else evalTerms.reduce(_ + _)
        if (remTermsNum == 0) {
          // Take out integer and add floor of evaluables using Scala
          val floorOfEvalTerm = FloorFunction(evalTerm).evalDouble
          intTerm + Cst(floorOfEvalTerm.toInt)
        }
        else {
          // Take integer out, leave rest inside ceiling
          val remTerm = remTerms.reduce(_ + _)
          val nonIntTerm = evalTerm + remTerm
          intTerm + FloorFunction(nonIntTerm)
        }
      case _ =>
        try {
          // Try if expression is evaluable
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
