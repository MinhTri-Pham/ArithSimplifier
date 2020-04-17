package arithmetic
package simplifier

import scala.collection.mutable.ListBuffer

object SimplifyFloor {

  /**
   * Try to promote floor(ae) to a different expression.
   * @param ae  Expression to take the floor of
   * @return    The simplified expression, a FloorFunction object if simplification isn't possible
   */

  def apply(ae: ArithExpr): ArithExpr = {
    if (ae.isInt) return ae // Floor of integer is the integer itself
    // Evaluable expression - compute floor using scala
    if (ae.isEvaluable) {
      val floorEval = FloorFunction(ae).evalDouble
      return Cst(floorEval.toInt)
    }
    ae match {
      // Work with sum representation if possible
      case Sum(terms) =>
        var intTermsNum = 0
        var intTerms = ListBuffer[ArithExpr]()
        var evalTermsNum = 0
        var evalTerms = ListBuffer[ArithExpr]()
        var remTermsNum = 0
        var remTerms = ListBuffer[ArithExpr]()
        for (t <- terms) {
          // Find all integer terms
          if (t.isInt) {
            intTermsNum += 1
            intTerms += t
          }
          // Find all evaluable terms
          else if (t.isEvaluable) {
            evalTermsNum += 1
            evalTerms += t
          }
          // Find all remaining terms
          else {
            remTermsNum += 1
            remTerms += t
          }
        }
        // Determine integer and evaluable part of input sum
        val intTerm = if (intTermsNum == 0) Cst(0) else intTerms.reduce(_ + _)
        val evalTerm = if (evalTermsNum == 0) Cst(0) else evalTerms.reduce(_ + _)
        if (remTermsNum == 0) {
          // All terms have an integer value or are evaluable
          // Take out integer part and evaluate floor of evaluable part using Scala
          val floorOfEvalTerm = FloorFunction(evalTerm).evalDouble
          intTerm + Cst(floorOfEvalTerm.toInt)
        }
        else {
          // There are some terms that don't have an integer value and aren't evaluable
          val remTerm = remTerms.reduce(_ + _)
          // Take integer term out, attempt to find floor of remaining part using min and max
          intTerm + tryBounds(evalTerm + remTerm)
        }
      case _ => tryBounds(ae)
    }
  }

  // Tries to find the floor of ae by considering its min and max
  // If their floors are the same, this is also the floor or ae
  // Otherwise return FloorFunction object, meaning that floor(ae) can't be simplified to another expression
  def tryBounds(ae : ArithExpr) : ArithExpr = {
    try {
      val min = FloorFunction(ae.min).evalDouble
      val max = FloorFunction(ae.max).evalDouble
      if (min == max) return Cst(min.toInt)
    } catch {
      case NotEvaluableException() => FloorFunction(ae)
      case e: Throwable => throw e
    }
    FloorFunction(ae)
  }
}
