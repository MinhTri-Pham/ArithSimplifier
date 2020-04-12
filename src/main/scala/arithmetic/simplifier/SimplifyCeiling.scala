package arithmetic
package simplifier

import scala.collection.mutable.ListBuffer

// Ceiling simplifier
object SimplifyCeiling {
  def apply(ae: ArithExpr): ArithExpr = {
    if (ae.isInt) return ae // Ceiling of integer is the integer itself
    if (ae.isEvaluable) {
      val ceilEval = CeilingFunction(ae).evalDouble
      return Cst(ceilEval.toInt)
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
          // Find all integer terms and how many of them are there
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
          // Take out integer and add ceiling of evaluables using Scala
          val ceilOfEvalTerm = CeilingFunction(evalTerm).evalDouble
          intTerm + Cst(ceilOfEvalTerm.toInt)
        }
        else {
          val remTerm = remTerms.reduce(_ + _)
          // Take integer out, try min and max on the rest
          intTerm + tryBounds(evalTerm + remTerm)
        }
      case _ => tryBounds(ae)
    }
  }

  def tryBounds(ae : ArithExpr) : ArithExpr = {
    try {
      val min = CeilingFunction(ae.min).evalDouble
      val max = CeilingFunction(ae.max).evalDouble
      if (min == max) return Cst(min.toInt)
    } catch {
      case NotEvaluableException() => CeilingFunction(ae)
      case e: Throwable => throw e
    }
    CeilingFunction(ae)
  }
}
