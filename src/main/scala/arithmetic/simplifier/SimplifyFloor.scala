package arithmetic
package simplifier

import scala.collection.mutable.ListBuffer

object SimplifyFloor {

  def apply(ae: ArithExpr): ArithExpr = {
    if (ae.isInt) return ae
    ae match {
      case _:Var => FloorFunction(ae)
      // Get integer terms out of sum
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

      // Fraction with sum numerator - try subset idea
      // Maybe too complicated, modify addition instead?
      case x if x.asSumFraction.isDefined =>
        val numer = x.asSumFraction.get._1
        val denom = x.asSumFraction.get._2
        val termsExpanded = Helper.expandTermsCst(numer.terms)
        val termSubsets = Helper.powerSet(termsExpanded).filter(_.nonEmpty)
        if (termSubsets.nonEmpty) {
          for (subset <- termSubsets) {
            val restTerms = termsExpanded.diff(subset)
            val sum = subset.reduce(_ + _)
            val rest = if (restTerms.isEmpty) Cst(0) else restTerms.reduce(_ + _)
            val gcd = ComputeGCD(sum, denom)
            if (gcd != Cst(1)) {
              return floor(sum /^ denom) + floor(rest /^ denom)
            }
          }
        }
        FloorFunction(x)
      // Product - expand out and apply floor to sum
      case p:Prod if p.asSum.isDefined =>
        floor(p.asSum.get)
      case _ =>
        try {
          // Try to directly evaluate floor using Scala
          val d = FloorFunction(ae).evalDouble
          assert(d.isValidInt)
          Cst(d.toInt)
        } catch {
          case NotEvaluableException() =>
            // Not possible, let's try to evaluate floor of min and max
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

  def main(args: Array[String]): Unit = {
    val a = Var("a", isInt = true)
    val b = Var("b", isInt = true)
    val c = Var("c", isInt = true)
    val fl = floor(c /^(a+b))
    val prod = (-1)*(a+b)*fl
    println(prod.toSum)
    println(c % (a+b))
  }

}
