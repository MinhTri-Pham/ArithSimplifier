package arithmetic
package simplifier

import scala.collection.mutable.ListBuffer

object SimplifyProd {

  def apply(lhs: ArithExpr, rhs: ArithExpr): ArithExpr = multExprs(lhs, rhs)

  // Multiplies two expressions
  def multExprs(lhs: ArithExpr, rhs: ArithExpr): ArithExpr = {
    var lhsFactors, rhsFactors: List[ArithExpr] = List[ArithExpr]()
    (lhs, rhs) match {
      // Work with product representation if possible
      case (Prod(lhsFs), Prod(rhsFs)) =>
        lhsFactors = lhsFs
        rhsFactors = rhsFs
      case (Prod(lhsFs), _) =>
        lhsFactors = lhsFs
        rhsFactors = List[ArithExpr](rhs)
      case (_, Prod(rhsFs)) =>
        lhsFactors = List[ArithExpr](lhs)
        rhsFactors = rhsFs
      case _ =>
        lhsFactors = List[ArithExpr](lhs)
        rhsFactors = List[ArithExpr](rhs)
    }
    if (lhsFactors.contains(?) || rhsFactors.contains(?)) return ?
    mergeFactors(lhsFactors,rhsFactors)
  }

  // Determine result of multiplication given the terms of multiplication to be added
  def mergeFactors(lhsFactors : List[ArithExpr], rhsFactors : List[ArithExpr]) : ArithExpr = {
    var merged = ListBuffer[ArithExpr]()
    merged = merged.addAll(lhsFactors)
    var i = 0
    for (rhsFactor <- rhsFactors) {
      var combined = false
      val n = merged.length
      i = 0
      while (i < n) {
        val term = merged(i)
        val newTerm = combineFactors(rhsFactor, term)
        if (newTerm.isDefined) {
          if (newTerm.get == Cst(1)) merged = Helper.removeAt(i,merged)
          else merged = Helper.replaceAt(i,newTerm.get,merged)
          combined = true
          i = n
        }
        i += 1
      }
      if (!combined) merged += rhsFactor
    }
    convert(merged.toList)
  }
  // Tries to combine a pair of factors
  def combineFactors(lhs: ArithExpr, rhs: ArithExpr) : Option[ArithExpr] = (lhs, rhs) match {
   // Trivial cases
    case (Cst(x), Cst(y)) => Some(Cst(x * y))
    case (Cst(1), _) => Some(rhs)
    case (_, Cst(1)) => Some(lhs)
    case (Cst(0), _) => Some(lhs)
    case (_, Cst(0)) => Some(rhs)

    // Constant cases
    // Compute powers when all bases and exponents are positive constants
    case (Pow(Cst(b1), e1), Pow(Cst(b2), e2)) if e1 > 0 && e2 > 0 =>
      Some(Cst((Math.pow(b1, e1) * Math.pow(b2, e2)).toInt))
    // Compute powers when all bases and exponents are negative constants
    case (Pow(Cst(b1), e1), Pow(Cst(b2), e2)) if e1 < 0 && e2 < 0 =>
      Some(SimplifyPow(Cst((Math.pow(b1, -e1) * Math.pow(b2, -e2)).toInt), -1))

    case (Cst(x), Pow(Cst(y), e2)) if e2 < 0 && x % y == 0 =>
      Some(Cst(x / y) * SimplifyPow(Cst(y), e2 + 1))
    case (Pow(Cst(y), e1), Cst(x)) if e1 < 0 && x % y == 0 =>
      Some(Cst(x / y) * SimplifyPow(Cst(y), e1 + 1))

    case (Cst(x), Pow(Cst(y), -1)) if y % x == 0 && x != -1 =>
      Some(SimplifyPow(Cst(y / x), -1))
    case (Pow(Cst(y), -1), Cst(x)) if y % x == 0  && x != -1 =>
      Some(SimplifyPow(Cst(y / x), -1))

    // Non-constant cases
    case (Pow(b1,e1), Pow(b2,e2)) if b1 == b2 => Some(b1 pow (e1+e2))
    case (x, Pow(b,e)) if x == b => Some(b pow (e+1))
    case (Pow(b,e),x) if x == b => Some(b pow (e+1))
    case (x,y) if x==y => Some(x pow 2)
    case _ => None
  }

  // Given list of factors of simplified result, determine resulting expression
  // If result is a product, sort factors in canonical order
  def convert(factors: List[ArithExpr]): ArithExpr = {
    if (factors.isEmpty) Cst(1) // Everything simplified with each other
    else if (factors.length == 1) factors.head // Simplified expression is primitive
    else flattenCst(factors)
  }

  // One thing that can happen is that resulting factors contain a constant
  // and a constant powers that could cancel each other out
  // This flattens these (if necessary) and gives final simplified expression
  def flattenCst(factors: List[ArithExpr]) : ArithExpr = {
    val cstPowCst = ListBuffer[ArithExpr]()
    // Collect constants and constant powers
    for (f<-factors) {
      f match {
        case _:Cst => cstPowCst += f
        case p:Pow => if (p.b.isInstanceOf[Cst]) cstPowCst += p
        case _ =>
      }
    }
    // Nothing to flatten
    if (cstPowCst.length < 2) Prod(factors.sortWith(ArithExpr.isCanonicallySorted))
    else {
      if (cstPowCst.head.isInstanceOf[Cst]) {
        var i = 1
        while (i < cstPowCst.length) {
          // Examine all constant power factors and look for case where we can combine constant with the power factor
          val combined = combineFactors(cstPowCst.head, cstPowCst(i))
          if (combined.isDefined) {
            // Delete combined factor
            val removePow = cstPowCst.take(i) ++ cstPowCst.drop(i + 1)
            val flattened = removePow.zipWithIndex.map(element => if (element._2 == 0) combined.get else element._1)
            val nonCstPow = factors.diff(cstPowCst)
            val allFactors = flattened ++ nonCstPow
            // Check that we didn't end up with a primitive expression
            if (allFactors.length == 1) return allFactors.head
            else return Prod(allFactors.toList.sortWith(ArithExpr.isCanonicallySorted))
          }
          i+=1
        }
      }
      // If no constant factor, also no need to flatten
      Prod(factors.sortWith(ArithExpr.isCanonicallySorted))
    }
  }

  def main(args: Array[String]): Unit = {
    val a: Var = Var("a")
    val b: Var = Var("b")
    val c: Var = Var("c")
    val d: Var = Var("d")
    val e: Var = Var("e")
    val f: Var = Var("f")
    val g: Var = Var("g")
    val h: Var = Var("h")
    val i: Var = Var("i")
    val k: Var = Var("k")
    val l: Var = Var("l")

    val exprs = (i pow -2) - 5 + 28*d
    println(Factorise(exprs))
  }
}
