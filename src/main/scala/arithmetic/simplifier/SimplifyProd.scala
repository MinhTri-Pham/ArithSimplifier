package arithmetic
package simplifier

import scala.collection.mutable.ListBuffer

object SimplifyProd {

  def apply(lhs: ArithExpr, rhs: ArithExpr): ArithExpr = multExprs(lhs, rhs)

  // Multiplies two expressions
  def multExprs(lhs: ArithExpr, rhs: ArithExpr): ArithExpr = {
    var lhsFactors, rhsFactors: List[ArithExpr] = List[ArithExpr]()
    (lhs, rhs) match {
            // If there's a power with product as base, expand it into a product of powers
            case (p1: Pow, p2:Pow) =>
              val lhsProd = p1.asProdPows
              val rhsProd = p2.asProdPows
              (lhsProd,rhsProd) match {
                case (Some(_), Some(_)) =>
                  lhsFactors = lhsProd.get.factors
                  rhsFactors = rhsProd.get.factors

                case (Some(_), None) =>
                  lhsFactors = lhsProd.get.factors
                  rhsFactors = List[ArithExpr](p2)

                case (None, Some(_)) =>
                  lhsFactors = List[ArithExpr](p1)
                  rhsFactors = rhsProd.get.factors

                case (None, None) =>
                  lhsFactors = List[ArithExpr](p1)
                  rhsFactors = List[ArithExpr](p2)
              }

            case (p: Pow, s:Sum) =>
              val lhsProd = p.asProdPows
              val rhsProd = s.asProd
              (lhsProd,rhsProd) match {
                case (Some(_), Some(_)) =>
                  lhsFactors = lhsProd.get.factors
                  rhsFactors = rhsProd.get.factors

                case (Some(_), None) =>
                  lhsFactors = lhsProd.get.factors
                  rhsFactors = List[ArithExpr](s)

                case (None, Some(_)) =>
                  lhsFactors = List[ArithExpr](p)
                  rhsFactors = rhsProd.get.factors

                case (None, None) =>
                  lhsFactors = List[ArithExpr](p)
                  rhsFactors = List[ArithExpr](s)
              }

            case (s:Sum, p:Pow) =>
              val lhsProd = s.asProd
              val rhsProd = p.asProdPows
              (lhsProd,rhsProd) match {
                case (Some(_), Some(_)) =>
                  lhsFactors = lhsProd.get.factors
                  rhsFactors = rhsProd.get.factors

                case (Some(_), None) =>
                  lhsFactors = lhsProd.get.factors
                  rhsFactors = List[ArithExpr](p)

                case (None, Some(_)) =>
                  lhsFactors = List[ArithExpr](s)
                  rhsFactors = rhsProd.get.factors

                case (None, None) =>
                  lhsFactors = List[ArithExpr](s)
                  rhsFactors = List[ArithExpr](p)
              }

            case (s1: Sum, s2: Sum) =>
              val lhsProd = s1.asProd
              val rhsProd = s2.asProd
              (lhsProd,rhsProd) match {
                case (Some(_), Some(_)) =>
                  lhsFactors = lhsProd.get.factors
                  rhsFactors = rhsProd.get.factors
                case (Some(_), None) =>
                  lhsFactors = lhsProd.get.factors
                  rhsFactors = List[ArithExpr](s2)

                case (None, Some(_)) =>
                  lhsFactors = List[ArithExpr](s1)
                  rhsFactors = rhsProd.get.factors

                case (None, None) =>
                  lhsFactors = List[ArithExpr](s1)
                  rhsFactors = List[ArithExpr](s2)
              }

            case (p: Pow, _) =>
              val pProd = p.asProdPows
              if (pProd.isDefined) lhsFactors = pProd.get.factors
              else lhsFactors = List[ArithExpr](p)
              rhsFactors = rhs.getTermsFactors

            case (_, p: Pow) =>
              val pProd = p.asProdPows
              if (pProd.isDefined) rhsFactors = pProd.get.factors
              else rhsFactors = List[ArithExpr](p)
              lhsFactors = lhs.getTermsFactors

            case (s: Sum, _) =>
              val sProd = s.asProd
              if (sProd.isDefined) lhsFactors = sProd.get.factors
              else lhsFactors = List[ArithExpr](s)
              rhsFactors = rhs.getTermsFactors

            case (_, s: Sum) =>
              val sProd = s.asProd
              if (sProd.isDefined) rhsFactors = sProd.get.factors
              else rhsFactors = List[ArithExpr](s)
              lhsFactors = lhs.getTermsFactors

            // Neither side is a sum or power, decompose into smaller terms and merge
            case _ =>
              // Extract and sort factors of both sides and merge
              lhsFactors = lhs.getTermsFactors
              rhsFactors = rhs.getTermsFactors
          }

//      // Work with product representation if possible
//      case (Prod(lhsFs), Prod(rhsFs)) =>
//        lhsFactors = lhsFs.sortWith(ArithExpr.isCanonicallySorted)
//        rhsFactors = rhsFs.sortWith(ArithExpr.isCanonicallySorted)
//      case (Prod(lhsFs), _) =>
//        lhsFactors = lhsFs.sortWith(ArithExpr.isCanonicallySorted)
//        rhsFactors = List[ArithExpr](rhs)
//      case (_, Prod(rhsFs)) =>
//        lhsFactors = List[ArithExpr](lhs)
//        rhsFactors = rhsFs.sortWith(ArithExpr.isCanonicallySorted)
//      case _ =>
//        lhsFactors = List[ArithExpr](lhs)
//        rhsFactors = List[ArithExpr](rhs)
//    }
//    mergeFactors(lhsFactors,rhsFactors)
        mergeFactors(lhsFactors,rhsFactors)
  }

  // Merges factors of expressions to be multiplied
  // Assumes both factor lists are canonically sorted
//  def mergeFactors(lhsFactors:List[ArithExpr], rhsFactors:List[ArithExpr]) : ArithExpr = {
//    val merged = ListBuffer[ArithExpr]()
//    val lhsSize = lhsFactors.length
//    val rhsSize = rhsFactors.length
//    var i,j = 0
//    while (i < lhsSize && j < rhsSize) {
//      val lhsTerm = lhsFactors(i)
//      val rhsTerm = rhsFactors(j)
//      val combinedTerm = combineFactors(lhsTerm, rhsTerm)
//      if (combinedTerm.isDefined) {
//        merged += combinedTerm.get
//        i+=1
//        j+=1
//      }
//      else {
//        if (ArithExpr.isCanonicallySorted(lhsTerm, rhsTerm)) {
//          merged += lhsTerm
//          i+=1
//        }
//        else {
//          merged += rhsTerm
//          j+=1
//        }
//      }
//    }
//    while (i < lhsSize) {
//      merged += lhsFactors(i)
//      i+=1
//    }
//    while (j < rhsSize) {
//      merged += rhsFactors(j)
//      j+=1
//    }
//    convert(merged.toList)
//  }

  // More robust then previous method
  // Sorting prevents a lot of unnecessary work
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

  // Given list of factors, determine resulting expression
  // By design, it doesn't contain any ones
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
}
