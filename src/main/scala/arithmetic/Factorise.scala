package arithmetic

import scala.collection.mutable.ListBuffer

object Factorise {
  // Factorises an expression
  def apply(e:ArithExpr) : Option[ArithExpr] = e match {
//    case c:Cst =>
//      val decomposition = primeDecomposition(c.value)
//      Some(Prod(decomposition.map(x => Cst(x))))
    case s:Sum =>factoriseSum(s)
    case p:Prod => factoriseProd(p)
    case _ => None
  }

  def factoriseSum(s: Sum) : Option[ArithExpr] = {
    if (s.terms.length < 2) return None
    val factorisation = factoriseTerms(s.terms)
    if (factorisation.isDefined) Some(factorisation.get.getFactors.reduce(_*_))
    else None
  }

  private def factoriseTerms(terms: List[ArithExpr]) : Option[ArithExpr] = {
    if (terms.length < 2) return None
    val gcd = ComputeGCD.commonTermList(terms)
    // Look for common factor
    if (gcd != Cst(1)) {
      val simplified = terms.map(x => x /^ gcd).reduce(_ + _)
      val simplifiedFactorisation = factoriseTerms(simplified.getTerms)
      // Try to factorise the simplified expression
      if (simplifiedFactorisation.isDefined) return Some(gcd * simplifiedFactorisation.get)
      Some(gcd*simplified)
    }
    else {
      val termsAsProds = asProds(terms)
      val factors = findFactors(termsAsProds)
      var i = 0
      // Loop over factors
      while (i < factors.length) {
        val currFactor = factors(i)
        // Find which terms does the factor appear in
        val containsF = ListBuffer[ArithExpr]()
        for (t <- terms) {
          if (ArithExpr.isMultipleOf(t,currFactor)) containsF += t
        }
        // If just one, move to the next
        if (containsF.length > 1) {
          // Idea: divide expression into two subexpressions
          // One subexpression contains some of the terms the factor is contained in
          // All other terms form the second subexpression
          // Try to recursively factorise these
          // Then combine factorisations (or just the subexpression(s) when can't factor) and try same algorithm with
          // the two formed expressions
          // Have to try all combinations
          // Fully expand all terms and the ones the factor is contained in
          val containsExpanded = Helper.expandTerms(containsF.toList)
          val termsExpanded = Helper.expandTerms(terms)
          // Try combinations
          for (subset <- Helper.powerSet(containsExpanded)) {
            if (subset.distinct.length > 1) {
              val rest = termsExpanded.diff(subset)
              // Take out factor from examined subset
              // Factorise recursively
              val fDivision = subset.map(x => x /^ currFactor).reduce(_ + _)
              val factorisedDivision = factoriseTerms(fDivision.getTerms)
              // Try to factorise the rest
              var restDivision : Option[ArithExpr] = None
              if (rest.distinct.length > 1) {
                val restTerm = rest.reduce(_ + _).getTerms
                restDivision = factoriseTerms(restTerm)
              }
              // See if we could factorise the two subexpressions and combine appropriately
              (factorisedDivision,restDivision) match {
                case (None,None) =>
                  val fTerm = currFactor*fDivision
                  if (rest.isEmpty) return Some(fTerm)
                  val restTerm = rest.reduce(_ + _)
                  val combinedFactorisation = factoriseTerms(List(fTerm,restTerm))
                  if (combinedFactorisation.isDefined) return combinedFactorisation

                case (Some(_), None) =>
                  val fTerm = currFactor * factorisedDivision.get
                  if (rest.isEmpty) return Some(fTerm)
                  val restTerm = rest.reduce(_ + _)
                  val combinedFactorisation = factoriseTerms(List(fTerm,restTerm))
                  if (combinedFactorisation.isDefined) return combinedFactorisation

                case (None, Some(_)) =>
                  val fTerm = currFactor * fDivision
                  if (rest.isEmpty) return Some(fTerm)
                  val combinedFactorisation = factoriseTerms(List(fTerm,restDivision.get))
                  if (combinedFactorisation.isDefined) return combinedFactorisation

                case (Some(_), Some(_)) =>
                  val fTerm = currFactor * factorisedDivision.get
                  val combinedFactorisation = factoriseTerms(List(fTerm,restDivision.get))
                  if (combinedFactorisation.isDefined) return combinedFactorisation
              }
            }
          }
        }
        i+=1
      }
      None
    }
  }

//  private def factoriseTerms2(terms: List[ArithExpr]) : Option[ArithExpr] = {
//    if (terms.length < 2) return None
//    val gcd = ComputeGCD.commonTermList(terms)
//    // Look for common factor
//    if (gcd != Cst(1)) {
//      val simplified = terms.map(x => x /^ gcd).reduce(_ + _)
//      val simplifiedFactorisation = factoriseTerms(simplified.toSum.get.asProds)
//      // Try to factorise the simplified expression
//      if (simplifiedFactorisation.isDefined) return Some(gcd * simplifiedFactorisation.get)
//      return Some(gcd*simplified)
//    }
//    val factors = findFactors(terms)
//    var i = 0
//    // Loop over factors
//    while (i < factors.length) {
//      val currFactor = factors(i)
//      // Find which terms does the factor appear in
//      val containsF = ListBuffer[ArithExpr]()
//      for (t <- terms) {
//        if (ArithExpr.isMultipleOf(t,currFactor)) containsF += t
//      }
//      // Common factor
//      if (containsF.length == terms.length) {
//        // Factor out common factor
//        val simplified = terms.map(x => x /^ currFactor).reduce(_ + _)
//        val simplifiedFactorisation = factoriseTerms(simplified.toSum.get.asProds)
//        // Try to factorise the simplified expression
//        if (simplifiedFactorisation.isDefined) return Some(currFactor * simplifiedFactorisation.get)
//        return Some(currFactor*simplified)
//      }
//      // Idea: divide expression into two subexpressions
//      // One subexpression contains some of the terms the factor is contained in
//      // All other terms form the second subexpression
//      // Try to recursively factorise these
//      // Then combine factorisations (or just the subexpression(s) when can't factor) and try same algorithm with
//      // the two formed expressions
//      // Have to try all combinations
//      else {
//        // Fully expand all terms and the ones the factor is contained in
//        val containsExpanded = Helper.expandTerms(containsF.toList)
//        val termsExpanded = Helper.expandTerms(terms)
//        // Try combinations
//        for (subset <- Helper.powerSet(containsExpanded)) {
//          if (subset.distinct.length > 1) {
//            val rest = termsExpanded.diff(subset)
//            // Take out factor from examined subset
//            // Factorise recursively
//            val fDivision = subset.map(x => x /^ currFactor).reduce(_ + _).toSum.get
//            val factorisedDivision = factoriseTerms(fDivision.asProds)
//            // Try to factorise the rest
//            var restDivision : Option[ArithExpr] = None
//            if (rest.distinct.length > 1) restDivision = factoriseTerms(rest.reduce(_ + _).toSum.get.asProds)
//            // See if we could factorise the two subexpressions and combine appropriately
//            (factorisedDivision,restDivision) match {
//              case (None,None) =>
//                val fTerm = currFactor*fDivision
//                if (rest.isEmpty) return Some(fTerm)
//                else {
//                  val restTerm = rest.reduce(_ + _)
//                  val combinedFactorisation = factoriseTerms(List(fTerm,restTerm))
//                  if (combinedFactorisation.isDefined) return combinedFactorisation
//                }
//
//              case (Some(_), None) =>
//                val fTerm = currFactor * factorisedDivision.get
//                if (rest.isEmpty) return Some(fTerm)
//                else {
//                  val restTerm = rest.reduce(_ + _)
//                  val combinedFactorisation = factoriseTerms(List(fTerm,restTerm))
//                  if (combinedFactorisation.isDefined) return combinedFactorisation
//                }
//
//              case (None, Some(_)) =>
//                val fTerm = currFactor * fDivision
//                if (rest.isEmpty) return Some(fTerm)
//                val combinedFactorisation = factoriseTerms(List(fTerm,restDivision.get))
//                if (combinedFactorisation.isDefined) return combinedFactorisation
//
//              case (Some(_), Some(_)) =>
//                val fTerm = currFactor * factorisedDivision.get
//                val combinedFactorisation = factoriseTerms(List(fTerm,restDivision.get))
//                if (combinedFactorisation.isDefined) return combinedFactorisation
//            }
//          }
//        }
//        i+=1
//      }
//    }
//    None
//  }

  // Factorises a product
  def factoriseProd(p: Prod): Option[ArithExpr] = {
    var accumExpr : ArithExpr = Cst(1)
    for (factor <- p.factors) factor match {
      case s:Sum =>
        val sFactored = Factorise(s)
        if (sFactored.isDefined) accumExpr *= sFactored.get
        else accumExpr *= s
      case _ => accumExpr *= factor
    }
    Some(accumExpr)
  }

  // Prepare array for factorisation using sieve algorithm
  private def sieve(n : Int) : Array[Int] = {
    val spf = Array.fill[Int](n+1)(0)
    var i = 2
    while(i * i <= n) {
      if (spf(i) == 0) {
        var k = i*i
        while (k <= n) {
          if (spf(k) == 0) spf(k) = i
          k += i
        }
      }
      i += 1
    }
    spf
  }

  // Gives prime decomposition of a positive integer
  // Repeated factors repeated in the product
  private def primeDecomposition(n : Int) : List[Int] = {
    var r = scala.math.abs(n)
    val spf = sieve(r)
    val factorisation = ListBuffer[Int]()
    if (n < 0) factorisation.addOne(-1)
    while(spf(r) > 0) {
      factorisation += spf(r)
      r /= spf(r)
    }
    factorisation += r
    factorisation.toList
  }

  private def findFactors(terms: List[ArithExpr]) : List[ArithExpr] = {
    val factors = ListBuffer[ArithExpr]()
    for (term <- terms) term match {
      case _:Var | _:Sum => if (!factors.contains(term)) factors += term
      case p:Pow =>
        if (p.e == 1 && !factors.contains(p.b)) factors += p.b
        if (p.e == -1 && !factors.contains(Pow(p.b,-1))) factors += Pow(p.b,-1)
      case p:Prod =>
        factors ++= findFactors(p.factors)
      case _ => // Do nothing
    }
    factors.distinct.toList
  }

  private def asProds(terms: List[ArithExpr]): List[ArithExpr] = {
    var prods = ListBuffer[ArithExpr]()
    for (t <- terms) t match {
      case p:Prod =>
        prods += p.primitiveProd
//      case c:Cst =>
//        if (c.value != 1) {
//          val primes = c.asProd.factors
//          for (prime <- primes) if (!prods.contains(t)) prods += prime
//        }
      case pow:Pow =>
        if (pow.asProdPows.isDefined) prods += pow.asProdPows.get.primitiveProd
        prods += pow.asProd.get

      case _ => prods += t
    }
    prods.toList
  }
}
