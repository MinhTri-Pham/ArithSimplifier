package arithmetic

import scala.collection.mutable.ListBuffer

object Factorise {

  var numFactorsTried = 0 // Number of factors tried - for evaluation

  // Currently only interested in factorising a sum
  def apply(e:ArithExpr) : Option[ArithExpr] = e match {
    case s:Sum =>
      if (s.terms.length < 2) return None
      factorise(s.terms)
    case _ => None
  }

  /**
   * Factorise a sum given its terms
   *
   * @param terms   Terms of the sum to factorise
   * @return        An option containing the factorisation if the corresponding sum can be factorised, None otherwise
   */
  private def factorise(terms: List[ArithExpr]) : Option[ArithExpr] = {
    if (terms.length < 2) return None
    val gcd = ComputeGCD.commonTermList(terms)
    if (gcd != Cst(1)) {
      // There's a common factor, solve a simpler problem
      val simplified = terms.map(x => x /^ gcd).reduce(_ + _)
      val simplifiedFactorisation = factorise(simplified.getTerms)
      if (simplifiedFactorisation.isDefined) return Some(gcd * simplifiedFactorisation.get)
      Some(gcd*simplified)
    }
    else {
      val factors = findFactorSet(terms) // Factors to be explored
      for (factor <- factors) {
        val containsF = ListBuffer[ArithExpr]()
        // Find terms that are multiples of factor being explored
        for (t <- terms) {
          if (ArithExpr.isMultipleOf(t,factor)) containsF += t
        }
        if (containsF.length > 1) {
          numFactorsTried += 1
          // Expands scalar multiples
          val containsExpanded = Helper.expandTerms(containsF.toList)
          val termsExpanded = Helper.expandTerms(terms)
          // Iterate subsets of containsF
          for (subset <- Helper.powerSet(containsExpanded)) {
            if (subset.distinct.length > 1) {
              val rest = termsExpanded.diff(subset)
              // Divide terms in subset by factor explored and factorise sum given by new terms
              val subsetDivision = subset.map(x => x /^ factor).reduce(_ + _)
              val subsetDivisionFactorised = factorise(subsetDivision.getTerms)
              // Factorise the sum given by remaining terms
              var restFactorised : Option[ArithExpr] = None
              if (rest.distinct.length > 1) {
                val restTerms = rest.reduce(_ + _).getTerms
                restFactorised = factorise(restTerms)
              }
              (subsetDivisionFactorised,restFactorised) match {
                case (None,None) =>
                  val subsetFactorised = factor*subsetDivision // Factorisation of subset
                  val restTerm = rest.reduce(_ + _)
                  // Try to combine factorisation of subset and rest
                  val combinedFactorisation = factorise(List(subsetFactorised,restTerm))
                  if (combinedFactorisation.isDefined) return combinedFactorisation

                case (Some(_), None) =>
                  val subsetFactorised = factor * subsetDivisionFactorised.get // Factorisation of subset
                  val restTerm = rest.reduce(_ + _)
                  // Try to combine factorisation of subset and rest
                  val combinedFactorisation = factorise(List(subsetFactorised,restTerm))
                  if (combinedFactorisation.isDefined) return combinedFactorisation

                case (None, Some(_)) =>
                  val subsetFactorised = factor * subsetDivision // Factorisation of subset
                  // Try to combine factorisation of subset and rest
                  val combinedFactorisation = factorise(List(subsetFactorised,restFactorised.get))
                  if (combinedFactorisation.isDefined) return combinedFactorisation

                case (Some(_), Some(_)) =>
                  val subsetFactorised = factor * subsetDivisionFactorised.get // Factorisation of subset
                  // Try to combine factorisation of subset and rest
                  val combinedFactorisation = factorise(List(subsetFactorised,restFactorised.get))
                  if (combinedFactorisation.isDefined) return combinedFactorisation
              }
            }
          }
        }
      }
      None // Exhausted all factors so factorisation doesn't exist
    }
  }


  /**
   * Find a set of factors to be explored by factorisation algorithm above
   *
   * @param terms   Terms of the sum to factorise
   * @return        Set of factors to explore for factorisation
   */
  def findFactorSet(terms: List[ArithExpr]) : Set[ArithExpr] = {
    var factors = Set[ArithExpr]()
    for (term <- terms) term match {
      // Add all variables and possibly sum factors of product term
      case _:Var | _:Sum =>  factors += term
      // Positive power
      case p:Pow if p.e > 1 =>
        // If power can be written as product of powers, consider these
        if (p.asProdPows.isDefined) factors ++= findFactorSet(p.asProdPows.get.factors)
        factors += p.b // Add base
      // Negative power
      case p:Pow if p.e <= -1 =>
        // If power can be written as product of powers, consider these
        if (p.asProdPows.isDefined) factors ++= findFactorSet(p.asProdPows.get.factors)
        factors += Pow(p.b,-1) // Add base^{-1}
      // Product, consider its factors
      case p:Prod => factors ++= findFactorSet(p.factors)
      case _ => // Do nothing
    }
    factors
  }
}
