package arithmetic

import scala.collection.mutable.ListBuffer

object Factorise {

  var numFactorsTried = 0

  // Factorises an expression
  def apply(e:ArithExpr) : Option[ArithExpr] = e match {
    case s:Sum =>
      if (s.terms.length < 2) return None
      factorise(s.terms)
    case _ => None
  }

  private def factorise(terms: List[ArithExpr]) : Option[ArithExpr] = {
    if (terms.length < 2) return None
    val gcd = ComputeGCD.commonTermList(terms)
    // Look for common factor
    if (gcd != Cst(1)) {
      val simplified = terms.map(x => x /^ gcd).reduce(_ + _)
      val simplifiedFactorisation = factorise(simplified.getTerms)
      // Try to factorise the simplified expression
      if (simplifiedFactorisation.isDefined) return Some(gcd * simplifiedFactorisation.get)
      Some(gcd*simplified)
    }
    else {
      val factors = findFactorSet(terms)
      for (currFactor <- factors) {
        val containsF = ListBuffer[ArithExpr]()
        for (t <- terms) {
          if (ArithExpr.isMultipleOf(t,currFactor)) containsF += t
        }
        if (containsF.length > 1) {
          numFactorsTried += 1
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
              val subsetDivision = subset.map(x => x /^ currFactor).reduce(_ + _)
              val subsetDivisionFactorised = factorise(subsetDivision.getTerms)
              // Try to factorise the rest
              var restFactorised : Option[ArithExpr] = None
              if (rest.distinct.length > 1) {
                val restTerm = rest.reduce(_ + _).getTerms
                restFactorised = factorise(restTerm)
              }
              // See if we could factorise the two subexpressions and combine appropriately
              (subsetDivisionFactorised,restFactorised) match {
                case (None,None) =>
                  val fTerm = currFactor*subsetDivision
                  if (rest.isEmpty) return Some(fTerm)
                  val restTerm = rest.reduce(_ + _)
                  val combinedFactorisation = factorise(List(fTerm,restTerm))
                  if (combinedFactorisation.isDefined) return combinedFactorisation

                case (Some(_), None) =>
                  val subsetFactorised = currFactor * subsetDivisionFactorised.get
                  if (rest.isEmpty) return Some(subsetFactorised)
                  val restTerm = rest.reduce(_ + _)
                  val combinedFactorisation = factorise(List(subsetFactorised,restTerm))
                  if (combinedFactorisation.isDefined) return combinedFactorisation

                case (None, Some(_)) =>
                  val fTerm = currFactor * subsetDivision
                  if (rest.isEmpty) return Some(fTerm)
                  val combinedFactorisation = factorise(List(fTerm,restFactorised.get))
                  if (combinedFactorisation.isDefined) return combinedFactorisation

                case (Some(_), Some(_)) =>
                  val fTerm = currFactor * subsetDivisionFactorised.get
                  val combinedFactorisation = factorise(List(fTerm,restFactorised.get))
                  if (combinedFactorisation.isDefined) return combinedFactorisation
              }
            }
          }
        }
      }
      None
    }
  }

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

  def findFactorSet(terms: List[ArithExpr]) : Set[ArithExpr] = {
    var factors = Set[ArithExpr]()
    for (term <- terms) term match {
      case _:Var | _:Sum =>  factors += term
      case p:Pow if p.e > 1 =>
        if (p.asProdPows.isDefined) factors ++= findFactorSet(p.asProdPows.get.factors)
        factors += p.b
      case p:Pow if p.e <= -1 =>
        if (p.asProdPows.isDefined) factors ++= findFactorSet(p.asProdPows.get.factors)
        factors += Pow(p.b,-1)
      case p:Prod => factors ++= findFactorSet(p.factors)
      case _ => // Do nothing
    }
    factors
  }
}
