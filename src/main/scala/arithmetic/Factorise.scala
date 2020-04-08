package arithmetic

import scala.collection.mutable.ListBuffer

object Factorise {

  var numFactorsTried = 0 // Number of factors tried - for evaluation

  def apply(e:ArithExpr) : Option[ArithExpr] = e match {
    case s:Sum =>
      if (s.terms.length < 2) return None
      factorise(s.terms)
    case _ => None
  }

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
      for (currFactor <- factors) {
        val containsF = ListBuffer[ArithExpr]() // Terms that are multiples of factor being explored
        for (t <- terms) {
          if (ArithExpr.isMultipleOf(t,currFactor)) containsF += t
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
              val subsetDivision = subset.map(x => x /^ currFactor).reduce(_ + _)
              val subsetDivisionFactorised = factorise(subsetDivision.getTerms)
              // Factorise the sum given by remaining terms
              var restFactorised : Option[ArithExpr] = None
              if (rest.distinct.length > 1) {
                val restTerm = rest.reduce(_ + _).getTerms
                restFactorised = factorise(restTerm)
              }
              // Combine the two obtained factorisations if they exist
              (subsetDivisionFactorised,restFactorised) match {
                case (None,None) =>
                  val fTerm = currFactor*subsetDivision
                  val restTerm = rest.reduce(_ + _)
                  val combinedFactorisation = factorise(List(fTerm,restTerm))
                  if (combinedFactorisation.isDefined) return combinedFactorisation

                case (Some(_), None) =>
                  val subsetFactorised = currFactor * subsetDivisionFactorised.get
                  val restTerm = rest.reduce(_ + _)
                  val combinedFactorisation = factorise(List(subsetFactorised,restTerm))
                  if (combinedFactorisation.isDefined) return combinedFactorisation

                case (None, Some(_)) =>
                  val fTerm = currFactor * subsetDivision
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

  def main(args: Array[String]): Unit = {
    val a = Var("a")
    val b = Var("b")
    val c = Var("c")
    val e1 = a*b*c
    val e2 = a*b
    println(ArithExpr.isMultipleOf(e1,e2))
  }
}
