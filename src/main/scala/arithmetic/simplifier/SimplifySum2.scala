package arithmetic
package simplifier

object SimplifySum2 {

  /**
   * Tries to simplify two expressions where one is a Sum by adding the new term to
   * the list of terms of the existing sum.
   * First, it tries to merge the new term from rhs with one of the lhs terms
   * from the list. If that fails, the lhs is reconstructed as a single expression; it then tries to
   * simplify reconstructed lhs with the new term from rhs using combineTerms.
   * The difference between addTerm and combineTerms is that addTerm handles cases with Sum as lhs,
   * and combineTerm doesn't.
   *
   * @param terms            A list of left hand-side (lhs) terms
   * @param term             A new term from right hand-side expression
   * @param termsComeFromSum A flag indicating whether lhs is originally a Sum and not another arithmetic
   *                         expression temporarily represented as a sum using Sum extractors
   * @return                 A simplified expression as Right if simplification was successful; a Sum object where
   *                         the new term is appended to the unchanged list of old terms as Left otherwise
   */
  def addTerm(terms: List[ArithExpr], term: ArithExpr,
              termsComeFromSum: Boolean = false):
  Either[ArithExpr, ArithExpr] = {
    terms.zipWithIndex.foreach {
      case (x, i) =>
        val newterm = combineTerms(term, x)
        if (newterm.isDefined) return Right(replaceAt(i, newterm.get, terms).reduce(_ + _))
    }

    // We didn't manage to combine the new term with any of the old terms.
    val simplifiedOriginalSum: ArithExpr =
      if (terms.length > 1) {
        if (termsComeFromSum)  Sum(terms)
        else {
          // TODO: investigate whether we can reconstruct the first expression without
          //  simplification even if it is not a Sum
          SimplifySum2(terms)
        }
      } else terms.head
    // Try to combine `term` with sum of `terms`
    combineTerms(simplifiedOriginalSum, term) match {
      case Some(simplifiedResult) => Right(simplifiedResult)
      // If simplified combination is not possible, it is safe to just prepend the term to `terms`
      // for a simplified sum
      case None =>
        Left(Sum((term +: terms).sortWith(ArithExpr.isCanonicallySorted)))
    }
  }

  /**
   * Try to combine a pair of terms.
   * If one or both of the terms are Sums, no Sum-specific simplifications are applied;
   * addTerms takes care of that. If one or both of the terms are Sums that can be represented
   * as something else using extractors, then simplification is possible.
   *
   * @param lhs The first term
   * @param rhs The second term
   * @return An option containing an expression if the terms can be combined, None otherwise
   */
  def combineTerms(lhs: ArithExpr, rhs: ArithExpr):
  Option[ArithExpr] = {
    (lhs, rhs) match {

      case (arithmetic.?, _) | (_, arithmetic.?) => Some(arithmetic.?)


      case (Cst(x), Cst(y)) => Some(Cst(x + y))
      case (Cst(0), _) => Some(rhs)
      case (_, Cst(0)) => Some(lhs)


      // Modulo Identity: a = a / b * b + a % b
      case (Prod(factors), _@Mod(a, b)) if factors.reduce(_*_) == (a / b) * b => Some(a)
      case (_@Mod(a, b), Prod(factors)) if factors.reduce(_*_) == (a / b) * b => Some(a)

      // Avoid duplicates in the term list
      case (x, y) if x == y => Some(Cst(2) * x)

      case x =>
        val termsAsProds = x match {
          /* First, convert lhs and rhs to a list of factors */
          // Try to factorise in hope that the factorised sum will be simpler
          case (Prod(fs1), Prod(fs2)) => Some((fs1, fs2))

          // Merge products if they only differ by a constant fac?tor
          //      case (x, p: Prod) if p.withoutFactor(p.cstFactor) == x => Some(x * (p.cstFactor + 1))
          case (ox, Prod(fs2)) => Some((List(ox), fs2))

          //      case (p: Prod, x) if p.withoutFactor(p.cstFactor) == x => Some(x * (p.cstFactor + 1))
          case (Prod(fs1), ox) => Some((fs1, List(ox)))
          case _ => None
        }

        termsAsProds match {
          case Some((factors1, factors2)) => simplifiableByFactorisation(factors1, factors2)
          case None => None
        }
    }
  }



  /**
   * Try to factorise a sum of products in hope that the sum without common factors can be simplified
   */
  def simplifiableByFactorisation(term1factors: List[ArithExpr],
                                  term2factors: List[ArithExpr]): Option[ArithExpr] = {
    getCommonFactors(term1factors, term2factors) match {
      case (Cst(1), Nil) => None
      case (cstCommonFactor, nonCstCommonFactors) =>

        val (term1CstFactor, term1NonCstFactors) = Prod.partitionFactorsOnCst(term1factors, simplified = true)
        val (term2CstFactor, term2NonCstFactors) = Prod.partitionFactorsOnCst(term2factors, simplified = true)

        val term1WithoutCommonFactors = SimplifyProd2(Cst(term1CstFactor.value / cstCommonFactor.value) +: Prod.removeFactors(term1NonCstFactors, nonCstCommonFactors))
        val term2WithoutCommonFactors = SimplifyProd2(Cst(term2CstFactor.value / cstCommonFactor.value) +: Prod.removeFactors(term2NonCstFactors, nonCstCommonFactors))

        val commonFactors = cstCommonFactor match {
          case Cst(1) => nonCstCommonFactors
          case _ => cstCommonFactor +: nonCstCommonFactors
        }

        // Here even if we fail to simplify, we might convert into normal form using asPowOfSum inside simplify
        simplify(term1WithoutCommonFactors, term2WithoutCommonFactors) match {

          case Right(simplifiedSumWithoutCommonFactors) =>
            // Remaining sum has been collapsed (simplified)
            Some(simplifiedSumWithoutCommonFactors * SimplifyProd2(commonFactors))

          case Left(nonSimplifiedSumWithoutCommonFactors) =>
            // No simplification of remaining sum is possible, but we might have converted into normal form
            // (e.g. using asPowOfSum)
            // Check if it can be simplified by multiplying it by each common factor
            // We prevent infinite loop here using a flag preventing us from distributing that will lead us back here
            var simplificationAchieved: Boolean = false

            val possiblySimplifiedCommonFactors = commonFactors.map(commonFactor =>
              if (!simplificationAchieved) {
                SimplifyProd2.combineFactors(nonSimplifiedSumWithoutCommonFactors, commonFactor,
                  distributionAllowed = false) match {
                  case Some(simplifiedExpr) =>
                    simplificationAchieved = true
                    simplifiedExpr

                  case None => commonFactor
                }
              } else commonFactor
            )

            if (simplificationAchieved) Some(possiblySimplifiedCommonFactors.reduce(_ * _)) else None
        }
    }
  }

  /**
   * Get non-constant and constant common factors from factors of two simplified Prods
   * @param factors1 First product.
   * @param factors2 Second product
   * @return A tuple of constant and non-constant common factors
   */
  def getCommonFactors(factors1: List[ArithExpr], factors2: List[ArithExpr]):
  (Cst, List[ArithExpr]) = {
    val (cstFactor1, nonCstFactors1) = Prod.partitionFactorsOnCst(factors1, simplified = true)

    getCommonFactors(cstFactor1, nonCstFactors1, factors2)
  }

  /**
   * Get non-constant and constant common factors from factors of two simplified Prods where the first
   * product is already factorised into constant and non-constant factors.
   * This version of getCommonFactors was added for performance, to avoid factorising the first product for
   * the second time.
   *
   * @param cstFactor1 Constant factor of the first product.
   * @param nonCstFactors1 Non-constant factors of the first product.
   * @param factors2 Second product
   * @return A tuple of constant and non-constant common factors
   */
  def getCommonFactors(cstFactor1: Cst, nonCstFactors1: List[ArithExpr],
                       factors2: List[ArithExpr]):
  (Cst, List[ArithExpr]) = {
    val (cstFactor2, nonCstFactors2) = Prod.partitionFactorsOnCst(factors2, simplified = true)

    val cstFactors: List[Int] = List(cstFactor1.value, cstFactor2.value)

    val cstCommonFactor = ComputeGCD.gcdLong(cstFactors)

    (Cst(cstCommonFactor), nonCstFactors1.intersect(nonCstFactors2))
  }


  /**
   * Try to simplify a sum of two terms. Indicate whether simplification was achieved by returning
   * either Left or Right.
   * For usages where we have to obtain the result of summing two expressions and know whether
   * the result was simplified or two expressions were just packaged into a Sum.
   *
   * @param lhs First term.
   * @param rhs Second term.
   * @return An arithmetic expression as Right if simplification was achieved; a Sum instance with
   *         lhs and rhs as terms as Left otherwise.
   */
  def simplify(lhs: ArithExpr, rhs: ArithExpr): Either[ArithExpr, ArithExpr] = {
    var simplified: Boolean = false


    /**
     * Unwrap the result of simplification attempt from Either into ArithExpr and
     * update the `simplified` flag if the result was Right
     */
    def updateStatus(simplificationResult: Either[ArithExpr, ArithExpr]):
    ArithExpr = simplificationResult match {
      case Right(simplifiedExpr) =>
        simplified = true
        simplifiedExpr
      case Left(nonSimplifiedExpr) => nonSimplifiedExpr
    }

    // Try to simplify if one or both expressions are sums or can be represented as such.
    val termWiseSimplifiedExpr: ArithExpr = (lhs, rhs) match {
      case (s1: Sum, s2: Sum) => s2.terms.foldLeft[ArithExpr](s1)(
        (acc, s2term) =>            updateStatus(simplify(acc, s2term)))
      case (s1: Sum, s2@Sum(_)) => s1.terms.foldLeft[ArithExpr](s2)(
        (acc, s1term) =>            updateStatus(simplify(acc, s1term)))
      case (_@Sum(_), _: Sum) =>  updateStatus(simplify(rhs, lhs))

      case (_@Sum(lhsTerms), Sum(rhsTerms)) =>
        lhsTerms.tail.foldLeft[ArithExpr](updateStatus(
          addTerm(rhsTerms, lhsTerms.head, termsComeFromSum = lhs.isInstanceOf[Sum])))(
          (acc, lhsTerm) =>         updateStatus(addTerm(List(acc), lhsTerm, termsComeFromSum = true)))

      case (Sum(lhsTerms), _) =>    updateStatus(addTerm(lhsTerms, rhs, termsComeFromSum = lhs.isInstanceOf[Sum]))
      case (_, Sum(rhsTerms)) =>    updateStatus(addTerm(rhsTerms, lhs, termsComeFromSum = rhs.isInstanceOf[Sum]))
      case _ =>                     updateStatus(addTerm(List(lhs), rhs))
    }

    if (simplified) Right(termWiseSimplifiedExpr)
    else Left(termWiseSimplifiedExpr)
  }


  /**
   * Try to simplify the sum into another expression if simplification is enabled
   *
   * @param lhs The left-hand side.
   * @param rhs The right-hand side.
   * @return A promoted expression or a simplified sum object.
   */
  def apply(lhs: ArithExpr, rhs: ArithExpr): ArithExpr = {
    if (PerformSimplification())
      simplify(lhs, rhs) match {
        case Right(simplifiedExpr) => simplifiedExpr
        case Left(nonSimplifiedExpr) => nonSimplifiedExpr
      }
    else
      Sum(List(lhs, rhs).sortWith(ArithExpr.isCanonicallySorted))
  }

  /**
   * Try to simplify the sum into another expression.
   *
   * @param terms The terms of the sum to simplify.
   * @return A promoted expression or a simplified sum object.
   */
  def apply(terms: List[ArithExpr]): ArithExpr = {
    if (terms.length > 1) terms.reduce(_ + _)
    else terms.head
  }

  def main(args: Array[String]): Unit = {
    val a = Var("a")
    val b = Var("b")
    val expr = a /^ (a+b) + Cst(1) /^ (a+b) + b /^ (a+b)
    println(expr)
  }
}
