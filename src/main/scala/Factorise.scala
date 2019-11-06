import scala.collection.mutable.ListBuffer

object Factorise {
  // Factorises an expression
  def apply(e:ArithExpr) : Option[ArithExpr] = e match {
    case c:Cst =>
      val decomposition = primeDecomposition(c.value)
      Some(Prod(decomposition.map(x => Cst(x))))
    case s:Sum =>factoriseSum(s)
    case p:Prod => factoriseProd(p)
    case _ => None
  }

  def factoriseSum(s: Sum) : Option[ArithExpr] = {
    if (s.terms.length < 2) return None
    val factorisation = factoriseTerms(s.asProds)
    if (factorisation.isDefined) Some(factorisation.get.getSumProdFactorise.reduce((x,y) => x*y))
    else None
  }

//   Factorises a sum
//  private def factoriseTerms(terms : List[ArithExpr]) : Option[ArithExpr] = {
//    if (terms.length < 2) return None
//    var i = 0
//    val triedFactors = ListBuffer[ArithExpr]()
//    while (i < terms.length) {
//      val term = terms(i)
//      for (f <- term.getSumProdFactorise) {
//        if (!triedFactors.contains(f)) {
//          val containsF = ListBuffer[ArithExpr]()
//          for (t <- terms) {
//            if (t.getSumProdFactorise.contains(f)) containsF += t
//          }
//          for (subset <- powerSet(containsF)) {
//            if (subset.distinct.length > 1) {
//              val rest = terms.diff(subset)
//              val fDivision = subset.map(x => x /^ f).reduce((x,y) => x+y).toSum.get
//              val factorisedDivision = factoriseTerms(fDivision.asProds)
//              var restDivision : Option[ArithExpr] = None
//                if (rest.distinct.length > 1) {
//                  restDivision = factoriseTerms(rest.reduce((x,y)=>x+y).toSum.get.asProds)
//                }
//              (factorisedDivision,restDivision) match {
//                case (None,None) =>
//                  val fTerm = (f*fDivision)
//                  if (rest.isEmpty) return Some(fTerm)
//                  else {
//                    val combinedFactorisation = factoriseTerms(List(fTerm,Sum(rest)))
//                    if (combinedFactorisation.isDefined) return combinedFactorisation
//                  }
//
//                case (Some(_), None) =>
//                  val fTerm = (f * factorisedDivision.get)
//                  if (rest.isEmpty) return Some(fTerm)
//                  else {
//                    val combinedFactorisation = factoriseTerms(List(fTerm,Sum(rest)))
//                    if (combinedFactorisation.isDefined) return combinedFactorisation
//                  }
//
//                case (None, Some(_)) =>
//                  val fTerm = (f*fDivision)
//                  if (rest.isEmpty) return Some(fTerm)
//                  val combinedFactorisation = factoriseTerms(List(fTerm,restDivision.get))
//                  if (combinedFactorisation.isDefined) return combinedFactorisation
//
//                case (Some(_), Some(_)) =>
//                  val fTerm = (f * factorisedDivision.get)
//                  val combinedFactorisation = factoriseTerms(List(fTerm,restDivision.get))
//                  if (combinedFactorisation.isDefined) return combinedFactorisation
//              }
//            }
//          }
//          triedFactors += f
//        }
//      }
//      i+=1
//    }
//    None
//  }

  private def factoriseTerms(terms: List[ArithExpr]) : Option[ArithExpr] = {
    if (terms.length < 2) return None
    val factors = findFactors(terms)
    var i = 0
    // Loop over factors
    while (i < factors.length) {
      val currFactor = factors(i)
      // Find which terms does the variable appear in
      val containsF = ListBuffer[ArithExpr]()
      for (t <- terms) {
        if (ArithExpr.isMulitpleOf(t,currFactor)) containsF += t
      }
      // Common factor
      if (containsF.length == terms.length) {
        val simplified = terms.map(x => x /^ currFactor).reduce((x,y) => x+y)
        val simplifiedFactorisation = factoriseTerms(simplified.toSum.get.asProds)
        if (simplifiedFactorisation.isDefined) return Some(currFactor * simplifiedFactorisation.get)
        else return Some(currFactor*simplified)
      }
      // Compute subsets and rest
      // Recursively factorise
      else {
        if (!currFactor.isInstanceOf[Cst]) {
          val containsExpanded = expandTerms(containsF.toList)
          for (subset <- powerSet(containsExpanded)) {
            if (subset.distinct.length > 1) {
              val rest = expandTerms(terms).diff(subset)
              val fDivision = subset.map(x => x /^ currFactor).reduce((x,y) => x+y).toSum.get
              val factorisedDivision = factoriseTerms(fDivision.asProds)
              var restDivision : Option[ArithExpr] = None
              if (rest.distinct.length > 1) restDivision = factoriseTerms(rest.reduce((x,y)=>x+y).toSum.get.asProds)
              (factorisedDivision,restDivision) match {
                case (None,None) =>
                  val fTerm = currFactor*fDivision
                  if (rest.isEmpty) return Some(fTerm)
                  else {
                    val combinedFactorisation = factoriseTerms(List(fTerm,Sum(rest)))
                    if (combinedFactorisation.isDefined) return combinedFactorisation
                  }

                case (Some(_), None) =>
                  val fTerm = currFactor * factorisedDivision.get
                  if (rest.isEmpty) return Some(fTerm)
                  else {
                    val combinedFactorisation = factoriseTerms(List(fTerm,Sum(rest)))
                    if (combinedFactorisation.isDefined) return combinedFactorisation
                  }

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
    }
    None
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

  private def expandTerms(terms: List[ArithExpr]) : List[ArithExpr] = {
    val expanded = ListBuffer[ArithExpr]()
    for (term <- terms) term match {
      case p:Prod =>
        val expandCst = p.asNonCstFactorsSum
        if (expandCst.isDefined) {
          expanded ++= expandCst.get.asProds
        }
        else {
          expanded += p
        }
      case _ => expanded += term
    }
    expanded.toList
  }

  // Finds all primes up to (and including) n using sieve algorithm
  private def sieve(n : Int) : List[Int] = {
    val isPrime = Array.fill[Boolean](n+1)(true)
    isPrime(0) = false
    isPrime(1) = false
    for (i <- 2 to n) {
      if (isPrime(i) && i*i <= n) {
        for (j <- i*i to n by i) {
          isPrime(j) = false
        }
      }
    }
    val primes = ListBuffer[Int]()
    for (i <- 2 until n+1) {
      if (isPrime(i)) primes += i
    }
    primes.toList
  }

  // Gives prime decomposition of an integer
  // If n < 0, include -1 in the decomposition
  // Repeated factors repeated in the product
  private def primeDecomposition(n : Int) : List[Int] = {
    var r = scala.math.abs(n)
    val primes = sieve(r)
    val factorisation = ListBuffer[Int]()
    if (n < 0) factorisation += -1
    var i = 0
    var lessSquare = true
    while (i < primes.length && lessSquare) {
      val p = primes(i)
      if (p*p > r) lessSquare = false
      else {
        while(r % p == 0) {
          factorisation += p
          r/= p
        }
      }
      i+=1
    }

    if (r > 1) factorisation += r
    factorisation.toList
  }

  private def findFactors(terms: List[ArithExpr]) : List[ArithExpr] = {
    val factors = ListBuffer[ArithExpr]()
    for (term <- terms) term match {
      case _:Var | _:Sum | _:Cst => if (!factors.contains(term)) factors += term
      case p:Pow => if (!factors.contains(p.b)) factors += p.b
      case p:Prod =>
        factors ++= findFactors(p.factors)
      case _ => // Do nothing
    }
    factors.distinct.toList
  }

  @scala.annotation.tailrec
  private def gcdPair(first:Int, second:Int) : Int = {
    if (second == 0) first else gcdPair(second, first % second)
  }

  private def gcdList(nums: List[Int]) : Int = {
    nums.reduce((a,b) => gcdPair(a,b))
  }

  private def powerSet[A](xs: List[A]): List[List[A]] =
    xs.foldLeft(List(Nil: List[A]))((accum, elem) => accum.flatMap(l => Seq(l, elem :: l))).distinct

}
