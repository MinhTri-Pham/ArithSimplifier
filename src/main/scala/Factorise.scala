import scala.collection.mutable.ListBuffer

object Factorise {
  // Factorises an expression
  def apply(e:ArithExpr) : Option[Prod] = e match {
    case c:Cst =>
      val decomposition = primeDecomposition(c.value)
      Some(Prod(decomposition.map(x => Cst(x))))
    case s:Sum =>factoriseSum(s)
    case p:Prod => factoriseProd(p)
    case _ => None
  }

  def factoriseSum(s: Sum) : Option[Prod] = {
    if (s.terms.length < 2) return None
    val common = commonCst(s.terms)
    if (common.isDefined && common.get != Cst(1)) {
      val nonCst = (s / common.get).toSum.get
      val factorisation = factoriseTerms(nonCst.asProds)
      if (factorisation.isDefined) (common.get*factorisation.get.factors.reduce((x,y) => x*y)).toProd
      else (common.get*nonCst).toProd
    }
    else {
      val factorisation = factoriseTerms(s.asProds)
      if (factorisation.isDefined) factorisation.get.factors.reduce((x,y) => x*y).toProd
      else None
    }
  }

//   Factorises a sum
  private def factoriseTerms(terms : List[ArithExpr]) : Option[Prod] = {
    if (terms.length < 2) return None
    var i = 0
    val triedFactors = ListBuffer[ArithExpr]()
    while (i < terms.length) {
      val term = terms(i)
      for (f <- term.getSumProdFactorise) {
        if (!triedFactors.contains(f)) {
          val containsF = ListBuffer[ArithExpr]()
          for (t <- terms) {
            if (t.getSumProdFactorise.contains(f)) containsF += t
          }
          for (subset <- powerSet(containsF.toList)) {
            if (subset.distinct.length > 1) {
              val rest = terms.diff(subset)
              val fDivision = subset.map(x => x /^ f).reduce((x,y) => x+y).toSum.get
              val factorisedDivision = factoriseTerms(fDivision.asProds)
              var restDivision : Option[Prod] = None
              if (rest.distinct.length > 1) {
                restDivision = Factorise(correctList(rest).reduce((x,y)=>x+y))
              }
              (factorisedDivision,restDivision) match {
                case (None,None) =>
                  val fTerm = (f*fDivision).toProd.get
                  if (rest.isEmpty) return Some(fTerm)
                  else {
                    val combinedFactorisation = factoriseTerms(List(fTerm,Sum(rest)))
                    if (combinedFactorisation.isDefined) return combinedFactorisation
                  }

                case (Some(_), None) =>
                  val fTerm = (f * factorisedDivision.get).toProd.get
                  if (rest.isEmpty) return Some(fTerm)
                  else {
                    val combinedFactorisation = factoriseTerms(List(fTerm,Sum(rest)))
                    if (combinedFactorisation.isDefined) return combinedFactorisation
                  }

                case (None, Some(_)) =>
                  val fTerm = (f*fDivision).toProd.get
                  if (rest.isEmpty) return Some(fTerm)
                  val combinedFactorisation = factoriseTerms(List(fTerm,restDivision.get))
                  if (combinedFactorisation.isDefined) return combinedFactorisation

                case (Some(_), Some(_)) =>
                  val fTerm = (f * factorisedDivision.get).toProd.get
                  val combinedFactorisation = factoriseTerms(List(fTerm,restDivision.get))
                  if (combinedFactorisation.isDefined) return combinedFactorisation
              }
            }
          }
          triedFactors += f
        }
      }
      i+=1
    }
    None
  }

  // Factorises a product
  def factoriseProd(p: Prod): Option[Prod] = {
    var accumExpr : ArithExpr = Cst(1)
    for (factor <- p.factors) factor match {
      case s:Sum =>
        val sFactored = Factorise(s)
        if (sFactored.isDefined) accumExpr *= sFactored.get
        else accumExpr *= s
      case _ => accumExpr *= factor
    }
    accumExpr.toProd
  }

  // Multiples constant decompositions to a single constant
  private def correctList(exprs: List[ArithExpr]) : List[ArithExpr] = {
    val corrected = new ListBuffer[ArithExpr]
    for (expr <- exprs) expr match {
      case p:Prod => corrected += p.factors.reduce((x,y) =>x*y)
      case _ => corrected += expr
    }
    corrected.toList
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

  private def commonCst(terms : List[ArithExpr]) : Option[Cst] = {
    val constantTerms = ListBuffer[Int]()
    for (term <- terms) term match {
      case Cst(n) => constantTerms += n
      case p:Prod => constantTerms += Math.abs(p.cstFactor)
      case _ => return None
    }
    Some(Cst(gcdList(constantTerms.toList)))
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
