import scala.collection.mutable.ListBuffer

object Factorise {
  // Factorises an expression
  def apply(e:ArithExpr) : Option[Prod] = e match {
    case c:Cst =>
      val decomposition = primeDecomposition(c.value)
      Some(Prod(decomposition.map(x => Cst(x))))
    case s:Sum =>factoriseSum(s)
    case _ => None
  }

  def factoriseSum(s: Sum) : Option[Prod] = {
    if (s.terms.length < 2) return None
    val asProds = s.asProds
    val factorisation = factoriseTerms(asProds)
    if (factorisation.isDefined) factorisation.get.factors.reduce((x,y) => x*y).toProd
    else None
  }


  // Factorises a sum
  private def factoriseTerms(terms : List[ArithExpr]) : Option[Prod] = {
    if (terms.length < 2) return None
    var i = 0
    while (i < terms.length) {
      val term = terms(i)
      for (f <- term.getSumProdFactorise) {
        val containsF = ListBuffer[ArithExpr]()
        for (term <- terms) {
          if (term.getSumProdFactorise.contains(f)) containsF += term
        }
        for (subset <- powerSet(containsF.toList)) {
          if (subset.distinct.length > 1) {
            val rest = terms.diff(subset)
            val fDivision = subset.map(x => x /^ f)
            val factorisedDivision = factoriseTerms(fDivision)
            val restDivision = factoriseTerms(rest)
            (factorisedDivision,restDivision) match {
              case (None,None) =>
                var fTerm : Prod = Prod(List())
                val fDivSum = fDivision.reduce((x,y)=>x+y).toSum.get
                val fDivSumFactorised = factoriseTerms(fDivSum.asProds)
                if (fDivSumFactorised.isDefined) fTerm = (f * fDivSumFactorised.get).toProd.get.primitiveProd
                else fTerm = (f * fDivSum).toProd.get

                if (rest.isEmpty) return Some(fTerm)
                else {
                  val restTerm = Sum(rest)
                  val combinedFactorisation = factoriseTerms(List(fTerm,restTerm))
                  if (combinedFactorisation.isDefined) return combinedFactorisation
                }

              case (Some(_), None) =>
                val fTerm = (f * factorisedDivision.get).toProd.get.primitiveProd
                if (rest.isEmpty) return Some(fTerm)
                else {
                  val restTerm = Sum(rest)
                  val combinedFactorisation = factoriseTerms(List(fTerm,restTerm))
                  if (combinedFactorisation.isDefined) return combinedFactorisation
                }

              case (None, Some(_)) =>
                var fTerm : Prod = Prod(List())
                val fDivSum = fDivision.reduce((x,y)=>x+y).toSum.get
                val fDivSumFactorised = factoriseTerms(fDivSum.asProds)
                if (fDivSumFactorised.isDefined) fTerm = (f * fDivSumFactorised.get).toProd.get.primitiveProd
                else fTerm = (f * fDivSum).toProd.get

                val restTerm = restDivision.get
                val combinedFactorisation = factoriseTerms(List(fTerm,restTerm))
                if (combinedFactorisation.isDefined) return combinedFactorisation

              case (Some(_), Some(_)) =>
                val fTerm = (f * factorisedDivision.get).toProd.get.primitiveProd
                val restTerm = restDivision.get
                val combinedFactorisation = factoriseTerms(List(fTerm,restTerm))
                if (combinedFactorisation.isDefined) return combinedFactorisation
            }
          }
        }
      }
      i+=1
    }
    None
  }

  // Multiplies remaining products of prime constants in factorisation of a sum into a single constant
  // Everything else unchanged
  private def correctConstants(factorisation: Prod) : Option[Prod] = {
    var cleanedFactors = ListBuffer[ArithExpr]()
    for (f <- factorisation.factors) f match {
      case s:Sum =>
        val cleanedTerms = ListBuffer[ArithExpr]()
        for (t <- s.terms) t match {
            case p:Prod => cleanedTerms += p.factors.reduce((x,y) =>x*y)
          case _ => cleanedTerms += t
        }
        cleanedFactors += Sum(cleanedTerms.toList.sortWith(ArithExpr.isCanonicallySorted))
      case _ => cleanedFactors += f
    }
    Some(Prod(cleanedFactors.toList))
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

  private def powerSet[A](xs: List[A]): List[List[A]] =
    xs.foldLeft(List(Nil: List[A]))((accum, elem) => accum.flatMap(l => Seq(l, elem :: l)))

  def main(args: Array[String]): Unit = {
    val a = Var("a")
    val b = Var("b")
    val s = a*a*a + a*a*b + a*a*b + a*a*b + a*b*b + a*b*b + a*b*b + b*b*b
    println(s)
    println(Factorise(s).get.toPow.get)
//    val s1 = a*b*(a+b)
//    val s2 = (b*(a+b)*(a+b)).toProd.get.primitiveProd
//    println(factoriseTerms(List(s1,s2)))
  }
}
