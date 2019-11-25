object ComputeGCD {

  def apply(a: ArithExpr, b: ArithExpr): ArithExpr = {
    val g: ArithExpr = (a, b) match {
      // GCD of constants
      case (Cst(x), Cst(y)) => Cst(gcdInt(x, y))

      // GCD of two identical things is itself
      case (x, y) if x == y => x

      // GCD of powers, go through bases and find a match, return smallest exp
      // TODO: handle negative exp
      case (Pow(_, x), _) if x < 0 => Cst(1)
      case (_, Pow(_, x)) if x < 0 => Cst(1)
      case (x, Pow(ob, _)) if ob == x => x // pow 1 (implicit)
      case (Pow(b1, e1), Pow(b2, e2)) if b1 == b2 && (e1 <= e2) => b1 pow e1
      case (Pow(b1, _), Pow(b2, e2)) if b1 == b2 => b1 pow e2
      case (Pow(ob, _), Prod(factors)) if factors.contains(ob) => ob // pow 1 (implicit)
      case (Prod(factors), Pow(ob, _)) if factors.contains(ob) => ob // pow 1 (implicit)
      case (Pow(ob, _), x) if ob == x => x // pow 1 (implicit)
      case (x, Pow(ob, _)) if ob == x => x // pow 1 (implicit)

      // GCD of products: find GCD in factor pairs
      case (Prod(fs1), Prod(fs2)) => (for {f1 <- fs1; f2 <- fs2} yield ComputeGCD(f1, f2)).reduce(_ * _)
      case (Prod(f), c: Cst) => ComputeGCD(b, a)
      case (c: Cst, Prod(f)) => f.find(_.isInstanceOf[Cst]) match {
        case Some(x) => ComputeGCD(c, x)
        case _ => Cst(1)
      }
      case (Prod(f), x) if f.contains(x) => x
      case (x, Prod(f)) if f.contains(x) => x

      // GCD of sums: find common factor across all terms
      // If new factorization of sum is enabled, then Sum.asProd inside Prod.unapply() is enabled and handles the sums
      // Otherwise, we have to handle sums here
      case (s1: Sum, s2: Sum) =>
        // Compute the common factors
        val fac1 = factorizeSum(s1)
        if (fac1 == Cst(1)) return Cst(1)
        val fac2 = factorizeSum(s2)
        if (fac2 == Cst(1)) return Cst(1)

        // The GCD could be either the factor or the remainder, so we compute the intersection
        val common = List(fac1, s1 /^ fac1).intersect(List(fac2, s2 /^ fac2))
        if (common.isEmpty) Cst(1)
        else common.head

      case (_, _: Sum) => ComputeGCD(b, a)
      case (s: Sum, x) =>
        // Factorise s
        val factorAttempt = Factorise(s)
        var factor : ArithExpr = s
        if (factorAttempt.isDefined) factor = factorAttempt.get
        // If there is none, there is no possible common factor
        if (factor == Cst(1)) factor
        // otherwise see if there is a common factor with the sum's terms
        ComputeGCD(factor,x)

      case _ => Cst(1)
    }
    // Never factorize by a fraction
    g match {
      case Pow(_, -1) => Cst(1)
      case x => x
    }
  }

  // Factorize a sum: find a factor common to all terms
  private def factorizeSum(s: Sum): ArithExpr = {
    factorizeList(s.terms)
  }

  def factorizeList(terms: List[ArithExpr]): ArithExpr = {
    val fac = for {
      t1 <- terms
      t2 <- terms
    } yield ComputeGCD(t1, t2)

    if (fac.isEmpty) Cst(1)
    else fac.reduce((l, r) => ComputeGCD(l, r))
  }

  def gcdLong(terms: List[Int]): Int = {
    terms.length match {
      case 0 => throw new IllegalArgumentException
      case 1 => terms.head
      case _ => terms.foldLeft(terms.head)((x,y) => gcdInt(x,y))
    }
  }

  @scala.annotation.tailrec
  def gcdInt(x: Int, y: Int): Int = {
    if (y == 0) scala.math.abs(x) else gcdInt(scala.math.abs(y), scala.math.abs(x) % y)
  }

}
