package arithmetic

object ComputeGCD {

  def apply(a: ArithExpr, b: ArithExpr): ArithExpr = (a, b) match {
      // GCD of constants
      case (Cst(x), Cst(y)) => Cst(gcdInt(x, y))

      // GCD of two identical things is itself
      case (x, y) if x == y => x

      // GCD of powers, go through bases and find a match, return smaller exp in absolute value
      case (x, Pow(ob, _)) if ob == x => x // pow 1 (implicit)
      case (Pow(b1, e1), Pow(b2, e2)) if b1 == b2 && e1 > 0 && e2 > 0 =>
        if (e1 <= e2) a
        else b
      case (Pow(b1, e1), Pow(b2, e2)) if b1 == b2 && e1 < 0 && e2 < 0 =>
        if (e1 <= e2) b
        else a
      case (Pow(b1, _), Pow(b2, e2)) if b1 == b2 => b1 pow e2
      case (Pow(ob, _), Prod(factors)) if factors.contains(ob) => ob // pow 1 (implicit)
      case (Prod(factors), Pow(ob, _)) if factors.contains(ob) => ob // pow 1 (implicit)
      case (Pow(ob, _), x) if ob == x => x // pow 1 (implicit)
      case (x, Pow(ob, _)) if ob == x => x // pow 1 (implicit)

      // GCD of products: find GCD in factor pairs
      case (Prod(fs1), Prod(fs2)) if fs1.intersect(fs2).isEmpty => (for {f1 <- fs1; f2 <- fs2} yield ComputeGCD(f1, f2)).reduce(_ * _)
      case (Prod(fs1), Prod(fs2)) =>
        val common = fs1.intersect(fs2).reduce(_*_)
        val gcdNonCommon = ComputeGCD(a /^ common, b /^ common)
        common * gcdNonCommon
      case (_:Prod, _: Cst) => ComputeGCD(b, a)
      case (c: Cst, Prod(f)) => f.find(_.isInstanceOf[Cst]) match {
        case Some(x) => ComputeGCD(c, x)
        case _ => Cst(1)
      }
      case (Prod(f), x) if f.contains(x)  => x
      case (x, Prod(f)) if f.contains(x)  => x

      // GCD of sums: try to factorise
      case (s1: Sum, s2: Sum) =>
        val fac1 = s1.asProd
        val fac2 = s2.asProd
        if (fac1.isDefined && fac2.isDefined) {
          ComputeGCD(fac1.get,fac2.get)
        }
        else if (fac1.isDefined) {
          ComputeGCD(fac1.get,s2)
        }
        else if (fac2.isDefined) {
          ComputeGCD(s1,fac2.get)
        }
        else {
          Cst(1)
        }

      case (_, _: Sum) => ComputeGCD(b, a)
      case (s: Sum, x) =>
        // Factorise s
        val factorAttempt = s.asProd
        var factor : ArithExpr = s
        // If can factorise, try on the resulting factorisation instead of the sum
        if (factorAttempt.isDefined) {
          factor = factorAttempt.get
          ComputeGCD(factor,x)
        }
        // Can't factorise so gcd must be 1
        else Cst(1)


      case _ => Cst(1)
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

  def commonTermList(terms: List[ArithExpr]) : ArithExpr = {
    terms.foldLeft(terms.head)((x,y) => ComputeGCD(x,y))
  }
}
