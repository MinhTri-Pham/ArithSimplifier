package arithmetic

object ComputeGCD {

  // GCD of two expressions
  def apply(a: ArithExpr, b: ArithExpr): ArithExpr = (a, b) match {
      // GCD of constants
      case (Cst(x), Cst(y)) => Cst(gcdInt(x, y))

      // GCD of two identical things is itself
      case (x, y) if x == y => x

      // GCD of powers
      // If bases same and exponents have same sign, GCD is b^(min(abs(e1),abs(e2))
      case (Pow(b1, e1), Pow(b2, e2)) if b1 == b2 && e1 > 0 && e2 > 0 =>
        if (e1 <= e2) a
        else b
      case (Pow(b1, e1), Pow(b2, e2)) if b1 == b2 && e1 < 0 && e2 < 0 =>
        if (e1 <= e2) b
        else a
      // If bases same but exponents have mixed sign, GCD is 1
      case (Pow(b1, _), Pow(b2, _)) if b1 == b2 => Cst(1)

      // Implicit pow 1
      case (Pow(ob, e), x) if ob == x && e > 1 => x // pow 1 (implicit)
      case (x, Pow(ob, e)) if ob == x  && e > 1 => x // pow 1 (implicit)
      case (Pow(ob, _), p:Prod) if p.factors.contains(ob) => ob // pow 1 (implicit)
      case (p:Prod, Pow(ob, _)) if p.factors.contains(ob) => ob // pow 1 (implicit)

      // GCD of two products:
      case (p1:Prod, p2:Prod) => (for {f1 <- p1.factors; f2 <- p2.factors} yield ComputeGCD(f1, f2)).reduce(_ * _)

      // GCD of constant and product: Look at constant factor of product
      case (c: Cst, p:Prod) => p.factors.find(_.isInstanceOf[Cst]) match {
        case Some(x) => ComputeGCD(c, x)
        case _ => Cst(1)
      }
      case (_:Prod, _: Cst) => ComputeGCD(b, a)

      case (p:Prod, x) if p.factors.contains(x)  => x
      case (x, p:Prod) if p.factors.contains(x)  => x

      // GCD of involving sums: try to factorise
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

      case (_, _: Sum) => ComputeGCD(b, a)

      case _ => Cst(1)
    }

  // GCD of list of ints
  def gcdIntList(terms: List[Int]): Int = {
    terms.length match {
      case 0 => throw new IllegalArgumentException
      case 1 => terms.head
      case _ => terms.foldLeft(terms.head)((x,y) => gcdInt(x,y))
    }
  }

  // GCD of two ints
  @scala.annotation.tailrec
  def gcdInt(x: Int, y: Int): Int = {
    if (y == 0) scala.math.abs(x) else gcdInt(scala.math.abs(y), scala.math.abs(x) % y)
  }

  // GCD of list of expressions
  def commonTermList(terms: List[ArithExpr]) : ArithExpr = {
    terms.foldLeft(terms.head)((x,y) => ComputeGCD(x,y))
  }
}
