package arithmetic

object ComputeGCD {

  // GCD of two expressions
  def apply(a: ArithExpr, b: ArithExpr): ArithExpr = (a, b) match {
      // GCD of constants
      case (Cst(x), Cst(y)) => Cst(gcdLong(x, y))

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

      // Implicit pow 1
      case (Pow(ob, e), x) if ob == x && e > 1 => x // pow 1 (implicit)
      case (x, Pow(ob, e)) if ob == x  && e > 1 => x // pow 1 (implicit)
      case (Pow(ob, _), p:Prod) if p.factors.contains(ob) => ob // pow 1 (implicit)
      case (p:Prod, Pow(ob, _)) if p.factors.contains(ob) => ob // pow 1 (implicit)

      // Pows of prods
      case (p1:Pow, p2:Pow) if p1.b.isInstanceOf[Prod] && p1.b.isInstanceOf[Prod] =>
        ComputeGCD(p1.asProdPows.get,p2.asProdPows.get)
      case (p:Pow, x) if p.b.isInstanceOf[Prod] => ComputeGCD(p.asProdPows.get,x)
      case (x, p:Pow) if p.b.isInstanceOf[Prod] => ComputeGCD(p.asProdPows.get,x)

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

      case (p:Prod, x) => p.factors.map(f => ComputeGCD(f,x)).reduce(_ * _)
      case (x, p:Prod) => p.factors.map(f => ComputeGCD(f,x)).reduce(_ * _)

      // GCD of involving sums: try to factorise
      case (s1: Sum, s2: Sum) =>
        val fac1 = s1.asProd
        val fac2 = s2.asProd
        if (fac1.isDefined && fac2.isDefined) ComputeGCD(fac1.get,fac2.get)
        else if (fac1.isDefined) ComputeGCD(fac1.get,s2)
        else if (fac2.isDefined) ComputeGCD(s1,fac2.get)
        else Cst(1)

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

  // GCD of list of longs
  def gcdLongList(terms: List[Long]): Long = {
    terms.length match {
      case 0 => throw new IllegalArgumentException
      case 1 => terms.head
      case _ => terms.foldLeft(terms.head)((x,y) => gcdLong(x,y))
    }
  }

  // GCD of two ints
  @scala.annotation.tailrec
  def gcdLong(x: Long, y: Long): Long = {
    if (y == 0) scala.math.abs(x) else gcdLong(scala.math.abs(y), scala.math.abs(x) % y)
  }

  // GCD of list of expressions
  def commonTermList(terms: List[ArithExpr]) : ArithExpr = {
    terms.foldLeft(terms.head)((x,y) => ComputeGCD(x,y))
  }

  def main(args: Array[String]): Unit = {
    val a = Var("a")
    val b = Var("b")
    val c = Var("c")
    val e1 = (a*c) pow 2
    val e2 = (a*b) pow 2
    println(ComputeGCD(e1,e2))
  }
}
