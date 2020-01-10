package arithmetic
package simplifier

object SimplifyIntDiv {

  def apply(numer : ArithExpr, denom : ArithExpr) : ArithExpr = simplifyIntDiv(numer,denom)

  def simplifyIntDiv(numer: ArithExpr, denom: ArithExpr): ArithExpr = (numer, denom) match {
    case (_, Cst(1)) => numer
    case (Cst(x), Cst(y)) if y != 0 => Cst(x/y)
    case (Cst(0),_) => Cst(0)
    // Return zero if the denominator is smaller than the numerator
    case (x, y) if ArithExpr.isSmaller(x, y).getOrElse(false) => Cst(0)
    // If there exists a common denominator, simplify
    case (x, y) if ComputeGCD(x, y) != Cst(1) =>
      val fac = ComputeGCD(x, y)
      (x /^ fac) / (y /^ fac)
    case (x,y) if !(x.min == ? || x.max == ? || y.min == ? || y.max == ?)  =>
      val numMin = x.min
      val numMax = x.max
      val denomMin = y.min
      val denomMax = y.max
      if ((numMin / denomMax).evalDouble ==(numMax / denomMin).evalDouble) {
        numMin / denomMax
      }
      else IntDiv(numer,denom)
    // (AE % div) / div = 0
    case (Mod(_, div1: ArithExpr), div2: ArithExpr) if div1 == div2 => Cst(0)
    // Pull out constant
    case (s@Sum(terms), c:Cst) if terms.collect({ case Cst(_) => }).nonEmpty =>
      val h = terms.head
      h/c + (s - h) / c
    // For sum numerator s, try to partition into s1 and s2 so that s1 is multiple of d in the constant case
    // or gcd(s1,d) != 1 (d is the denominator)
    case (s:Sum, _) =>
      val terms = s.terms
      val termSubsets = Factorise.powerSet(terms).filter(_.nonEmpty)
      if (termSubsets.nonEmpty) {
        for (subset <- termSubsets.tail) {
          val rest = terms.diff(subset)
          val sum = if (subset.length > 1) Sum(subset) else subset.head
          val gcd = ComputeGCD(sum, denom)
          if ((denom.isInstanceOf[Cst] && gcd % denom == Cst(0)) || (!denom.isInstanceOf[Cst] && gcd != Cst(1))) {
            if (rest.length > 1) return sum / denom + Sum(rest) / denom
            else return sum / denom + rest.head / denom
          }
        }
      }
      // Can't find a good partition
      IntDiv(numer,denom)
    // Flip division in the numerator
    case (IntDiv(numer, denom1), denom2) => numer / (denom1 * denom2)
    // Flip fractions in the denominator
    case (numer, Pow(base, -1)) => numer * base
    // Can't simplify
    case _ => IntDiv(numer,denom)
  }

}
