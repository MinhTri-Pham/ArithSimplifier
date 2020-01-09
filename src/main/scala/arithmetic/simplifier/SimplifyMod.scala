package arithmetic
package simplifier

object SimplifyMod {

  def apply(dividend: ArithExpr, divisor: ArithExpr) : ArithExpr = simplifyMod(dividend, divisor)

  def simplifyMod(dividend: ArithExpr, divisor: ArithExpr) : ArithExpr = (dividend, divisor) match {
    case (_, Cst(1)) => Cst(0)
    case (Cst(x), _) if x == 0 || x == 1 => dividend

    case (Cst(a), Cst(b)) => Cst(a%b)

    case (m:Mod, d) if m.divisor == d => m

    case (x, y) if x == y => Cst(0)

    case (x, y) if ComputeGCD(x,y) == y => Cst(0)

    // Try through integer division and floor
    case (x, y) if floor(x/y).isEvaluable => x - y * floor(x/y)

    // Pull out constant
    case (s@Sum(terms), c:Cst) if terms.collect({ case Cst(_) => }).nonEmpty =>
      val h = terms.head
      h%c + (s - h) % c

    // For sum dividend s, try to partition into s1 and s2 so that s1/d is a multiple of d (d is the divisor)
    case (s:Sum,_) =>
      val terms = s.terms
      val termSubsets = Factorise.powerSet(terms).filter(_.nonEmpty)
      if (termSubsets.nonEmpty) {
        for (subset <- termSubsets.tail) {
          val rest = terms.diff(subset)
          val sum = if (subset.length > 1) Sum(subset) else subset.head
          val sumProd = sum.toProd
          if (sum == divisor || (sumProd.isDefined && sumProd.get.factors.contains(divisor))) {
            if (rest.length > 1) return Sum(rest) % divisor
            else return rest.head % divisor
          }
        }
      }
      // Can't find a good partition
      Mod(dividend,divisor)

    // Can't simplify
    case _ => Mod(dividend,divisor)
  }

}
