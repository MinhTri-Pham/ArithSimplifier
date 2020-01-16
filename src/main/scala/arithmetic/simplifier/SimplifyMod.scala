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

    case (x, y) if ArithExpr.isSmaller(x,y).getOrElse(false) => x

    // Try through integer division and floor
    case (x, y) if floor(x/y).isEvaluable => x - y * floor(x/y)

    // Try through integer division
    //case (x, y) if (x/y).getSumProdSimplify.collect({ case IntDiv(_, _)=> }).isEmpty => x - y * (x/y)

    // Pull out constant
    case (s@Sum(terms), c:Cst) if terms.collect({ case Cst(_) => }).nonEmpty =>
      val h = terms.head
      h%c + (s - h) % c

    // For sum dividend s, try to partition into s1 and s2 so that s1 is multiple of d in the constant case
    // or gcd(s1,d) != 1 (d is the divisor)
    case (s:Sum,_) =>
      val termsExpanded = Helper.expandTermsCst(s.terms)
      val termSubsets = Helper.powerSet(termsExpanded).filter(_.nonEmpty)
      if (termSubsets.nonEmpty) {
        for (subset <- termSubsets.tail) {
          val restExp = termsExpanded.diff(subset)
          val sum = subset.reduce(_ + _)
          val rest = restExp.reduce(_ + _)
          val gcd = ComputeGCD(sum, divisor)
          if ((divisor.isInstanceOf[Cst] && gcd % divisor == Cst(0)) || (!divisor.isInstanceOf[Cst] && gcd != Cst(1))) {
            return sum % divisor + rest % divisor
          }
        }
      }
      // Can't find a good partition
      Mod(dividend,divisor)

    // Can't simplify
    case _ => Mod(dividend,divisor)
  }

}
