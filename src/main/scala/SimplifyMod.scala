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

    case (s:Sum,_) if divisor.isInstanceOf[Cst] || divisor.isInstanceOf[Var]=> s.terms.map(x => x % divisor).reduce((a,b) => a+b)

    // Idea: Otherwise for sum try subsets (larger ones first)??
    // Examples that could be handled through this
    // (a + b + c) / (a+b)  =>  c % (a+b)  (through (a+b) % (a+b) + c % (a+b))
    // (ax + bx + c) / (a+b) => c % (a+b) (through (ax+bx) % (a+b) + c % (a+b))
    // (ax + bx + c + d) / (a+b) => x + (c+d) / (a+b) (through (ax+bx) % (a+b) + (c+d) % (a+b))
    // (ax + ay + bx + by + c) / (a+b) => x + y + c / (a+b)  (through (ax + ay + bx + by) % (a+b) + c % (a+b))
    case (s:Sum,_) =>
      val terms = s.terms
      val termSubsets = Factorise.powerSet(terms).filter(_.length > 1)
      if (termSubsets.nonEmpty) {
        for (subset <- termSubsets.tail) {
          val rest = terms.diff(subset)
          val sum = Sum(subset)
          val sumProd = sum.asProd
          if (sum == divisor || (sumProd.isDefined && sumProd.get.factors.contains(divisor))) {
            if (rest.length > 1) return Sum(rest) % divisor
            else return rest.head % divisor
          }
        }
      }
      // Can't find a good subset
      Mod(dividend,divisor)

    // Can't simplify
    case _ => Mod(dividend,divisor)
  }

}
