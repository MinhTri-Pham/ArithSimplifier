object SimplifyIntDiv {

  def apply(numer : ArithExpr, denom : ArithExpr) : ArithExpr = simplifyIntDiv(numer,denom)

  def simplifyIntDiv(numer: ArithExpr, denom: ArithExpr): ArithExpr = (numer, denom) match {
    case (_, Cst(1)) => numer
    case (Cst(x), Cst(y)) if y != 0 => Cst(x/y)
    case (Cst(0),_) => Cst(0)
    case (p:Prod,Cst(n)) if p.cstFactor >= n => Cst(p.cstFactor / n) * p.nonCstFactor
    case (p:Prod,Cst(n)) if p.cstFactor < n => p.nonCstFactor
    // If denominator a constant or a variable, try integer division of sum => sum of integer division of terms
    // E.g (ax + bx + c) / x => c / x
    case (s:Sum,_) if denom.isInstanceOf[Cst] || denom.isInstanceOf[Var]=> s.terms.map(x => x / denom).reduce((a,b) => a+b)
    // Return zero if the denominator is smaller than the numerator
    case (x, y) if ArithExpr.isSmaller(x, y).getOrElse(false) => Cst(0)
    // If there exists a common denominator, simplify
    case (x, y) if ComputeGCD(x, y) != Cst(1) =>
      val fac = ComputeGCD(x, y)
      (x /^ fac) / (y /^ fac)
    // Idea: Otherwise for sum try subsets (larger ones first)??
    // Examples that could be handled through this
    // (a + b + c) / (a+b)  => 1 + c / (a+b)  (through (a+b) / (a+b) + c / (a+b))
    // (ax + bx + c) / (a+b) => x + c / (a+b) (through (ax+bx) / (a+b) + c / (a+b))
    // (ax + bx + c + d) / (a+b) => x + (c+d) / (a+b) (through (ax+bx) / (a+b) + (c+d) / (a+b))
    // (ax + ay + bx + by + c) / (a+b) => x + y + c / (a+b)  (through (ax + ay + bx + by) / (a+b) + (c) / (a+b))
    case (s:Sum,_) =>
      val terms = s.terms
      val termSubsets = Factorise.powerSet(terms).filter(_.length > 1)
      if (termSubsets.nonEmpty) {
        for (subset <- termSubsets.tail) {
          val rest = terms.diff(subset)
          val subsetDiv = Sum(subset) / denom
          if (!subsetDiv.isInstanceOf[IntDiv]) {
            if (rest.length > 1) return subsetDiv + Sum(rest) / denom
            else return subsetDiv + rest.head / denom
          }
        }
      }
      // Can't find a good subset
      IntDiv(numer,denom)
    // Flip division in the numerator
    case (IntDiv(numer, denom1), denom2) => numer / (denom1 * denom2)
    // Flip fractions in the denominator
    case (numer, Pow(base, -1)) =>numer * base
    // Try min and max
    case _ =>
      val numMin = numer.min
      val numMax = numer.max
      val denomMin = denom.min
      val denomMax = denom.max
      if (numMin == ? || numMax == ? || denomMin == ? || denomMax == ?) return IntDiv(numer,denom)
      if ((numMin / denomMax).evalDouble ==(numMax / denomMin).evalDouble) {
        return numMin / denomMax
      }
      // Can't simplify
      IntDiv(numer,denom)
  }

}
