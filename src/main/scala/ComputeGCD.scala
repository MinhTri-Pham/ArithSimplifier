object ComputeGCD {
  // Computes gcd of two expressions
  def apply (a : ArithExpr, b : ArithExpr) : ArithExpr = (a, b) match {
    case (Cst(x), Cst(y)) => Cst(gcdInt(x,y))
    case (x,y) if x == y => x
    case (x, Pow(ob, _)) if ob == x => x
    case (Pow(ob, _),x) if ob == x => x
    case (Pow(b1, e1), Pow(b2, e2)) if b1 == b2 => b1 pow scala.math.min(e1,e2)
    case (Pow(ob,_), Prod(f)) if f.contains(ob) => ob
    case (Prod(f), Pow(ob, _)) if f.contains(ob) => ob
    case (Cst(c), p:Prod) => Cst(gcdInt(c,p.cstFactor))
    case (p:Prod, Cst(c)) => Cst(gcdInt(c,p.cstFactor))
    case (Prod(fs1), Prod(fs2)) => (for {f1 <- fs1; f2 <- fs2} yield ComputeGCD(f1, f2)).reduce(_ * _)
    case (x, Prod(f)) if f.contains(x) => x
    case (Prod(f),x) if f.contains(x) => x
    case _ => Cst(1)
  }

  // Computes gcd of a sum by finding a common factor of all terms
  def factorizeSum(s: Sum): ArithExpr = {
    factorizeList(s.terms)
  }

  def factorizeList(terms: List[ArithExpr]): ArithExpr = {
    if (terms.isEmpty) Cst(1)
    else terms.reduce((l,r) => ComputeGCD(l,r))
  }

  // Computes gcd of two integers
  @scala.annotation.tailrec
  def gcdInt(a: Int, b: Int) : Int = {
    if (b == 0) scala.math.abs(a) else gcdInt(scala.math.abs(b), scala.math.abs(a) % b)
  }

}
