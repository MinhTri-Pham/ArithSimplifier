import scala.collection.mutable.ListBuffer

object SimplifyProd {

  def apply(lhs: ArithExpr, rhs: ArithExpr): ArithExpr = multExprs(lhs, rhs)

  // Multiplies two expressions
  def multExprs(lhs: ArithExpr, rhs: ArithExpr): ArithExpr = {
    var lhsFactors, rhsFactors: List[ArithExpr] = List[ArithExpr]()
    (lhs, rhs) match {
      // Special case
      case (Cst(0), _) => return Cst(0)
      case (_, Cst(0)) => return Cst(0)
      // If there's a power (with product as base), expand it into a product
      // To do: If either side is a sum, try to factorise and then collect terms as usual
      case (p1: Pow, p2:Pow) =>
        val lhsProd = p1.asProdPows
        val rhsProd = p2.asProdPows
        (lhsProd,rhsProd) match {
          case (Some(_), Some(_)) =>
            lhsFactors = lhsProd.get.getSumProdList.sortWith(ArithExpr.isCanonicallySorted)
            rhsFactors = rhsProd.get.getSumProdList.sortWith(ArithExpr.isCanonicallySorted)

          case (Some(_), None) =>
            lhsFactors = lhsProd.get.getSumProdList.sortWith(ArithExpr.isCanonicallySorted)
            rhsFactors = List[ArithExpr](p2)

          case (None, Some(_)) =>
            lhsFactors = List[ArithExpr](p1)
            rhsFactors = rhsProd.get.getSumProdList.sortWith(ArithExpr.isCanonicallySorted)

          case (None, None) =>
            lhsFactors = List[ArithExpr](p1)
            rhsFactors = List[ArithExpr](p2)
        }

      case (p: Pow, s:Sum) =>
        val lhsProd = p.asProdPows
        lhsProd match {
          case Some(_) =>
            lhsFactors = lhsProd.get.getSumProdList.sortWith(ArithExpr.isCanonicallySorted)

          case None =>
            lhsFactors = List[ArithExpr](p)
        }
        rhsFactors = List[ArithExpr](s)

      case (s:Sum, p:Pow) =>
        val rhsProd = p.asProdPows
        rhsProd match {
          case Some(_) =>
            rhsFactors = rhsProd.get.getSumProdList.sortWith(ArithExpr.isCanonicallySorted)

          case None =>
            rhsFactors = List[ArithExpr](p)
        }
        lhsFactors = List[ArithExpr](s)

      case (s1: Sum, s2: Sum) =>
        lhsFactors = List[ArithExpr](s1)
        rhsFactors = List[ArithExpr](s2)

      case (p: Pow, _) =>
        val pProd = p.asProdPows
        if (pProd.isDefined) lhsFactors = pProd.get.getSumProdList.sortWith(ArithExpr.isCanonicallySorted)
        else lhsFactors = List[ArithExpr](p)
        rhsFactors = rhs.getSumProdList.sortWith(ArithExpr.isCanonicallySorted)

      case (_, p: Pow) =>
        val pProd = p.asProdPows
        if (pProd.isDefined) rhsFactors = pProd.get.getSumProdList.sortWith(ArithExpr.isCanonicallySorted)
        else rhsFactors = List[ArithExpr](p)
        lhsFactors = lhs.getSumProdList.sortWith(ArithExpr.isCanonicallySorted)

      case (s: Sum, _) =>
        lhsFactors = List[ArithExpr](s)
        rhsFactors = rhs.getSumProdList.sortWith(ArithExpr.isCanonicallySorted)

      case (_, s: Sum) =>
        lhsFactors = lhs.getSumProdList.sortWith(ArithExpr.isCanonicallySorted)
        rhsFactors = List[ArithExpr](s)

      // Neither side is a sum, decompose into smaller terms and merge
      case _ =>
        // Extract and sort factors of both sides and merge
        lhsFactors = lhs.getSumProdList.sortWith(ArithExpr.isCanonicallySorted)
        rhsFactors = rhs.getSumProdList.sortWith(ArithExpr.isCanonicallySorted)
    }
    mergeFactors(lhsFactors,rhsFactors)
  }

  // Merges factors of expressions to be multiplied
  // Assumes both factor lists are canonically sorted
  def mergeFactors(lhsFactors:List[ArithExpr], rhsFactors:List[ArithExpr]) : ArithExpr = {
    val merged = ListBuffer[ArithExpr]()
    val lhsSize = lhsFactors.length
    val rhsSize = rhsFactors.length
    var i,j = 0
    while (i < lhsSize && j < rhsSize) {
      val lhsTerm = lhsFactors(i)
      val rhsTerm = rhsFactors(j)
      val combinedTerm = combineFactors(lhsTerm, rhsTerm)
      if (combinedTerm.isDefined) {
        merged += combinedTerm.get
        i+=1
        j+=1
      }
      else {
        if (ArithExpr.isCanonicallySorted(lhsTerm, rhsTerm)) {
          merged += lhsTerm
          i+=1
        }
        else {
          merged += rhsTerm
          j+=1
        }
      }
    }
    while (i < lhsSize) {
      merged += lhsFactors(i)
      i+=1
    }
    while (j < rhsSize) {
      merged += rhsFactors(j)
      j+=1
    }
    convert(merged.toList)
  }

  // Tries to combine a pair of factors
  def combineFactors(lhs: ArithExpr, rhs: ArithExpr) : Option[ArithExpr] = (lhs, rhs) match {
   // Trivial cases
    case (Cst(x), Cst(y)) => Some(Cst(x * y))
    case (Cst(1), _) => Some(rhs)
    case (_, Cst(1)) => Some(lhs)
//    case (Cst(c), v: Var) => Some(v.copy(c*v.cstMult))
//    case (v:Var,Cst(c)) => Some(v.copy(c*v.cstMult))

    // Compute powers when all bases and exponents are positive constants
    case (Pow(Cst(b1), e1), Pow(Cst(b2), e2)) if e1 > 0 && e2 > 0 =>
      Some(Cst((Math.pow(b1, e1) * Math.pow(b2, e2)).toInt))

    // More general cases
    case (Pow(b1,e1), Pow(b2,e2)) =>
      if (b1 == b2) Some(b1 pow (e1+e2))
      else if (e1 == e2) Some((b1 * b2) pow e1)
      else None
    case (x, Pow(b,e)) =>
      if (x == b) Some(b pow (e+1))
      else None
    case (Pow(b,e),x) =>
      if (x == b) Some(b pow (e+1))
      else None
    case (x,y) =>
      if (x==y) Some(x pow 2)
      else None
    case _ => None
  }

  // Given list of factors, determine resulting expression
  // Remove unnecessary one terms that could arise from combining terms
  // I.e. Prod(1,Var(a),Var(b)) -> Prod(Var(a),Var(b)) but Prod(1, Var(a)) -> Var(a)
  def convert(factors: List[ArithExpr]): ArithExpr = {
    val nonOne = factors.filter(_ != Cst(1))
    if (nonOne.isEmpty) Cst(1) // Eliminated everything, so result is 1
    else if (nonOne.length == 1) nonOne.head // Result is a Var or Cst
    else Prod(nonOne) // Have a product of expressions
  }
}
