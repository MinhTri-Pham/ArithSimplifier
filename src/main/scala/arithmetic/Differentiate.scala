package arithmetic

import arithmetic.NotDifferentiableException._

object Differentiate {

  /**
   * Tries to differentiate an expression by applying a standard rule
   *
   * @param expr            A expression to differentiate
   * @param v               A variable with respect to which to differentiate
   * @return                The derivative of expr with respect to v with successful,
   *                        a NotDifferentiable exception otherwise
   */

  private def differentiate(expr: ArithExpr, v:Var) : ArithExpr = {
    // Check that expr contains v in the first place, if not return 0
    val containsV = ArithExpr.visitUntil(expr, x => x == v)
    if (!containsV) return Cst(0)
    (expr, v) match {
      case (_: Cst, _) => Cst(0)
      case (w: Var, _) =>
        if (v == w) Cst(1)
        else Cst(0)
      case (Pow(f, e), _) => Cst(e) * (f pow (e - 1)) * (f diff v) // Power rule
      case (Sum(terms),_) =>
        val derivatives = terms.map(t => t diff v)
        derivatives.reduce(_ + _)
      // General product rule
      case (p: Prod, _) =>
        val cstFactor = Cst(p.cstFactor)
        val nonCst = p.nonCstList
        if (nonCst.length == 1) cstFactor * (nonCst.head diff v)
        else if (nonCst.length == 2) cstFactor * productRule(nonCst.head, nonCst.last, v)
        else {
          val summands = p.factors.map(x => (p /^ x) * (x diff v))
          summands.reduce(_ + _)
        }
      case (AbsFunction(ae), _) => (ae /^ expr) * (ae diff v)
      // Derivative of floor and ceiling is 0 at non-integers
      // Argument inside floor or ceiling can't be an integer value
      case (FloorFunction(_), _) => Cst(0)
      case (CeilingFunction(_), _) => Cst(0)
      case _ => throw NotDifferentiable // Log function needs natural log to be defined
    }
  }

  // Standard product rule for two expressions
  private def productRule(ae1:ArithExpr, ae2:ArithExpr, v:Var): ArithExpr = {
    ae1*(ae2 diff v) + ae2*(ae1 diff v)
  }

  def apply(expr:ArithExpr, v:Var) : ArithExpr = differentiate(expr,v)

  // Higher derivative by repeated differentiation
  def apply(expr:ArithExpr, v:Var, n:Int) : ArithExpr = {
    if (n < 0) throw new ArithmeticException()
    if (n == 0) return expr // Zeroth derivative is expression itself
    var derivative = expr
    var i = 1;
    while(i <= n) {
      derivative = differentiate(derivative,v)
      if (derivative == Cst(0)) i = n+1 // Avoid doing differentiating needlessly once when reached 0
      else i+=1
    }
    derivative
  }
}
