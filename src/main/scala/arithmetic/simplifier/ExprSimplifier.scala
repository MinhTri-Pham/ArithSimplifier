package arithmetic
package simplifier

// Generic expression simplifier
object ExprSimplifier {

  def apply(expr: ArithExpr): ArithExpr = expr match {
    case c:Cst => c
    case v:Var => v
    case Pow(x, y) => SimplifyPow(x, y)
    case p:Prod => p.factors.reduce(_*_)
    case s:Sum => s.terms.reduce(_+_)
    case AbsFunction(ae) => SimplifyAbs(ae)
    case FloorFunction(ae) => SimplifyFloor(ae)
    case CeilingFunction(ae) => SimplifyCeiling(ae)
    case _ => expr
  }
}
