package arithmetic

import scala.util.control.ControlThrowable

abstract case class NotEvaluableException private() extends ControlThrowable

object NotEvaluableException {
  val NotEvaluable: NotEvaluableException = new NotEvaluableException() {}
}

abstract case class NotDifferentiableException private() extends ControlThrowable

object NotDifferentiableException {
  val NotDifferentiable: NotDifferentiableException = new NotDifferentiableException() {}
}
