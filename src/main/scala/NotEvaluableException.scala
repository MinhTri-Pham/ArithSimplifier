import scala.util.control.ControlThrowable

abstract case class NotEvaluableException private () extends ControlThrowable

/**
 * Companion object for `NotEvaluableException`.
 * Use `NotEvaluableException.NotEvaluable` when throwing.
 * Allows for easier debugging while not calling the constructor in the general case.
 */
object NotEvaluableException {
  val NotEvaluable: NotEvaluableException = new NotEvaluableException() {}
}
