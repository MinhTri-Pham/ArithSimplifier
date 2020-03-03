import arithmetic._
import org.junit.Test
import org.junit.Assert._
class TestSum {

  val a: Var = Var("a")
  val b: Var = Var("b")
  val c: Var = Var("c")

  @Test
  def addZero(): Unit = {
    assertEquals(a, a+0)
  }

  @Test
  def varCancel() : Unit = {
    assertEquals(Cst(0), b-b)
    assertEquals(a,b+a-b)
  }

  @Test
  def constVarFold() : Unit = {
    val e1 = 3 + a - 2*b
    val e2 = b - 2
    assertEquals(1+a-b, e1+e2)
  }

  @Test
  def genTest(): Unit = {
    assertEquals(b + c - (a pow 2), 2*(a pow 2)  - c + b + 0 + 2 * c - 3 * (a pow 2))
  }
}
