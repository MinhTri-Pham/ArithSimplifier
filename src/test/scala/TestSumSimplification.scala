import arithmetic._
import org.junit.Test
import org.junit.Assert._
class TestSumSimplification {

  val a: Var = Var("a")
  val b: Var = Var("b")

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
  def constFold(): Unit = {
    assertEquals(a + 4, 1 + a + 3)
  }

  @Test
  def varFold() : Unit = {
    assertEquals(2*b+2*a+b, 2*a+3*b)
  }

  @Test
  def constVarFold() : Unit = {
    val e1 = 2 + a - b
    val e2 = b - 2
    assertEquals(a, e1+e2)
  }
}
