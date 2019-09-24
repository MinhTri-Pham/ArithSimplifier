import org.junit.Test
import org.junit.Assert._
class TestSum {

  val a = Var("a")
  val b = Var("b")

  @Test
  def addZero(): Unit = {
    assertEquals(a, a+Cst(0))
  }

  @Test
  def varCancel() : Unit = {
    assertEquals(a,b+a-b)
  }

  @Test
  def constFolding(): Unit = {
    assertEquals(a+Cst(4), Cst(1) + a + Cst(3))
  }

  @Test
  def bothCstVar() : Unit = {
    val e1 = Cst(2) + a - b
    val e2 = b + Cst(-2)
    assertEquals(a, e1+e2)
  }

}
