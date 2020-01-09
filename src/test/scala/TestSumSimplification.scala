import arithmetic._
import org.junit.Test
import org.junit.Assert._
class TestSumSimplification {

  val a = Var("a")
  val b = Var("b")

  @Test
  def addZero(): Unit = {
    assertEquals(a, a+Cst(0))
  }

  @Test
  def varCancel() : Unit = {
    assertEquals(Cst(0), b-b)
    assertEquals(a,b+a-b)
  }

  @Test
  def constFold(): Unit = {
    assertEquals(a+Cst(4), Cst(1) + a + Cst(3))
  }

  @Test
  def varFold() : Unit = {
    assertEquals(b+b, Cst(2)*b)
    assertEquals(b+a+b, a+Cst(2)*b)
    assertEquals(Cst(2)*b+a+b, a+Cst(3)*b)
    assertEquals(Cst(2)*b+Cst(2)*a+b, Cst(2)*a+Cst(3)*b)
  }

  @Test
  def bothCstVar() : Unit = {
    val e1 = Cst(2) + a - b
    val e2 = b + Cst(-2)
    assertEquals(a, e1+e2)
  }

}
