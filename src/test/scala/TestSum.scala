import org.junit.Test
import org.junit.Assert._
class TestSum {

  val a = Var(1,"a")
  val bp = Var(1,"b", Some(1))
  val bm = Var(-1,"b", Some(1))

  @Test
  def addZero(): Unit = {
    assertEquals(a, a+Cst(0))
  }

  @Test
  def varCancel() : Unit = {
    assertEquals(a,bp+a+bm)
  }

  @Test
  def constFolding(): Unit = {
    assertEquals(Cst(4), Cst(1) + Cst(3))
  }

  @Test
  def bothCstVar() : Unit = {
    val e1 = Cst(2) + a + bm
    val e2 = bp + Cst(-2)
    assertEquals(a, e1+e2)
  }

}
