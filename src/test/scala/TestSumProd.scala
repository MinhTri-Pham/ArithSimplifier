import org.junit.Test
import org.junit.Assert._

class TestSumProd {

  val a = Var("a")
  val b = Var("b")

@Test
  def tests(): Unit = {
    val e1 = a+b
    assertEquals(e1, b+a)
    val e2 = Cst(3) * e1
    assertEquals(e2, e1 + Cst(2) * e1)
    val e3 = a * Pow(a,2)
    assertEquals(e3, Pow(a,3))
    val e4 = Cst(3) * a + Cst(4) * Pow(b,2)
    assertEquals(e4, a + Cst(3) * b * b + Cst(2) * a + Pow(b,2))
    val e5 = e1 * e1 * e1
    assertEquals(e5, Pow(e1,3))
  }
}
