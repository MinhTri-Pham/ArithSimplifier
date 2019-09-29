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
    val e3 = a * (a pow 2)
    assertEquals(e3, a pow 3)
    val e4 = Cst(3) * a + Cst(2) * (b pow 2)
    assertEquals(e4, a + Cst(3) * b * b + Cst(2) * a - (b pow 2))
    val e5 = e1 * e1 * e1
    assertEquals(e5, e1 pow 3)
  }

  @Test
  def ordDivTests(): Unit = {
    assertEquals(Cst(2) * b, Cst(2) * a * b /^ a)
   // assertEquals(Cst(2), Cst(2) * a * b /^ (a * b) // Need to expand the power in denominator as product
  }
}
