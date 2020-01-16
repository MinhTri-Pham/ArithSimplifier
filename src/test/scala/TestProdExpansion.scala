import arithmetic._
import org.junit.Test
import org.junit.Assert._

class TestProdExpansion {

  val a = Var("a")
  val b = Var("b")

@Test
  def basicTests(): Unit = {
    val e1 = a+b
    assertEquals(e1, b+a)
    val e2 = a * (a pow 2)
    assertEquals(e2, a pow 3)

    val e3_1 = a + Cst(3) * b * b
    val e3_2 = Cst(2) * a - (b pow 2)
    val e4 = Cst(3) * a + Cst(2) * (b pow 2)
    assertEquals(e4, e3_1+e3_2)

    val e5 = e1 * e1 * e1
    assertEquals(e5, e1 pow 3)
    assertEquals(e1 pow 2, e5 /^ e1)
  }

  @Test
  def expandProdTests() : Unit = {
    val e1 = Cst(2) * (a+Cst(2)) - Cst(4)
    val e2 = (a+Cst(2)) * Cst(2) - a * Cst(2)
    assertEquals(e1, Cst(2) * a)
    assertEquals(e2, Cst(4))

    val e3 = a * ((a pow -1) + b) - a*b
    assertEquals(e3, Cst(1))

    val e4 = a * (a-Cst(1)) + a * (Cst(1)-a)
    val e5 = a * (a-Cst(1)) + a * (a + Cst(1))
    assertEquals(e4,Cst(0))
    assertEquals(e5, Cst(2) * (a pow 2))

    val e6 = a*b*(a+Cst(2)) + b*(Cst(1)-a)* a
    assertEquals(e6, Cst(3)*a*b)

    val e7 = a*b*(a+(a pow -1)) + a*((a pow -1)-a)* b
    assertEquals(e7, Cst(2) * b)

    val e8 = (a+b pow 2) - Cst(2) * a * b
    assertEquals(e8, (a pow 2) + (b pow 2))

    val e9 = (a+Cst(2) pow 2) - Cst(4)*(a+Cst(1))
    assertEquals(e9, a pow 2)

    val e10 = ((a+Cst(2)) pow 2) - ((a+Cst(1)) pow 2)
    assertEquals(e10, Cst(3) + Cst(2)*a)
  }
}
