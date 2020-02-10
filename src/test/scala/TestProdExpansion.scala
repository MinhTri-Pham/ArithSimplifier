import arithmetic._
import org.junit.Test
import org.junit.Assert._

class TestProdExpansion {

  val a: Var = Var("a")
  val b: Var = Var("b")
  val c: Var = Var("c")

@Test
  def basicTests(): Unit = {
    val e1 = a+b
    assertEquals(e1, b+a)
    val e2 = a * (a pow 2)
    assertEquals(e2, a pow 3)

    val e3_1 = a + 3*b*b
    val e3_2 = 2*a - (b pow 2)
    val e4 = 3*a + 2*(b pow 2)
    assertEquals(e4, e3_1+e3_2)

    val e5 = e1 * e1 * e1
    assertEquals(e5, e1 pow 3)
    assertEquals(e1 pow 2, e5 /^ e1)
  }

  @Test
  def expandTests() : Unit = {
    val e1 = 2*(a + 2) - 4
    val e2 = (a + 2)*2 - 2*a
    assertEquals(e1, 2*a)
    assertEquals(e2, Cst(4))

    val e3 = a * ((a pow -1) + b) - a*b
    assertEquals(e3, Cst(1))

    val e4 = a * (a-Cst(1)) + a * (1 - a)
    val e5 = a * (a-Cst(1)) + a * (a + 1)
    assertEquals(e4,Cst(0))
    assertEquals(e5, 2*(a pow 2))

    val e6 = a*b*(a + 2) + b*(1 - a)* a
    assertEquals(e6, 3*a*b)

    val e7 = a*b*(a+(a pow -1)) + a*((a pow -1)-a)* b
    assertEquals(e7, 2*b)

    val e8 = ((a+b) pow 2) - 2*a*b
    assertEquals(e8, (a pow 2) + (b pow 2))

    val e9 = ((a+2) pow 2) - 4*(a+1)
    assertEquals(e9, a pow 2)

    val e10 = ((a+2) pow 2) - ((a+1) pow 2)
    assertEquals(e10, 3 + 2*a)
  }

  @Test
  def partialCancelTests(): Unit = {
    val e1 = (a*b+c) /^ a - b
    assertEquals(e1, c /^ a)

    val e2 = (a*b + a*c + 1) /^ a - b - c
    assertEquals(e2, a pow -1)

    val e3 = (a+b+c) /^ (a+b) - 1
    assertEquals(e3, c /^ (a+b))
  }
}
