import org.junit.Test
import org.junit.Assert._

class TestPowProd {

  val a = Var("a")
  val b = Var("b")
  val c = Var("c")

  @Test
  def basicTests(): Unit = {
    assertEquals(Cst(1), a /^ a)
    assertEquals(a, (a * a) /^ a)
    assertEquals(a, a * (a /^ a))
    assertEquals(a, (a pow 3) /^ (a pow 2))
  }

  @Test
  def expandPowTest(): Unit = {
    val e1 = (a*b) pow 2
    assertEquals(e1 * a, (a pow 3) * (b pow 2))
    assertEquals(e1 /^ a, a * (b pow 2))
    assertEquals(e1 /^ (a*b), a*b)
    assertEquals(e1 /^ (a*a*b), b)
    assertEquals(e1 /^ ((a pow 3) * (b pow 2)), Cst(1) /^ a)

    assertEquals((a*b)/^(a*c), b/^c)
    assertEquals((a*b*b*c) /^ ((b*c) pow 2), a/^c)
    assertEquals(((a*b*c) pow 2) /^ ((b*c) pow 2), a pow 2)
  }

}
