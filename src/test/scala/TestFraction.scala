import arithmetic._
import org.junit.Test
import org.junit.Assert._

class TestFraction {

  val a: Var = Var("a")
  val b: Var = Var("b")
  val c: Var = Var("c")
  val d: Var = Var("d")

  @Test
  def simplificationTest(): Unit = {
    assertEquals(a /^ 2048, a * 128 * Cst(1) /^ 262144)
    assertEquals(a /^ 2, a * (a*Cst(1) /^ Cst(2)) /^ a)
    assertEquals(b,(b/^(a+b))*(a pow -1)*(a+b)*a)
    assertEquals(Cst(1), (a*b /^b) /^b * (b /^ a))
  }

  @Test
  def multiVarBasic() : Unit = {
    val e1 = 2*a + 2*b
    val e2 = a*a + a*b
    val e3 = a*a + 2*a*b + b*b
    val e4 = a+b
    val e5 = a*c + b*c + a*d + b*d
    val e6 = (a pow 2) + a*b+a+b

    assertEquals(a,e2 /^ e4)
    assertEquals((a+b)/^(c+d),e3 /^ e5)
    assertEquals((c+d) /^ 2, e5 /^ e1)
    assertEquals(a/^(a+1),e2 /^ e6)
  }

  @Test
  def uniVarCst(): Unit = {
    val e1 = a*a + a
    val e2 = a*a + 3*a + 2
    val e3 = (a pow 3) + 6*a*a + 11*a + 6
    val e4 = (a pow 3) + 5*a*a + 8*a + 4
    assertEquals(a /^ (a + 2),e1 /^ e2)
    assertEquals(a + 3,e3 /^ e2)
    assertEquals((a + 3) /^ (a + 2),e3 /^ e4)
  }

  @Test
  def multiVarSimplification(): Unit = {
    assertEquals(2*(a+b),(a /^ 2)*(4*a*c + 4*a*d + 4*b*c + 4*b*d) * (Cst(1) /^(a*(c+d))))
  }
}
