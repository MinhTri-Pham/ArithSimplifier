import arithmetic._
import org.junit.Test
import org.junit.Assert._

class TestFraction {

  val a: Var = Var("a")
  val b: Var = Var("b")
  val c: Var = Var("c")
  val d: Var = Var("d")

  @Test
  def divisionNormalForm(): Unit = {
    assertEquals(a*c /^ b, a /^ (b /^ c))
    assertEquals((a*c) /^ (b*d), a /^ (b /^ (c /^ d)))
    assertEquals(a /^ b, (a /^ (b /^ c)) /^ c)
    assertEquals(a, (a /^ (b /^ c)) /^ (c /^ b))
  }

  @Test
  def simplificationTest(): Unit = {
    assertEquals(a /^ 2048, a * 128 * Cst(1) /^ 262144)
    assertEquals(a /^ 2, a * (a*Cst(1) /^ Cst(2)) /^ a)
    assertEquals(b,(b/^(a+b))*(a pow -1)*(a+b)*a)
    assertEquals(Cst(1), (a*b /^b) /^b * (b /^ a))
  }

  @Test
  def multiVarBasic() : Unit = {
    val numer1 = 2*a + 2*b
    val numer2 = a*a + a*b
    val numer3 = a*a + 2*a*b + b*b
    val denom1 = a+b
    val denom2 = a*c + b*c + a*d + b*d

    assertEquals(Cst(1),numer1 /^ numer1)
    assertEquals(Cst(2),numer1 /^ denom1)
    assertEquals(a,numer2 /^ denom1)
    assertEquals(a+b,numer3 /^ denom1)
    assertEquals((a+b)/^(c+d),numer3 /^ denom2)
  }

  @Test
  def sumTest(): Unit = {
    val expr_1 = a*((a pow -1) + (b pow -1))*b
    assertEquals(a+b, expr_1.toSum.get)
    val expr_2 = a*((a /^ b) + (b /^ a))*b
    assertEquals((a pow 2) + (b pow 2), expr_2.toSum.get)
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
    assertEquals(2*a*(a+b),(4*a*c + 4*a*d + 4*b*c + 4*b*d) * a * (a /^ 2) * (Cst(1) /^(a*(c+d))))
  }
}
