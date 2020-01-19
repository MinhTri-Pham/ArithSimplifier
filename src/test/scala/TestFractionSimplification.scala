import arithmetic._
import org.junit.Test
import org.junit.Assert._

class TestFractionSimplification {

  val a: Var = Var("a")
  val b: Var = Var("b")
  val c: Var = Var("c")
  val d: Var = Var("d")

  @Test
  def simplificationTest(): Unit = {
    assertEquals(a /^ Cst(2048), a * Cst(128) * Cst(1) /^ Cst(262144))
    assertEquals(a /^ Cst(2), a * (a*Cst(1)/^Cst(2)) /^ a)
    assertEquals(b,(b/^(a+b))*(a pow -1)*(a+b)*a)
    assertEquals(Cst(1), (a*b /^b) /^b * (b /^ a))
  }

  @Test
  def sumTest(): Unit = {
    assertEquals(a+b,a*((a pow -1)+(b pow -1))*b)
  }

  @Test
  def multiVarBasic() : Unit = {
    val numer1 = Cst(2)*a + Cst(2)*b
    val numer2 = a*a + a*b
    val numer3 = a*a + Cst(2)*a*b + b*b
    val denom1 = a+b
    val denom2 = a*c + b*c + a*d + b*d

    assertEquals(Cst(1),numer1 /^ numer1)
    assertEquals(Cst(2),numer1 /^ denom1)
    assertEquals(a,numer2 /^ denom1)
    assertEquals(a+b,numer3 /^ denom1)
    assertEquals((a+b)/^(c+d),numer3 /^ denom2)
  }

  @Test
  def uniVarCst(): Unit = {
    val e1 = a*a + a
    val e2 = a*a + Cst(3)*a + Cst(2)
    val e3 = (a pow 3) + Cst(6)*a*a+Cst(11)*a+Cst(6)
    val e4 = (a pow 3) + Cst(5)*a*a+Cst(8)*a+Cst(4)
    assertEquals(a /^ (a+Cst(2)),e1 /^ e2)
    assertEquals(a+Cst(3),e3 /^ e2)
    assertEquals((a+Cst(3)) /^ (a+Cst(2)),e3 /^ e4)
  }

  @Test
  def multiVarSimplification(): Unit = {
    assertEquals(Cst(2)*a*(a+b),(Cst(4)*a*c+Cst(4)*a*d+Cst(4)*b*c+Cst(4)*b*d) * a * (a /^ Cst(2)) * (Cst(1) /^(a*(c+d))))
  }

}
