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
  def sumTest(): Unit = {
    val expr_1 = a*((a pow -1) + (b pow -1))*b
    assertEquals(a+b, expr_1.toSum.get)
    val expr_2 = a*((a /^ b) + (b /^ a))*b
    assertEquals((a pow 2) + (b pow 2), expr_2.toSum.get)
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

//  @Test
//  def sumProd(): Unit = {
//
//    val expr15_1 = Cst(1)/^(a + b) * a.pow(2) + Cst(1)/^(a + b) * Cst(2)*a*b
//    val expr15_2 = Cst(1)/^(a + b) * b.pow(2)
//
//    val expr15 = expr15_1 + expr15_2
//    assertEquals(expr15, a+b)
//
//    val expr_16_1 = Cst(1)/^(a + b) * c * a.pow(2) + Cst(1)/^(a + b) * c * Cst(2)*a*b
//    val expr_16_2 = Cst(1)/^(a + b)* c * b.pow(2)
//
//    val expr_16 = expr_16_1 + expr_16_2
//    assertEquals(expr_16, c*(a+b))
//
//    val expr_18_1 = Cst(1)/^(a + b) * Cst(3) * a.pow(2) + Cst(1)/^(a + b) * Cst(3) * Cst(2)*a*b
//    val expr_18_2 = Cst(1)/^(a + b) * Cst(3) * b.pow(2)
//
//    val expr_18 = expr_18_1 + expr_18_2
//    assertEquals(expr_18, Cst(3) * (a + b))
//  }
}
