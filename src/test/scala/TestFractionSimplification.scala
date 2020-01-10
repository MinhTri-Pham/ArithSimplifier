import arithmetic._
import org.junit.Test
import org.junit.Assert._

class TestFractionSimplification {

  val a = Var("a")
  val b = Var("b")
  val c = Var("c")
  val d = Var("d")

  @Test
  def simplificationTest(): Unit = {
    assertEquals(a /^ Cst(2048), a * Cst(128) * Cst(1) /^ Cst(262144))
    assertEquals(a /^ Cst(2), a * (a*Cst(1)/^Cst(2)) /^ a)
  }

  @Test
  def factorisationTests() : Unit = {
    val numer1 = Cst(2)*a + Cst(2)*b
    val numer2 = a*a + a*b
    val numer3 = a*a + Cst(2)*a*b + b*b
    val denom1 = a+b
    val denom2 = a*c + b*c + a*d + b*d

    assertEquals(numer1 /^ numer1, Cst(1))
    assertEquals(denom1 /^ denom1, Cst(1))

    assertEquals(numer1 /^ denom1,Cst(2))
    assertEquals(numer2 /^ denom1,a)
    assertEquals(numer3 /^ denom1,a+b)
    assertEquals(numer3 /^ denom2,(a+b)/^(c+d))

    val numer4 = a*a + a
    val denom4 = a*a + Cst(3)*a

    val numer5 = a*a*a + Cst(4)*a*a + Cst(3)*a
    val denom5 = a*a*a + Cst(3)*a*a + Cst(2)*a

    assertEquals(numer4 /^ denom4, (a+Cst(1)) /^ (a+Cst(3)))
    assertEquals(numer5 /^ denom5, (a+Cst(3)) /^ (a+Cst(2)))
    assertEquals(numer4 /^ denom5, Cst(1) /^ (a+Cst(2)))
    assertEquals(numer5 /^ denom4, Cst(1)+a)


  }

}
