import arithmetic._
import org.junit.Test
import org.junit.Assert._

class TestIntDivModWithNegative {

  @Test
  def cstIntDiv(): Unit = {
    assertEquals(Cst(1), Cst(5) / Cst(3))
    assertEquals(Cst(-1), Cst(5) / Cst(-3))
    assertEquals(Cst(-1), Cst(-5) / Cst(3))
    assertEquals(Cst(1), Cst(-5) / Cst(-3))
  }

  @Test
  def cstMod(): Unit = {
    assertEquals(Cst(2), Cst(5) % Cst(3))
    assertEquals(Cst(2), Cst(5) % Cst(-3))
    assertEquals(Cst(-2), Cst(-5) % Cst(3))
    assertEquals(Cst(-2), Cst(-5) % Cst(-3))
  }

  @Test
  def varTest(): Unit = {
    val a = Var("a")
    val b = NegVar("b")
    val c = Var("c")
    assertEquals(ceil(b),(a*b) / a)
    assertEquals(floor(a),(a*b) / b)
    assertEquals(ceil(b),((a+c) * b) / (a+c))
    assertEquals(floor(a+c),((a+c) * b) / b)
  }

}
