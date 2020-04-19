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

  @Test
  def elemSumTest(): Unit = {
    val a = Var("a", isInt = true)
    assertEquals(-2*a-1,(6*a+4) / -3)
    assertEquals(Cst(1),(6*a+4) % -3)
  }

  @Test
  def rangeTest() : Unit = {
    val a = Var("a", Range(0,2))
    val b = Var("b", Range(0,2))
    val c1 = Var("c1", Range(1,4))
    val c2 = Var("c2", Range(-7,-5))
    val posNumer = a*b + c1
    val negNumer = a*b + c2
    assertEquals(posNumer / a, b + floor(c1 /^a))
    assertEquals(negNumer / a, b + ceil(c2 /^a))
    assertEquals(posNumer % a, c1 - a*floor(c1 /^a))
    assertEquals(negNumer % a, c2 - a*ceil(c2 /^a))
  }

}
