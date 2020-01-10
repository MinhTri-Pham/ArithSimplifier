import arithmetic._
import org.junit.Test
import org.junit.Assert._

class TestIntDiv {

  @Test
  def cstVarProdDenom() : Unit = {
    assertEquals(Cst(6) / Cst(3), Cst(2))
    assertEquals(Cst(7) / Cst(3), Cst(2))
    val a = Var("a")
    assertEquals(Cst(6)*a / Cst(3), Cst(2)*a)
    assertEquals((Cst(6)*a) / (Cst(3)*a), Cst(2))
    assertEquals((Cst(6)*a + Cst(2)) / Cst(3), Cst(2)*a)
    assertEquals((Cst(6)*a + Cst(4)) / Cst(3), Cst(2)*a + Cst(1))
    val b = Var("b")
    val c = Var("c")
    assertEquals((Cst(6)*a + b) / a, Cst(6) + b/a)
    assertEquals((Cst(4)*a*c + Cst(6)*b*c) / (Cst(2)*c), Cst(2)*a+Cst(3)*b)
  }

  @Test
  def intervalTest(): Unit = {
    val a = Var("a",Interval(Cst(1), Cst(2)))
    val b = Var("b", Interval(Cst(2), Cst(3)))
    val c = Var("c",Interval(Cst(7), Cst(8)))
    val d = Var("d", Interval(Cst(10), Cst(11)))
    assertEquals(a / c, Cst(0))
    assertEquals((a+b) / c, Cst(0))
    assertEquals(d / c, Cst(1))
    assertEquals(Cst(2)*c / (a+d), Cst(1))
    assertEquals((Cst(5) + Cst(4)*b) / d, Cst(1))
  }

  @Test
  def sumDenom(): Unit = {
    val a = Var("a")
    val b = Var("b")
    val c = Var("c")
    val d = Var("d")
    val e = Var("e")
    // Direct factorisation
    assertEquals((a*c+b*c) / (a+b), c)
    assertEquals((a*c+b*c+a*d+b*d) / (a+b), c+d)
    // Find a good partition
    assertEquals((a+b+c) / (a+b), Cst(1) + c/(a+b))
    assertEquals((a*c+b*c+d) / (a+b), c + d/(a+b))
    assertEquals((a*c+b*c+d+e) / (a+b), c + (d+e)/(a+b))
    assertEquals((a*c+b*c+a*d+b*d+e) / (a+b), c + d + e / (a+b))
  }

  @Test
  def constSum(): Unit = {
    val c = Cst(5)
    val x = Var("x")
    val y = Var("y")
    assertEquals(Cst(1) + (y / (c+x)), (c+x+y) / (c+x))
  }

  @Test
  def constFactorisation(): Unit = {
    val c = Cst(2)
    val x = Var("x")
    val y = Var("y")
    val m = Var("m")
    assertEquals((Cst(4) + x + c*m + c*y + m*y) / (c+m), c+y + (x / (c+m)))
  }
}
