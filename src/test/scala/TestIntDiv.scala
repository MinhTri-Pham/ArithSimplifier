import arithmetic._
import org.junit.Test
import org.junit.Assert._

class TestIntDiv {

  @Test
  def cstVarProdDenom() : Unit = {
    val a = Var("a", isInt = true)
    assertEquals(Cst(6)*a / Cst(3), Cst(2)*a)
    assertEquals((Cst(6)*a) / (Cst(3)*a), Cst(2))
    assertEquals((Cst(6)*a + Cst(2)) / Cst(3), Cst(2)*a)
//    assertEquals((Cst(6)*a + Cst(4)) / Cst(3), Cst(2)*a + Cst(1))
//    val b = Var("b")
//    val c = Var("c")
//    assertEquals((Cst(6)*a + b) / a, Cst(6) + b/a)
//    assertEquals((Cst(4)*a*c + Cst(6)*b*c) / (Cst(2)*c), Cst(2)*a+Cst(3)*b)
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
    val b = Var("b")
    val d = Var("d")
    val a = Cst(2)
    val c = Var("c")
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

  @Test
  def factoriseDenom(): Unit = {
    val a = Var("a")
    val b = Var("b")
    val m = Var("m")
    val n = Var("n")
    val c = Cst(2)
    val x = Var("x")
    val y = Var("y")
    // Important that these two don't involve a or b
    val expr_1 = x+Cst(2)*y
    val expr_2 = x*x*y
    assertEquals(a/n + b/(c*n+m*n),(b + c*a + m*a) / (c*n+m*n))
    assertEquals(expr_1/n + expr_2/(c*n+m*n),(expr_2 + c*expr_1 + m*expr_1) / (c*n+m*n))
  }

  @Test
  def partitionConstantMultiple(): Unit = {
    val a = Var("a", Interval(Cst(2), Cst(3)))
    val b = Var("b", Interval(Cst(6), Cst(8)))
    val c = Cst(2)
    val d = Var("d")
    // Partition as (a+b) / (a+b) + (a+c) / (a+b)
    assertEquals(Cst(1), (Cst(2)*a+b+c) / (a+b))
    // Partition as (2a+4b) / (a+2b) + (a+b)/(a+2b)
    assertEquals(Cst(2),(Cst(3)*a + Cst(5)*b) / (a + Cst(2)*b))
    // Partition as (ac+bc+ad+bd) / (a+b) + ac / (a+b)
    assertEquals(c+d,(Cst(2)*a*c+b*c+a*d+b*d) / (a+b))
  }

  @Test
  def partitionConstant(): Unit = {
    val a = Var("a", Interval(Cst(4), Cst(6)))
    val b = Var("b", Interval(Cst(1), Cst(3)))
    // Partition as (2+a) / (2+a) + 1 / (2+a) = 1
    assertEquals(Cst(1), (Cst(3)+a) / (Cst(2)+a))
    // Partition as (2+a) / (2+a) + (2+b) / (2+a) = 1 (since b < a)
    assertEquals(Cst(1), (Cst(4)+a+b) / (Cst(2)+a))
    // Partition as (4+2a) / (2+a) + (1+b) / (2+a) = 2 (since 1+b < 2+a)
    assertEquals(Cst(2),(Cst(5)+Cst(2)*a+b) / (Cst(2)+a))
    // Partition as (ab+a+2b+2) / (2+a) + (1+b) / (2+a) = 1+b
    assertEquals(Cst(1)+b,(Cst(3)+a+Cst(3)*b+a*b) / (Cst(2)+a))
  }
}
