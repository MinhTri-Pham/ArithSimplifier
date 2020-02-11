import arithmetic._
import org.junit.Test
import org.junit.Assert._

class TestIntDiv {

  @Test
  def cstVarProdDenom() : Unit = {
    val a = Var("a", isInt = true)
    assertEquals(2*a,6*a / 3)
    assertEquals(Cst(2),(6*a) / (3*a))
    assertEquals(2*a, (6*a + 2) / 3)
    assertEquals(2*a + 1, (6*a + 4) / 3)
    val b = Var("b")
    val c = Var("c")
    assertEquals(6 + floor(b/^a), (6*a+b) / a)
    assertEquals(2*a + floor(3*b), (4*a*c + 6*b*c) / (2*c))
  }

  @Test
  def sumDenom(): Unit = {
    val a = Cst(2)
    val b = Var("b")
    val c = Var("c", isInt = true)
    val d = Var("d", isInt = true)
    val e = Var("e")
    // Direct factorisation
    assertEquals(c, (a*c+b*c) / (a+b))
    assertEquals((a*c+b*c+a*d+b*d) / (a+b), c+d)
    // Find a good partition
    assertEquals(1 + c/(a+b), (a+b+c) / (a+b))
    assertEquals(c + floor(d/^(a+b)), (a*c+b*c+d) / (a+b))
    assertEquals(c + floor((d+e)/^(a+b)), (a*c+b*c+d+e) / (a+b))
    assertEquals(c + d + floor(e /^ (a+b)), (a*c+b*c+a*d+b*d+e) / (a+b))
  }

  @Test
  def constSum(): Unit = {
    val c = Cst(5)
    val x = Var("x")
    val y = Var("y")
    assertEquals(1 + floor(y /^ (c+x)), (c+x+y) / (c+x))
  }

  @Test
  def constFactorisation(): Unit = {
    val c = Cst(2)
    val x = Var("x")
    val y = Var("y", isInt = true)
    val m = Var("m")
    assertEquals(c + y + floor(x /^ (c+m)), (4 + x + c*m + c*y + m*y) / (c+m))
  }

  @Test
  def partitionConstantMultiple(): Unit = {
    val a = Var("a")
    val b = Var("b")
    val c = Var("c", isInt = true)
    val d = Var("d", isInt = true)
    assertEquals(1 + floor((a+c) /^ (a+b)), (2*a+b+c) / (a+b))
    assertEquals(2 + floor((a+b)/^(a+2*b)),(3*a + 5*b) / (a + 2*b))
    assertEquals(c+d+floor((a*c) /^(a+b)) ,(2*a*c+b*c+a*d+b*d) / (a+b))
  }

  @Test
  def partitionConstant(): Unit = {
    val a = Var("a")
    val b = Var("b", isInt = true)
    assertEquals(1 + floor(Cst(1) /^ (2+a)), (3+a) / (2+a))
    assertEquals(1 + floor((2+b) /^ (2+a)), (4+a+b) / (2+a))
    assertEquals(2 + floor(b /^(2+a)), (4+2*a+b) / (2+a))
    assertEquals(1 + b + floor((2+b) /^ (2+a)), (4+a+3*b+a*b) / (2+a))
  }
}
