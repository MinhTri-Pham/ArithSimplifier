import arithmetic._
import org.junit.Test
import org.junit.Assert._

class TestFloorCeil {

  @Test
  // Taking out integer terms out of sums
  def basicInt(): Unit = {
    val a = Var("a",isInt = true)
    val b = Var("b",isInt = true)
    val c = Var("c") //Not an integer
    val d = Var("d")
    assertEquals(floor(a+b), a+b)
    assertEquals(ceil(a+b+c), a+b+CeilingFunction(c))
    assertEquals(floor(a*b+c), a*b+FloorFunction(c))
    assertEquals(ceil(a+c*d), a + CeilingFunction(c*d))
  }
  @Test
  def cstTest(): Unit = {
    val a = Var("a", isInt = true)
    assertEquals(Cst(2)*a, floor((Cst(6)*a) /^ Cst(3)))
    assertEquals(Cst(2), ceil((Cst(6)*a) /^ (Cst(3)*a)))
    assertEquals(Cst(3)*a, floor((Cst(9)*a + Cst(2)) /^ Cst(3)))
    assertEquals(Cst(3)*a + Cst(1), ceil((Cst(9)*a + Cst(2)) /^ Cst(3)))
  }

  @Test
  // Based on min and max
  def intervalTest(): Unit = {
    val x = Var("x", Interval(Cst(6), Cst(7)))
    val y = Var("y", Interval(Cst(8), Cst(10)))
    val z = Var("z", isInt = true)
    val expr = z + x /^ y
    assertEquals(z, floor(expr))
    assertEquals(Cst(1),ceil(expr) - floor(expr))
  }

  @Test
  def nested(): Unit = {
    val x = Var("x", Interval(Cst(6), Cst(7)))
    val y = Var("y", Interval(Cst(8), Cst(10)))
    // Should be the innermost function
    assertEquals(ceil(ceil(floor(ceil(floor(x))))),floor(x))
    assertEquals(ceil(ceil(floor(ceil(floor(y /^ x))))),Cst(1))
  }

  @Test
  def sumTest(): Unit = {
    val a = Var("a", isInt = true)
    val b = Var("b")
    val c = Var("c", isInt = true)
    assertEquals(Cst(2) + (c pow 2) + FloorFunction(a*b),
      floor(a*b + Cst(8)*(Cst(3) pow -1) + (c pow 2)))
    assertEquals(Cst(3) + (c pow 2) + CeilingFunction(a*b),
      ceil(a*b + Cst(8)*(Cst(3) pow -1) + (c pow 2)))
  }

  @Test
  def factorise(): Unit = {
    val a = Var("a")
    val b = Var("b")
    val c = Var("c", isInt = true)
    val d = Var("d", isInt = true)
    assertEquals(c+d, floor((a*c+a*d) /^ a))
    assertEquals(c+CeilingFunction(b), ceil((a*b+a*c) /^ a))
    assertEquals(c+d, floor((a*c+a*d+b*c+b*d) /^ (a+b)))
  }

  @Test
  def sumProd(): Unit = {
    val a = Var("a")
    val b = Var("b")
    val c = Var("c", isInt = true)
    val d = Var("d", isInt = true)
    val e = Var("e")
    assertEquals(Cst(1) + c + CeilingFunction(b /^ a), ceil((a+a*c+b) /^ a))
    assertEquals(c+FloorFunction((a+b) pow -1), floor((a*c+b*c+Cst(1)) /^ (a+b)))
//    assertEquals(c+CeilingFunction(d * ((a+b) pow -1)), ceil((a*c+b*c+d) /^ (a+b)))
//    assertEquals(c+FloorFunction(d * ((a+b) pow -1) + e * ((a+b) pow -1)), floor((a*c+b*c+d+e) /^ (a+b)))
  }
}
