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
    assertEquals(2*a, floor((6*a) /^ Cst(3)))
    assertEquals(Cst(2), ceil((6*a) /^ (3*a)))
    assertEquals(2*a, floor((6*a + 2) /^ 3))
    assertEquals(2*a + 1, ceil((6*a + 2) /^ 3))
  }

  @Test
  def sumTest(): Unit = {
    val a = Var("a", isInt = true)
    assertEquals(1 + a, floor(a + 1 + Cst(2) /^ Cst(3)))
    val b = Var("b", isInt = false)
    assertEquals(1 + floor(b + Cst(2) /^ Cst(3)), floor(b + 1 + Cst(2) /^ Cst(3)))
    assertEquals(1 + a + floor(b + Cst(2) /^ Cst(3)), floor(1 + a + b + Cst(2) /^ Cst(3)))

    assertEquals(2 + a, ceil(a + 1 + Cst(2) /^ Cst(3)))
    assertEquals(1 + ceil(b + Cst(2) /^ Cst(3)), ceil(b + 1 + Cst(2) /^ Cst(3)))
    assertEquals(1 + a + ceil(b + Cst(2) /^ Cst(3)), ceil(1 + a + b + Cst(2) /^ Cst(3)))
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
  def constFactorisation(): Unit = {
    val c = Cst(2)
    val x = Var("x")
    val y = Var("y", isInt = true)
    val m = Var("m")
    assertEquals(c+y + floor(x /^ (c+m)),floor((4 + x + c*m + c*y + m*y) /^ (c+m)))
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
    assertEquals(c+CeilingFunction(d * ((a+b) pow -1)), ceil((a*c+b*c+d) /^ (a+b)))
    assertEquals(c+FloorFunction(d*((a+b) pow -1)+e*((a+b) pow -1)), floor((a*c+b*c+d+e) /^ (a+b)))
  }

  @Test
  def partitionConstantMultiple(): Unit = {
    val a = Var("a")
    val b = Var("b")
    val c = Var("c", isInt = true)
    val d = Var("d", isInt = true)
    // Partition as floor((a+b) /^ (a+b)) + floor((a+c) /^ (a+b))
    assertEquals(1 + floor((a+c) /^ (a+b)), floor((2*a+b+c) /^ (a+b)))
    // Partition as ceil((2a+4b) /^ (a+2b)) + ceil((a+b) /^ (a+2b))
    assertEquals(2 + ceil((a+b)/^(a+2*b)),ceil((3*a + 5*b) /^ (a + 2*b)))
    // Partition as floor((ac+bc+ad+bd) /^ (a+b)) + floor(bc / (a+b))
    assertEquals(c+d+floor((b*c) /^(a+b)) ,floor((a*c+2*b*c+a*d+b*d) /^ (a+b)))
  }

  @Test
  def partitionConstant(): Unit = {
    val a = Var("a")
    val b = Var("b", isInt = true)
    // Partition as floor((2+a) /^ (2+a)) + floor(1 / (2+a))
    assertEquals(1 + floor(Cst(1) /^ (2+a)), floor((3+a) /^ (2+a)))
    // Partition as ceil((2+a) /^ (2+a)) + ceil((2+b) / (2+a))
    assertEquals(1 + ceil((2+b) /^ (2+a)), ceil((4+a+b) /^ (2+a)))
    // Partition as floor((4+2a) /^ (2+a)) + floor(b / (2+a))
    assertEquals(2 + floor(b /^(2+a)),floor((4+2*a+b) /^ (2+a)))
    // Partition as ceil((ab+a+2b+2) /^ (2+a)) + ceil((2+b) /^ (2+a))
    assertEquals(1 + b + ceil((2+b) /^ (2+a)),ceil((4+a+3*b+a*b) /^ (2+a)))
  }
}
