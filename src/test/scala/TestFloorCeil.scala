import arithmetic._
import org.junit.Test
import org.junit.Assert._

class TestFloorCeil {

  val a: Var = Var("a")
  val b: Var = Var("b")
  val c: Var = Var("c", isInt = false) //Not an integer
  val d: Var = Var("d", isInt = false) //Not an integer
  val e: Var = Var("e", isInt = false) //Not an integer


  @Test
  // Taking out integer terms out of sums
  def basicInt(): Unit = {
    assertEquals(floor(a+b), a+b)
    assertEquals(ceil(a+b+c), a+b+ceil(c))
    assertEquals(floor(a*b+c), a*b+floor(c))
    assertEquals(ceil(a+b*c), a + ceil(b*c))
  }
  @Test
  def cstTest(): Unit = {
    assertEquals(2*a, floor((6*a) /^ 3))
    assertEquals(Cst(2), ceil((6*a) /^ (3*a)))
    assertEquals(2*a + 1, floor((6*a + 4) /^ 3))
    assertEquals(2*a + 2, ceil((6*a + 4) /^ 3))
  }

  @Test
  def sumTest(): Unit = {
    assertEquals(1 + a, floor(a + 1 + Cst(2) /^ Cst(3)))
    assertEquals(1 + floor(c + Cst(2) /^ Cst(3)), floor(1 + c + Cst(2) /^ Cst(3)))
    assertEquals(1 + a + floor(c + Cst(2) /^ Cst(3)), floor(1 + a + c + Cst(2) /^ Cst(3)))

    assertEquals(2 + a, ceil(a + 1 + Cst(2) /^ Cst(3)))
    assertEquals(1 + ceil(c + Cst(2) /^ Cst(3)), ceil(c + 1 + Cst(2) /^ Cst(3)))
    assertEquals(1 + a + ceil(c + Cst(2) /^ Cst(3)), ceil(1 + a + c + Cst(2) /^ Cst(3)))
  }

  @Test
  def factorise(): Unit = {
    assertEquals(a+b, floor((c*a+c*b) /^ c))
    assertEquals(a+ceil(d), ceil((c*d+c*a) /^ c))
    assertEquals(a+b, floor((c*a+c*b+d*a+d*b) /^ (c+d)))
  }

  @Test
  def constFactorisation(): Unit = {
    val n = Cst(2)
    assertEquals(n+a + floor(c /^ (n+d)),floor((4 + c + n*d + n*a + d*a) /^ (n+d)))
    assertEquals(n+a + floor((c+1) /^ (n+d)),floor((5 + c + n*d + n*a + d*a) /^ (n+d)))
    assertEquals(n+a + ceil((3*c) /^ (n+d)),ceil((4 + 3*c + n*d + n*a + d*a) /^ (n+d)))
  }

  @Test
  def sumProd(): Unit = {
    assertEquals(1+ a + ceil(d /^ c), ceil((c+c*a+d) /^ c))
    assertEquals(a+floor((c+d) pow -1), floor((c*a+d*a+Cst(1)) /^ (c+d)))
    assertEquals(a+ceil(b * ((c+d) pow -1)), ceil((c*a+d*a+b) /^ (c+d)))
    assertEquals(a+floor((b+e)*((c+d) pow -1)), floor((c*a+d*a+b+e) /^ (c+d)))
  }

  @Test
  def partitionConstantMultiple(): Unit = {
    assertEquals(1 + floor((a+c) /^ (c+d)), floor((2*c+d+a) /^ (c+d)))
    assertEquals(2 + ceil((c+d)/^(c+2*d)),ceil((3*c + 5*d) /^ (c + 2*d)))
    assertEquals(a+b+floor((1+a*d) /^(c+d)) ,floor((1 +c*a+2*d*a+c*b+d*b) /^ (c+d)))
  }

  @Test
  def partitionConstant(): Unit = {
    assertEquals(1 + ceil((2+a) /^ (2+c)), ceil((4+c+a) /^ (2+c)))
    assertEquals(2 + floor(a /^(2+c)),floor((4+2*c+a) /^ (2+c)))
    assertEquals(1 + a + ceil((2+a) /^ (2+c)),ceil((4+c+3*a+c*a) /^ (2+c)))
  }
}
