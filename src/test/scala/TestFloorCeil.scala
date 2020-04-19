import arithmetic._
import org.junit.Test
import org.junit.Assert._

class TestFloorCeil {

  val a: Var = Var("a")
  val b: Var = Var("b")
  val c: Var = Var("c", isInt = false)
  val d: Var = Var("d", isInt = false)
  val e: Var = Var("e", isInt = false)


  @Test
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
    assertEquals(a+b, floor(((a pow 2)+ a*b+a*c+b*c) /^ (a+c)))
    assertEquals(a+b, ceil((c*a+c*b+d*a+d*b) /^ (c+d)))
  }

  @Test
  def partialFactorise(): Unit = {
    val n = Cst(2)
    assertEquals(n+a + floor(c /^ (n+d)),floor((4 + c + n*d + n*a + d*a) /^ (n+d)))
    assertEquals(n+a + floor((c+1) /^ (n+d)),floor((5 + c + n*d + n*a + d*a) /^ (n+d)))
    assertEquals(n+a + ceil((3*c) /^ (n+d)),ceil((4 + 3*c + n*d + n*a + d*a) /^ (n+d)))
    assertEquals(a+b+ceil((1+a*d) /^  (c+d)), ceil((1+a*c+2*a*d+b*c+b*d) /^ (c+d)))
    assertEquals(1+b+floor(a*b /^ (a+d)), floor((a + d + 2*a*b + b*d) /^ (a+d)))
  }
}
