import arithmetic._
import org.junit.Test
import org.junit.Assert._

class TestProd {

  val a: Var = Var("a")
  val b: Var = Var("b")
  val c: Var = Var("c")

  @Test
  def cstTest(): Unit = {
    assertEquals(Cst(2),2*(Cst(3) pow -1)*3)
    assertEquals(Cst(2),(Cst(3) pow -1)*2*3)
    assertEquals(Cst(2),2*3*(Cst(3) pow -1))
    val a = Var("a")
    val n = Cst(8)
    val c = Var("c")
    assertEquals(a,(c*(n pow -1)*a) /^ (c*(n pow -1)))
  }

  @Test
  def trivialDiv(): Unit = {
    val a = Var("a")
    assertEquals(Cst(1), a /^ a)
    assertEquals(a, (a * a) /^ a)
    assertEquals(a, a * (a /^ a))
    assertEquals(a, (a pow 3) /^ (a pow 2))
  }

  @Test
  def prodOfFractions(): Unit = {
    val v = Var("v")
    assertEquals(v /^ 8, (v /^ 2) /^ 4)
    assertEquals(v /^ 8, (v /^ 2) * (Cst(1) /^ 4))
  }

  @Test
  def powWithProd(): Unit = {
    val a = Var("a")
    val b = Var("b")
    val c = Var("c")
    val e1 = (a pow 2) * (b pow 2)
    assertEquals(e1 * a, (a pow 3) * (b pow 2))
    assertEquals(e1 /^ a, a * (b pow 2))
    assertEquals(e1 /^ (a*b), a*b)
    assertEquals(e1 /^ (a*a*b), b)
    assertEquals(e1 /^ ((a pow 3) * (b pow 2)), Cst(1) /^ a)

    assertEquals(c, (a*b*c) /^ (a*b))
    assertEquals((a*b*b*c) /^ ((b pow 2)*a), c)

    // Make this pass
    assertEquals(((a*b*c) pow 2) /^ ((b*c) pow 2), a pow 2)
  }

}
