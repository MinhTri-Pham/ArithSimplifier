import arithmetic._
import org.junit.Test
import org.junit.Assert._

class TestIntDiv {

  @Test
  def intDivOne(): Unit = {
    val x = Var("x", isInt = true)
    assertEquals(x, x/1)
  }

  @Test
  def f1(): Unit = {
    val c = Cst(2)
    val m = Var("m")
    val n = Var("n", isInt = true)
    assertEquals((c*n + m*n) / (c+m), n)
  }

  @Test
  def f23(): Unit = {
    val c = Cst(2)
    val a = Var("a", isInt = true)
    val i = Var("i")
    val k = Var("k")
    val m = Var("m")
    assertEquals((i+c*a + m*a) / (c+m), a + i / (c+m))
    assertEquals((i+k+c*a + m*a) / (c+m), a + (i+k) / (c+m))
    assertEquals((i+c*(a pow 2) + m*(a pow 2)) / (c+m), (a pow 2) + i / (c+m))
    assertEquals((i+k+c*(a pow 2) + m*(a pow 2)) / (c+m), (a pow 2) + (i+k) / (c+m))
  }

  @Test
  def f4(): Unit = {
    val c = Cst(2)
    val i = Var("i", isInt = true)
    val j = Var("j")
    val k = Var("k", isInt = true)
    val n = Var("n")
    assertEquals((j+c*k+c*i+n*i+n*k) / (c+n), i + k + (j / (c+n)))
    assertEquals((1+j+c*k+c*i+n*i+n*k) / (c+n), i + k + ((j+1) / (c+n)))
  }

  @Test
  def f5(): Unit = {
    val i = Var("i", isInt = true)
    val j = Var("j")
    val n = Var("n")
    assertEquals((3+n+j+2*i+n*i) / (2+n), i+1+(1+j) / (2+n))
    assertEquals((4+n+j+2*i+n*i) / (2+n), i+1+(2+j) / (2+n))
  }

  @Test
  def f6(): Unit = {
    val i = Var("i")
    val j = Var("j", isInt = true)
    val m = Var("m")
    assertEquals((4+i+2*m+2*j+m*j) / (2+m), 2+j+i/(2+m))
    assertEquals((5+i+2*m+2*j+m*j) / (2+m), 2+j+(1+i)/(2+m))
  }

  @Test
  def f7(): Unit = {
    val j = Var("j")
    val n = Var("n")
    assertEquals((2+n+j) / (2+n), 1+j/(2+n))
    assertEquals((3+n+j) / (2+n), 1+(1+j)/(2+n))
  }

  @Test
  def f8(): Unit = {
    val j = Var("j")
    val n = Var("n")
    assertEquals((4+j+2*n) / (2+n), 2+j/(2+n))
    assertEquals((5+j+2*n) / (2+n), 2+(1+j)/(2+n))
  }

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
  def custom() : Unit = {
    val i = Var("i", isInt = true)
    val m = Var("m")
    assertEquals((7+3*m+2*i+i*m) / (2+m), 3+i+(1 / (2+m)))
    assertEquals((7+3*m+3*i+2*i*m) / (2+m), 3+i + (1+i+i*m) / (2+m))
  }


}
