import arithmetic._
import org.junit.Test
import org.junit.Assert._

class TestIntDiv {

  val c: Cst = Cst(2)
  val a: Var = Var("a")
  val i: Var = Var("i")
  val j: Var = Var("j")
  val k: Var = Var("k")
  val m: Var = Var("m")
  val n: Var = Var("n")

  @Test
  def intDivOne(): Unit = {
    assertEquals(m*n, (m*n)/1)
  }

  @Test
  def f1(): Unit = {
    assertEquals((c*n + m*n) / (c+m), n)
  }

  @Test
  def f23(): Unit = {
    assertEquals((i+c*a + m*a) / (c+m), a + i / (c+m))
    assertEquals((i+k+c*a + m*a) / (c+m), a + (i+k) / (c+m))
    assertEquals((i+c*(a pow 2) + m*(a pow 2)) / (c+m), (a pow 2) + i / (c+m))
    assertEquals((i+k+c*(a pow 2) + m*(a pow 2)) / (c+m), (a pow 2) + (i+k) / (c+m))
  }

  @Test
  def f4(): Unit = {
    assertEquals((j+c*k+c*i+n*i+n*k) / (c+n), i + k + (j / (c+n)))
    assertEquals((1+j+c*k+c*i+n*i+n*k) / (c+n), i + k + ((j+1) / (c+n)))
  }

  @Test
  def f5(): Unit = {
    assertEquals((3+n+j+2*i+n*i) / (2+n), i+1+(1+j) / (2+n))
    assertEquals((4+n+j+2*i+n*i) / (2+n), i+1+(2+j) / (2+n))
  }

  @Test
  def f6(): Unit = {
    assertEquals((4+i+2*m+2*j+m*j) / (2+m), 2+j+i/(2+m))
    assertEquals((5+i+2*m+2*j+m*j) / (2+m), 2+j+(1+i)/(2+m))
  }

  @Test
  def f7(): Unit = {
    assertEquals((2+n+j) / (2+n), 1+j/(2+n))
    assertEquals((3+n+j) / (2+n), 1+(1+j)/(2+n))
  }

  @Test
  def f8(): Unit = {
    assertEquals((4+j+2*n) / (2+n), 2+j/(2+n))
    assertEquals((5+j+2*n) / (2+n), 2+(1+j)/(2+n))
  }

  @Test
  def custom() : Unit = {
    assertEquals((7+2*n) / (3+n), Cst(2))
    assertEquals((7+3*n + 2*i) / (3+n), 2 + (1+n+2*i) / (3+n))
    assertEquals((7+3*n+2*i+i*n) / (2+n), 3+i)
    assertEquals((7+3*n+3*i+2*i*n) / (2+n), 3+i + (1+i+i*n) / (2+n))
    assertEquals((i+3*k+n*k+j*k) / (3+n+j), k + i / (3+n+j))
    assertEquals((10+2*n+3*j) / (3+n+j), 2 + (4+j) / (3+n+j))
    assertEquals((4+2*n+3*i+3*i*n) / (3+2*n), (1+i) + (1+i*n) / (3+2*n))
  }
}
