import arithmetic._
import org.junit.Assert._
import org.junit.Test

class TestMod {

  val c: Cst = Cst(2)
  val a: Var = Var("a")
  val i: Var = Var("i")
  val j: Var = Var("j")
  val k: Var = Var("k")
  val m: Var = Var("m")
  val n: Var = Var("n")

  @Test
  def intDivOne(): Unit = {
    assertEquals(Cst(0), a % 1)
  }

  @Test
  def f1(): Unit = {
    assertEquals((c*n + m*n) % (c+m), Cst(0))
  }

  @Test
  def f23(): Unit = {
    assertEquals((i+c*a + m*a) % (c+m), i % (c+m))
    assertEquals((i+k+c*a + m*a) % (c+m), (i+k) % (c+m))
    assertEquals((i+c*(a pow 2) + m*(a pow 2)) % (c+m), i % (c+m))
    assertEquals((i+k+c*(a pow 2) + m*(a pow 2)) % (c+m), (i+k) % (c+m))
  }

  @Test
  def f4(): Unit = {
    assertEquals((j+c*k+c*i+n*i+n*k) % (c+n), j % (c+n))
    assertEquals((1+j+c*k+c*i+n*i+n*k) % (c+n), (1+j) % (c+n))
  }

  @Test
  def f5(): Unit = {
    assertEquals((3+n+j+2*i+n*i) % (2+n), (1+j) % (2+n))
    assertEquals((4+n+j+2*i+n*i) % (2+n), (2+j) % (2+n))
  }

  @Test
  def f6(): Unit = {
    assertEquals((4+i+2*m+2*j+m*j) % (2+m), i % (2+m))
    assertEquals((5+i+2*m+2*j+m*j) % (2+m), (1+i) % (2+m))
  }

  @Test
  def f7(): Unit = {
    assertEquals((2+n+j) % (2+n), j % (2+n))
    assertEquals((3+n+j) % (2+n), (1+j) % (2+n))
  }

  @Test
  def f8(): Unit = {
    assertEquals((4+j+2*n) % (2+n), j % (2+n))
    assertEquals((5+j+2*n) % (2+n), (1+j) % (2+n))
  }

  @Test
  def custom() : Unit = {
        assertEquals((7+2*n) % (3+n), Cst(1))
    assertEquals((7+3*n + 2*i) % (3+n), (1+n+2*i) % (3+n))
        assertEquals((7+3*n+2*i+i*n) % (2+n), Cst(1))
    assertEquals((7+3*n+3*i+2*i*n) % (2+n), (1+i+i*n) % (2+n))
    assertEquals((i+3*k+n*k+j*k) % (3+n+j), i % (3+n+j))
    assertEquals((10+2*n+3*j) % (3+n+j), (4+j) % (3+n+j))
    assertEquals((4+2*n+3*i+3*i*n) % (3+2*n), (1+i*n) % (3+2*n))
  }

  @Test
  def nested(): Unit = {
    assertEquals(j % i, (j % i) % i)
  }

  @Test
  def sumTest() : Unit = {
    val n = Cst(8)
    val c_1 = Cst(3)
    assertEquals(i, i / n * n + i % n)
    assertEquals(i*c_1, i*c_1 / n * n + i*c_1 % n)
    assertEquals(i*c_1+j*c_1, (i+j)*c_1 / n * n + (i+j)*c_1 % n)
    val c_2 = Cst(4) // Note c and n aren't coprime
    assertEquals(i, i / n * n + i % n)
    assertEquals(i*c_2, i*c_2 / n * n + i*c_2 % n)
    assertEquals(i*c_2+j*c_2, (i+j)*c_2 / n * n + (i+j)*c_2 % n)
  }

  @Test
  def divPlusModOfSumMultipliedConstants(): Unit = {
    val x = Cst(8)
    val expr_1 = x * 4 * (i+j)
    val expr_2 = x * 4 * (i+j) / 16 * 16 + x * 4 * (i + j) % 16
    assertEquals(expr_1, expr_2.toProd.get)
  }

  @Test
  def sumDivModSimplification(): Unit = {
    assertNotEquals(i % 8, ((4*j + i) % 8) * 1)
    assertEquals(4*j + i, (4*j + i) % 8 + ((4*j + i) / 8) * 8)
    assertEquals(4*j + i, 0 + (((4*j + i) / 8) * Cst(8) * Cst(1)) +
      (((4*j + i) % Cst(8)) * Cst(1)))
  }

  @Test
  def sumWithMod(): Unit = {
    val expr_1 = j+k
    val expr_2 = j*k
    assertEquals(i % c, (i % c + expr_1 * c) % c)
    assertEquals(i % c, (i % c + expr_1 * c + expr_2 * c) % c)
  }
}
