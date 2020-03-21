import arithmetic._
import org.junit.Assert._
import org.junit.Test

class TestMod {

  @Test
  def intDivOne(): Unit = {
    val x = Var("x", isInt = true)
    assertEquals(Cst(0), x % 1)
  }

  @Test
  def f1(): Unit = {
    val c = Cst(2)
    val m = Var("m")
    val n = Var("n", isInt = true)
    assertEquals((c*n + m*n) % (c+m), Cst(0))
  }

  @Test
  def f23(): Unit = {
    val c = Cst(2)
    val a = Var("a", isInt = true)
    val i = Var("i")
    val k = Var("k")
    val m = Var("m")
    assertEquals((i+c*a + m*a) % (c+m), i % (c+m))
    assertEquals((i+k+c*a + m*a) % (c+m), (i+k) % (c+m))
    assertEquals((i+c*(a pow 2) + m*(a pow 2)) % (c+m), i % (c+m))
    assertEquals((i+k+c*(a pow 2) + m*(a pow 2)) % (c+m), (i+k) % (c+m))
  }

  @Test
  def f4(): Unit = {
    val c = Cst(2)
    val i = Var("i", isInt = true)
    val j = Var("j")
    val k = Var("k", isInt = true)
    val n = Var("n")
    assertEquals((j+c*k+c*i+n*i+n*k) % (c+n), j % (c+n))
    assertEquals((1+j+c*k+c*i+n*i+n*k) % (c+n), (1+j) % (c+n))
  }

  @Test
  def f5(): Unit = {
    val i = Var("i", isInt = true)
    val j = Var("j")
    val n = Var("n")
    assertEquals((3+n+j+2*i+n*i) % (2+n), (1+j) % (2+n))
    assertEquals((4+n+j+2*i+n*i) % (2+n), (2+j) % (2+n))
  }

  @Test
  def f6(): Unit = {
    val i = Var("i")
    val j = Var("j", isInt = true)
    val m = Var("m")
    assertEquals((4+i+2*m+2*j+m*j) % (2+m), i % (2+m))
    assertEquals((5+i+2*m+2*j+m*j) % (2+m), (1+i) % (2+m))
  }

  @Test
  def f7(): Unit = {
    val j = Var("j")
    val n = Var("n")
    assertEquals((2+n+j) % (2+n), j % (2+n))
    assertEquals((3+n+j) % (2+n), (1+j) % (2+n))
  }

  @Test
  def f8(): Unit = {
    val j = Var("j")
    val n = Var("n")
    assertEquals((4+j+2*n) % (2+n), j % (2+n))
    assertEquals((5+j+2*n) % (2+n), (1+j) % (2+n))
  }

  @Test
  def nested(): Unit = {
    val a = Var("a")
    val b = Var("b")
    assertEquals(b % a, (b % a) % a)
  }

  @Test
  def sumCoprimeDivMod() : Unit = {
    val a = Var("a")
    val b = Var("b")
    val n = Cst(8)
    val c = Cst(3) // Note c and n are coprime
    assertEquals(a, a / n * n + a % n)
    assertEquals(a*c, a*c / n * n + a*c % n)
    assertEquals(a*c+b*c, (a+b)*c / n * n + (a+b)*c % n)
  }

  @Test
  def sumNonCoprimeDivMod(): Unit = {
    val a = Var("a")
    val b = Var("b")
    val n = Cst(8)
    val c = Cst(4) // Note c and n aren't coprime
    assertEquals(a, a / n * n + a % n)
    assertEquals(a*c, a*c / n * n + a*c % n)
    assertEquals(a*c+b*c, (a+b)*c / n * n + (a+b)*c % n)
  }

  @Test
  def divPlusModOfSumMultipliedConstants(): Unit = {
    val a = Var("a")
    val b = Var("b")
    val x = Cst(8)
    val expr_1 = x * 4 * (a+b)
    val expr_2 = x * 4 * (a+b) / 16 * 16 + x * 4 * (a + b) % 16
    assertEquals(expr_1, expr_2.toProd.get)
  }

  @Test
  def sumDivModSimplification(): Unit = {
    val a = Var("a")
    val b = Var("b")
    assertNotEquals(a % 8, ((4*b + a) % 8) * 1)
    assertEquals(4*b + a, (4*b + a) % 8 + ((4*b + a) / 8) * 8)
    assertEquals(4*b + a, 0 + (((4*b + a) / 8) * Cst(8) * Cst(1)) +
      (((4*b + a) % Cst(8)) * Cst(1)))
  }

  @Test
  def sumWithMod(): Unit = {
    val a = Var("a", isInt = true)
    val b = Var("b", isInt = true)
    val c = Var("c", isInt = true)
    val expr_1 = b+c
    val expr_2 = b*c
    val n = Cst(2)
    assertEquals(a % n, (a % n + expr_1 * n) % n)
    assertEquals(a % n, (a % n + expr_1 * n + expr_2 * n) % n)
  }

  @Test
  def custom() : Unit = {
    val i = Var("i", isInt = true)
    val n = Var("n")
    assertEquals((7+2*n) % (3+n), 1 % (3+n))
    assertEquals((7+3*n + 2*i) % (3+n), (1+n+2*i) % (3+n))
    assertEquals((7+3*n+2*i+i*n) % (2+n), 1 % (2+n))
    assertEquals((7+3*n+3*i+2*i*n) % (2+n), (1+i+i*n) % (2+n))
    val j = Var("j")
    val k = Var("k", isInt = true)
    assertEquals((i+3*k+n*k+j*k) % (3+n+j), i % (3+n+j))
    assertEquals((10+2*n+3*j) % (3+n+j), (4+j) % (3+n+j))

    assertEquals((4+2*n+3*i+3*i*n) % (3+2*n), (1+i*n) % (3+2*n))

  }

}
