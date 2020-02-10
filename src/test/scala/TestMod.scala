import arithmetic._
import org.junit.Test
import org.junit.Assert._

class TestMod {

  @Test
  def basicTests(): Unit = {
    val a = Var("a", isInt = true)
    val b = Var("b")
    assertEquals(Cst(2),(6*a + 2) % 3)
    assertEquals(Cst(1), (6*a + 4) % 3)
    assertEquals(b % a, (6*a + b) % a)
  }

  @Test
  def varTest(): Unit = {
    val a = Var("a", isInt = true)
    val b = Var("b", isInt = true)
    val c = Var("c")
    assertEquals(Cst(0),(4*a*c + 6*b*c) % (2*c))
  }

  @Test
  def sumDivisor(): Unit = {
    val a = Var("a", isInt = true)
    val b = Var("b", isInt = true)
    val c = Var("c", isInt = true)
    val d = Var("d", isInt = true)
    val e = Var("e")
    // Trivial cases
    assertEquals(Cst(0),(a*b) % a)
    assertEquals(b % a, (a+b) % a)
    // Direct factorisation
    assertEquals(Cst(0), (a*c+b*c) % (a+b))
    assertEquals(Cst(0),(a*c+b*c+a*d+b*d) % (a+b))
    // Find a good partition of dividend
    assertEquals(c % (a+b), (a+b+c) % (a+b))
    assertEquals(d % (a+b), (a*c+b*c+d) % (a+b))
    assertEquals((d+e) % (a+b), (a*c+b*c+d+e) % (a+b))
    assertEquals(e % (a+b), (a*c+b*c+a*d+b*d+e) % (a+b))
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
  def partitionConstantMultiple(): Unit = {
    val a = Var("a")
    val b = Var("b")
    val c = Var("c", isInt = true)
    val d = Var("d", isInt = true)
    assertEquals(a % (a+b), (2*a+b) % (a+b))
    assertEquals((a+b) % (a+2*b),(3*a + 5*b) % (a + 2*b))
    assertEquals(a*c % (a+b),(2*a*c+b*c+a*d+b*d) % (a+b))
  }

  @Test
  def partitionConstant(): Unit = {
    val a = Var("a")
    val b = Var("b", isInt = true)
    assertEquals(1 % (2+a),(3+a) % (2+a))
    assertEquals((2+b) % (2+a),(4+a+b) % (2+a))
    assertEquals(b % (2+a),(4+2*a+b) % (2+a))
    assertEquals((2+b) % (2+a),(4+a+3*b+a*b) % (2+a))
    val c = Var("c", isInt = true)
    assertEquals((1+b) % (2+a),(3+a+b+2*c+c*a) % (2+a))
  }
}
