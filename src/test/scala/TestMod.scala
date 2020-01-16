import arithmetic._
import org.junit.Test
import org.junit.Assert._

class TestMod {

  @Test
  def basicTests(): Unit = {
    assertEquals(Cst(6) % Cst(3), Cst(0))
    assertEquals(Cst(7) % Cst(3), Cst(1))
    val a = Var("a")
    val b = Var("b")
    assertEquals((Cst(6)*a + Cst(2)) % Cst(3), Cst(2))
    assertEquals((Cst(6)*a + Cst(4)) % Cst(3), Cst(1))
    assertEquals((Cst(6)*a + b) % a, b%a)
    assertEquals(((Cst(6)*a + b) % a) % a, b%a) // Nested
    val c = Var("c")
    assertEquals((Cst(4)*a*c + Cst(6)*b*c) % (Cst(2)*c),Cst(0))
  }

  @Test
  def sumDivisor(): Unit = {
    val a = Cst(2)
    val b = Var("b")
    val c = Var("c")
    val d = Var("d")
    val e = Var("e")
    // Trivial cases
    assertEquals((a*b) % a, Cst(0))
    assertEquals((a+b) % a, b % a)
    // Direct factorisation
    assertEquals((a*c+b*c) % (a+b), Cst(0))
    assertEquals((a*c+b*c+a*d+b*d) % (a+b), Cst(0))
    // Find a good partition of dividend
    assertEquals((a+b+c) % (a+b), c % (a+b))
    assertEquals((a*c+b*c+d) % (a+b), d % (a+b))
    assertEquals((a*c+b*c+d+e) % (a+b), (d+e) % (a+b))
    assertEquals((a*c+b*c+a*d+b*d+e) % (a+b), e % (a+b))
  }

  @Test
  def sumCoprimeDivMod() : Unit = {
    val a = Var("a")
    val b = Var("b")
    val n = Cst(8)
    val c = Cst(3) // Note c and n are coprime
    assertEquals(a, a / n * n + a % n)
    assertEquals(a*c, a*c / n * n + a*c % n)
    assertEquals((a+b)*c, (a+b)*c / n * n + (a+b)*c % n)
  }

  @Test
  def sumNonCoprimeDivMod(): Unit = {
    val a = Var("a")
    val b = Var("b")
    val n = Cst(8)
    val c = Cst(4) // Note c and n aren't coprime
    assertEquals(a*c, a*c / n * n + a*c % n)
    assertEquals((a+b)*c, (a+b)*c / n * n + (a+b)*c % n)
  }

  @Test
  def divPlusModOfSumMultipliedConstants(): Unit = {
    val a = Var("a")
    val b = Var("b")
    val x = Cst(8)
    val expr_1 = x * Cst(4) * (a+b)
    val expr_2 = x * Cst(4) * (a + b) / Cst(16) * Cst(16) + x * Cst(4) * (a + b) % Cst(16)
    assertEquals(expr_1, expr_2.toProd.get)
  }

  @Test
  def sumDivModSimplification(): Unit = {
    val a = Var("a")
    val b = Var("b")
    assertNotEquals(a % Cst(8), (((b * Cst(4)) + a) % Cst(8)) * Cst(1))
    assertEquals(b * Cst(4) + a, Cst(0) + ((((b * Cst(4)) + a) / Cst(8)) * Cst(8) * Cst(1)) +
      (((b * Cst(4)) + a) % Cst(8)) * Cst(1))
  }

  @Test
  def intDivFloorTest(): Unit = {
    val expr = Cst(4) * Var("a", Interval(Cst(0), Cst(31)))
    val startDiv = (Cst(899) + expr) / Cst(128)
    val startMod = (Cst(899) + expr) % Cst(128)
    assertEquals(startDiv, Cst(7))
    assertEquals(startMod, Cst(3) + expr)
  }

  @Test
  def sumWithMod(): Unit = {
    val a = Var("a")
    val b = Var("b")
    val c = Var("c")
    val expr_1 = b+c
    val expr_2 = b*c
    val n = Var("n")
    assertEquals(a % n, (a % n + expr_1 * n) % n)
    assertEquals(a % n, (a % n + expr_1 * n + expr_2 * n) % n)
  }

  @Test
  def partitionConstantMultiple(): Unit = {
    val a = Var("a", Interval(Cst(2), Cst(3)))
    val b = Var("b", Interval(Cst(6), Cst(8)))
    val c = Cst(2)
    val d = Var("d")
    // Using int div
    assertEquals(a+c, (Cst(2)*a+b+c) % (a+b))
    assertEquals(a+b,(Cst(3)*a + Cst(5)*b) % (a + Cst(2)*b))
    // Partition dividend
    assertEquals(a*c,(Cst(2)*a*c+b*c+a*d+b*d) % (a+b))
    val e = Var("e")
    assertEquals(a*e % (a+b),(Cst(2)*a*e+b*e+a*d+b*d) % (a+b))
  }

  @Test
  def partitionConstant(): Unit = {
    val a = Var("a")
    val b = Var("b")
    // Partition as (2+a) % (2+a) + (2+b) % (2+a) = (2+b) % (2+a)
    assertEquals((Cst(2)+b) % (Cst(2)+a),(Cst(4)+a+b) % (Cst(2)+a))
    // Partition as (4+2a) % (2+a) + (1+b) % (2+a) = (1+b) % (2+a)
    assertEquals((Cst(1)+b) % (Cst(2)+a),(Cst(5)+Cst(2)*a+b) % (Cst(2)+a))
    // Partition as (ab+a+2b+2) % (2+a) + (1+b) % (2+a)
    assertEquals((Cst(1)+b) % (Cst(2)+a),(Cst(3)+a+Cst(3)*b+a*b) % (Cst(2)+a))
    val c = Var("c")
    // Partition as (2+a+2c+ac) % (2+a) + (1+b) % (2+a)
    assertEquals((Cst(1)+b) % (Cst(2)+a),(Cst(3)+a+b+Cst(2)*c+c*a) % (Cst(2)+a))
  }
}
