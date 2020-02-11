import arithmetic._
import org.junit.Test
import org.junit.Assert._

class TestAssocAndComm {

  val a: Var = Var("a")
  val b: Var = Var("b")
  val c: Var = Var("c")
  val d: Var = Var("d")

  @Test
  def testAddition(): Unit = {
    assertEquals(a + b, b + a)
    assertEquals(a + b + c, c + b + a)

    assertEquals(a + (b + c), (a + b) + c)
  }

  @Test
  def testMultiplication(): Unit = {
    assertEquals(a * b, b * a)
    assertEquals(a * b * c, c * b * a)

    assertEquals(a * (b * c), (a * b) * c)
  }

  @Test
  def testDiv(): Unit = {
    assertNotEquals(a /^ b, b /^ a)
    assertNotEquals(a /^ b /^ c, c /^ b /^ a)

    assertNotEquals(a /^ (b /^ c), (a /^ b) /^ c)
  }

  @Test
  def testIntDiv(): Unit = {
    assertNotEquals(a / b, b / a)
    assertNotEquals(a / b / c, c / b / a)

    assertNotEquals(a / (b / c), (a / b) / c)
  }

  @Test
  def testMod(): Unit = {
    assertNotEquals(a % b, b % a)
    assertNotEquals(a % b % c, c % b % a)

    assertNotEquals(a % (b % c), (a % b) % c)
  }
}
