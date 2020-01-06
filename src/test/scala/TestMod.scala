import org.junit.Test
import org.junit.Assert._

class TestMod {

  @Test
  def basicTests(): Unit = {
    assertEquals(Cst(6) % Cst(3), Cst(0))
    assertEquals(Cst(7) % Cst(3), Cst(1))
    val a = Var("a")
    val b = Var("b")
    assertEquals((Cst(6)*a + Cst(4)) % Cst(3), Cst(1))
    assertEquals((Cst(6)*a + b) % a, b%a)
    assertEquals(((Cst(6)*a + b) % a) % a, b%a) // Nested
    val c = Var("c")
    assertEquals((Cst(4)*a*c + Cst(6)*b*c) % (Cst(2)*c),Cst(0))
  }

  @Test
  def sumTests(): Unit = {
    val a = Var("a")
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
    // Find a good subset
    assertEquals((a+b+c) % (a+b), c % (a+b))
    assertEquals((a*c+b*c+d) % (a+b), d % (a+b))
    assertEquals((a*c+b*c+d+e) % (a+b), (d+e) % (a+b))
    assertEquals((a*c+b*c+a*d+b*d+e) % (a+b), e % (a+b))
  }

}
