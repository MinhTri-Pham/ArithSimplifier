import arithmetic._
import org.junit.Test
import org.junit.Assert._

class TestLog {
  @Test
  def tests(): Unit = {
    assertEquals(Cst(0), log(2,Cst(1)))
    assertEquals(Cst(1), log(2,Cst(2)))
    assertEquals(Cst(2), log(2,Cst(4)))
    assertEquals(Cst(-2), log(2,Cst(1) /^ Cst(4)))
    assertEquals(Cst(1) /^ Cst(2), log(4,Cst(2)))
    assertEquals(Cst(2), 8*log(16,2))

    val a = Var("a")
    assertEquals(7 + log(2,a), log(2,8*a) + 4)
    assertEquals(Cst(3) + 8*log(16,a), 8*log(16,2*a) + 1)
  }
}
