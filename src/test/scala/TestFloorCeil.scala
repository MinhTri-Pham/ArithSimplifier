import arithmetic._
import org.junit.Test
import org.junit.Assert._

class TestFloorCeil {
  val x: Var = Var("x", Interval(Cst(5), Cst(7)))
  val y: Var = Var("y", Interval(Cst(8), Cst(9)))

  @Test
  def constTest(): Unit = {
    val a = Cst(2)
    assertEquals(floor(a),a)
    assertEquals(ceil(a),a)
    assertEquals(floor(a+x),a+floor(x))
  }

  @Test
  def tests(): Unit = {
    val w = x /^ y
    val z = y /^ x
    assertEquals(floor(w),Cst(0))
    assertEquals(floor(z),Cst(1))
    val n = Cst(1)
    assertEquals(floor(z+n),floor(z)+n)
    assertEquals(ceil(w),Cst(1))
    assertEquals(ceil(z),Cst(2))
    assertEquals(ceil(z+n),ceil(z)+n)
    assertEquals(ceil(z)-floor(z),Cst(1))
  }

  @Test
  def nested(): Unit = {
    // Should be the innermost function
    assertEquals(ceil(ceil(floor(ceil(floor(x))))),floor(x))
    val z = y /^ x
    assertEquals(ceil(ceil(floor(ceil(floor(z))))),Cst(1))
  }
}
