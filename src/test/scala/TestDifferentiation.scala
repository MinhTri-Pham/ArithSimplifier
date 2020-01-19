import arithmetic._
import org.junit.Test
import org.junit.Assert._

class TestDifferentiation {

  @Test
  def orderingMixedTest(): Unit = {
    val x = Var("x")
    val y = Var("y")
    val z = Var("z")
    val expr = (y+z)*(x+Cst(2)*y)*(x pow 2)
    val xyz = Differentiate(Differentiate(Differentiate(expr,x),y),z)
    val zxy = Differentiate(Differentiate(Differentiate(expr,z),x),y)
    val yzx = Differentiate(Differentiate(Differentiate(expr,y),z),x)
    assertEquals(xyz, zxy)
    assertEquals(zxy, yzx)
  }

  @Test
  def orderingMixedRepeatedTest(): Unit = {
    val x = Var("x")
    val y = Var("y")
    val z = Var("z")
    val expr = (y+z)*(x+Cst(2)*y)*(x pow 2)
    val xyx = Differentiate(Differentiate(Differentiate(expr,x),y),x)
    val xxy = Differentiate(Differentiate(expr,x,2),y)
    val yxx = Differentiate(Differentiate(expr,y),x,2)
    assertEquals(xyx, xxy)
    assertEquals(xxy, yxx)
  }
}
