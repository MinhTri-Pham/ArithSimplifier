import arithmetic._
import org.junit.Test
import org.junit.Assert._

class TestDifferentiation {

  val x: Var = Var("x")
  val y: Var = Var("y")
  val z: Var = Var("z")

  @Test
  def orderingMixedTest(): Unit = {
    val expr = (y+z)*(x+Cst(2)*y)*(x pow 2)
    val xyz = Differentiate(Differentiate(Differentiate(expr,x),y),z)
    val zxy = Differentiate(Differentiate(Differentiate(expr,z),x),y)
    val yzx = Differentiate(Differentiate(Differentiate(expr,y),z),x)
    assertEquals(xyz, zxy)
    assertEquals(zxy, yzx)
  }

  @Test
  def orderingMixedRepeatedTest(): Unit = {
    val expr = (y+z)*(x+Cst(2)*y)*(x pow 2)
    val xyx = Differentiate(Differentiate(Differentiate(expr,x),y),x)
    val xxy = Differentiate(Differentiate(expr,x,2),y)
    val yxx = Differentiate(Differentiate(expr,y),x,2)
    assertEquals(xyx, xxy)
    assertEquals(xxy, yxx)
  }

  @Test
  def powerTest(): Unit = {
    val expr = (Cst(3)*x-y) pow 2
    val xy = Differentiate(Differentiate(expr,x),y)
    val yx = Differentiate(Differentiate(expr,y),x)
    assertEquals(xy,yx)
  }

  @Test
  def absTest(): Unit = {
    val expr = abs(x) pow 3
    assertEquals(Cst(3)*x*abs(x),Differentiate(expr,x))
  }
}
