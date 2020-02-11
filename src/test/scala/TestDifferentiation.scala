import arithmetic._
import org.junit.Test
import org.junit.Assert._

class TestDifferentiation {

  val x: Var = Var("x")
  val y: Var = Var("y")
  val z: Var = Var("z")

  @Test
  def polyTest(): Unit = {
    assertEquals(Cst(0), Differentiate(Cst(2),x)) // Constant
    assertEquals(Cst(1), Differentiate(x + 5,x)) // Linear
    assertEquals(2*x+1, Differentiate((x pow 2) + x + 5,x)) // Quadratic
    assertEquals(1 - (x pow -2) , Differentiate(x + (x pow -1) + 5,x)) // Negative power
  }

  // Mixing ordering of variable for mixed partial derivatives shouldn't change anything
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
    val expr = (Cst(3)*x-y) pow 3
    val xy = Differentiate(Differentiate(expr,x),y)
    val yx = Differentiate(Differentiate(expr,y),x)
    assertEquals(xy,yx)
  }

  @Test
  def powerProdTest(): Unit = {
    val expr = ((x+y)*x) pow 2
    val xy = Differentiate(Differentiate(expr,x),y)
    val yx = Differentiate(Differentiate(expr,y),x).toSum.get
    assertEquals(xy,yx)
  }

  @Test
  def absTest(): Unit = {
    val expr = abs(x) pow 3
    assertEquals(Cst(3)*x*abs(x),Differentiate(expr,x))
  }
}
