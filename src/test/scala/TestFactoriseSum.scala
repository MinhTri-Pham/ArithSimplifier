import org.junit.Test
import org.junit.Assert._

class TestFactoriseSum {
  val a = Var("a")
  val b = Var("b")
  val c = Var("c")
  val d = Var("d")
  val e = Var("e")
  val f = Var("f")

  @Test
  def commonTermTests(): Unit = {
    val s1 = Sum(List(a*b,a))
    assertEquals(FactoriseSum(s1),Some(a*(Cst(1)+b)))

    val s2 = Sum(List(a*b,a*c))
    assertEquals(FactoriseSum(s2),Some(a*(b+c)))

    val s3 = Sum(List(a*a,a*b))
    assertEquals(FactoriseSum(s3),Some(a*(a+b)))

    val s4 = Sum(List(a*a,a*b,a*c))
    assertEquals(FactoriseSum(s4),Some(a*(a+b+c)))

    val s5 = Sum(List(a*b*c,a*b*d))
    assertEquals(FactoriseSum(s5),Some(a*b*(c+d)))

    val s6 = Sum(List(a*b*c*d,a*b*c*e))
    assertEquals(FactoriseSum(s6),Some(a*b*c*(d+e)))

    val s7 = Sum(List(a*b*c*d,a*b*e))
    assertEquals(FactoriseSum(s7),Some(a*b*(c*d+e)))

    val s8 = Sum(List(a*(c+d),b*(c+d)))
    assertEquals(FactoriseSum(s8),Some((a+b)*(c+d)))
  }

  @Test
  def nonCommonTermTests() : Unit = {
    val s1 = Sum(List(a*c,a*d,b*c,b*d))
    assertEquals(FactoriseSum(s1), Some((a+b)*(c+d)))

    val s2 = Sum(List(a*d,a*e,b*d,b*e,c*d,c*e))
    assertEquals(FactoriseSum(s2), Some((a+b+c)*(d+e)))

    val s3 = Sum(List(a*d,a*e,b*d,b*e,c*d,c*e,f*a,f*b,f*c))
    assertEquals(FactoriseSum(s3), Some((a+b+c)*(d+e+f)))

    // To do: Make this pass
//    val s4 = Sum(List(a*b*e,a*c*e,b*c*e,b*d*e))
//    assertEquals(FactoriseSum(s4), Some((a+b)*(c+d)*e))

  }

}
