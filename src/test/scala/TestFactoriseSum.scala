import org.junit.Test
import org.junit.Assert._

class TestFactoriseSum {
  val a = Var("a")
  val b = Var("b")
  val c = Var("c")
  val d = Var("d")
  val e = Var("e")
  val f = Var("f")

  // Factorisation with a common term
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

    val s9 = Sum(List((a pow 2)*b,(a pow 2)*c))
    assertEquals(FactoriseSum(s9),Some((a pow 2)*(b+c)))
  }

  // Factorisation without common term
  @Test
  def nonCommonTermTests() : Unit = {
    val s1 = Sum(List(a*c,a*d,b*c,b*d))
    assertEquals(FactoriseSum(s1), Some((a+b)*(c+d)))

    val s2 = Sum(List(a*d,a*e,b*d,b*e,c*d,c*e))
    assertEquals(FactoriseSum(s2), Some((a+b+c)*(d+e)))

    val s3 = Sum(List(a*d,a*e,b*d,b*e,c*d,c*e,f*a,f*b,f*c))
    assertEquals(FactoriseSum(s3), Some((a+b+c)*(d+e+f)))

    val s4 = Sum(List(a*c*e,a*d*e,b*c*e,b*d*e))
    assertEquals(FactoriseSum(s4), Some((a+b)*(c+d)*e))

    val s5 = Sum(List(a*c*e,a*c*f,b*c*e,b*c*f,a*d*e,a*d*f,b*d*e,b*d*f))
    assertEquals(FactoriseSum(s5), Some((a+b)*(c+d)*(e+f)))
  }

  // Factorisation not possible
  @Test
  def nonFactorisableTests() : Unit = {
    val s1 = Sum(List(a,b))
    val s2 = Sum(List(a*b,c*d))
    val s3 = Sum(List(a*b,a*c,b*c))
    val s4 = Sum(List(a*b,a*c,a*d,e*f))
    val s5 = Sum(List(a*b*c,b*c*d,c*d*e,d*e*f))
    assertEquals(FactoriseSum(s1),None)
    assertEquals(FactoriseSum(s2),None)
    assertEquals(FactoriseSum(s3),None)
    assertEquals(FactoriseSum(s4),None)
    assertEquals(FactoriseSum(s5),None)
  }

  // Differs by first two test suites in the fact that we need to explore more terms to produce a factorisation
  @Test
  def nonInstantTests(): Unit = {
    val s1 = Sum(List(a pow 2,a*b,a*c,b*c))
    assertEquals(FactoriseSum(s1),Some((a+b)*(a+c)))

    val  s2 = Sum(List(a pow 2,a*d,b*a,b*d,c*a,c*d))
    assertEquals(FactoriseSum(s2), Some((a+b+c)*(a+d)))

    val s3 = Sum(List((a pow 2)*e,a*b*e,a*c*e,b*c*e))
    assertEquals(FactoriseSum(s3),Some((a+b)*(a+c)*e))
  }

}
