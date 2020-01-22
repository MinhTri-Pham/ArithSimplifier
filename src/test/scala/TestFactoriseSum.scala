import arithmetic._
import org.junit.Test
import org.junit.Assert._

class TestFactoriseSum {
  val a: Var = Var("a")
  val b: Var = Var("b")
  val c: Var = Var("c")
  val d: Var = Var("d")
  val e: Var = Var("e")
  val f: Var = Var("f")

  // Factorisation with a common term
  @Test
  def commonTerm(): Unit = {
    val s1 = a*b+a
    assertEquals(Factorise(s1),Some(a*(b+Cst(1))))

    val s2 = (a pow 2) + a*b
    assertEquals(Factorise(s2),Some(a*(a+b)))

    val s3 = a*a + a*b + a*c
    assertEquals(Factorise(s3),Some(a*(a+b+c)))

    val s4 = a*b*c + a*b*d
    assertEquals(Factorise(s4),Some(a*b*(c+d)))

    val s5 = a*b*c*d + a*b*e
    assertEquals(Factorise(s5),Some(a*b*(c*d+e)))

    val s6 = (a pow 2)*b + (a pow 2)*c
    assertEquals(Factorise(s6),Some((a pow 2)*(b+c)))

    val s7 = (a pow 3)*b*c+(a pow 2)*c*d
    assertEquals(Factorise(s7),Some((a pow 2)*c*(a*b+d)))

    val s9 = Cst(4)*a + Cst(6)*b
    assertEquals(Factorise(s9), Some(Cst(2)*(Cst(2)*a+Cst(3)*b)))

    val s10 = Cst(4)*a*c + Cst(8)*b*c
    assertEquals(Factorise(s10), Some(Cst(4)*c*(a+Cst(2)*b)))

    val s11 = (a pow -3)*b*c+(a pow -2)*c*d
    assertEquals(Factorise(s11),Some((a pow -2)*c*((a pow -1)*b+d)))
  }

  // Factorisation without common term
  @Test
  def nonCommonTerm() : Unit = {
    val s1 = a*c + a*d + b*c + b*d
    assertEquals(Factorise(s1), Some((a+b)*(c+d)))

    val s2 = a*d + a*e + b*d + b*e + c*d + c*e
    assertEquals(Factorise(s2), Some((a+b+c)*(d+e)))

    val s3 = a*d + a*e + b*d + b*e + c*d + c*e + f*a + f*b + f*c
    assertEquals(Factorise(s3), Some((a+b+c)*(d+e+f)))

    val s4 = a*c*e + a*d*e + b*c*e + b*d*e
    assertEquals(Factorise(s4), Some((a+b)*(c+d)*e))

    val s5 = a*c*e + a*c*f + b*c*e + b*c*f + a*d*e + a*d*f + b*d*e + b*d*f
    assertEquals(Factorise(s5), Some((a+b)*(c+d)*(e+f)))

    val s6 = a*b*d+a*b*e+c*d+c*e
    assertEquals(Factorise(s6),Some((a*b+c)*(d+e)))
  }

  // Factorisation not possible
  @Test
  def nonFactorisable() : Unit = {
    val s1 = a*b + a*c + b*c
    val s2 = a*b + a*c + a*d + e*f
    val s3 = a*b*c + b*c*d + c*d*e + d*e*f
    assertEquals(Factorise(s1),None)
    assertEquals(Factorise(s2),None)
    assertEquals(Factorise(s3),None)
  }

  @Test
  def prods(): Unit = {
    val s1 = (a pow 2) + a*b + a*c + b*c
    assertEquals(Factorise(s1),Some((a+b)*(a+c)))

    val s2 = (a pow 2) + a*d + b*a + b*d + c*a + c*d
    assertEquals(Factorise(s2), Some((a+b+c)*(a+d)))

    val s3 = (a pow 2)*d + (a pow 2)*e + a*b*d + a*b*e + a*c*d + a*c*e + b*c*d + b*c*e
    assertEquals(Factorise(s3),Some((a+b)*(a+c)*(d+e)))

    val s4 = (a pow 3) + (a pow 2)*b + (a pow 2)*c + (a pow 2)*d +a*b*c + a*b*d + a*c*d + b*c*d
    assertEquals(Factorise(s4),Some((a+b)*(a+c)*(a+d)))

    val s5 = (a pow 2)*b + a*b*d + a*c + c*d
    assertEquals(Factorise(s5),Some((a*b+c)*(a+d)))

    val s6 = (a*b pow 2) + a*b*c+ a*b*d + c*d
    assertEquals(Factorise(s6),Some((a*b+c)*(a*b+d)))

    val s7 = a*(b pow 2)*c + a*b*d+ b*(c pow 2) + c*d
    assertEquals(Factorise(s7),Some((a*b+c)*(b*c+d)))
  }

  @Test
  def pows() : Unit = {
    val s1 = (a pow 2) + Cst(2)*a*b + (b pow 2)
    assertEquals(Factorise(s1), Some((a+b) pow 2))

    val s2 = (a pow 3) + Cst(3)*(a pow 2)*b + Cst(3)*a*(b pow 2) + (b pow 3)
    assertEquals(Factorise(s2), Some((a+b) pow 3))

    val s3 = (a pow 2) + Cst(2)*a*b + (b pow 2) + a*c + b*c
    assertEquals(Factorise(s3), Some((a+b+c)*(a+b)))

    val s4 = (a pow 2) + Cst(2)*a*b + Cst(2)*a*c + (b pow 2) + Cst(2)*b*c + (c pow 2)
    assertEquals(Factorise(s4), Some((a+b+c) pow 2))

    // Takes long
//    val s5 = (a pow 3) + Cst(3)*(a pow 2)*b + Cst(3)*(a pow 2)*c + Cst(3)*a*(b pow 2) + Cst(6)*a*b*c +
//      Cst(3)*a*(c pow 2) + (b pow 3) + Cst(3)*(b pow 2)*c + Cst(3)*b*(c pow 2) + (c pow 3)
//    assertEquals(Factorise(s5), Some((a+b+c) pow 3))

    val s6 = (a*b pow 2) + Cst(2)*a*b*c + (c pow 2)
    assertEquals(Factorise(s6), Some((a*b+c) pow 2))
  }

  @Test
  def withNegativePowers() : Unit = {

    val s1 = (a*c pow -1) + d*(a pow -1)+b*(c pow -1) + b*d
    assertEquals(Factorise(s1), Some(((a pow -1) + b)*((c pow -1) + d)))

    val s2 = (a pow -2) + Cst(2)*b/^a + (b pow 2)
    assertEquals(Factorise(s2), Some((Cst(1)/^a + b) pow 2))

    val s3 = (a pow -3) + Cst(3)*b*(a pow -2) + Cst(3)*(b pow 2)*(a pow -1) + (b pow 3)
    assertEquals(Factorise(s3), Some(((a pow -1) + b) pow 3))

    val s4 = (a*c pow -1) + (a*d pow -1) + (b*c pow -1) + (b*d pow -1)
    assertEquals(Factorise(s4), Some(((a pow -1) + (b pow -1))*((c pow -1) + (d pow -1))))

    val s6 = (a pow -2) + Cst(2)*b*c/^a + (b*c pow 2)
    assertEquals(Factorise(s6), Some((Cst(1)/^a + b*c) pow 2))

    val s7 = (a pow -3) + Cst(3)*b*c/^(a pow 2) + Cst(3)*(b*c pow 2)/^a + (b*c pow 3)
    assertEquals(Factorise(s7), Some(((a pow -1) + b*c) pow 3))

    val s8 = (a*b pow -2) + Cst(2)*c * (a*b pow -1) + (c pow 2)
    assertEquals(Factorise(s8), Some(((a pow -1)*(b pow -1) + c) pow 2))

    val s9 = (a pow -4) + Cst(2)*b*c/^(a pow 2) + b*b*c*c
    assertEquals(Factorise(s9), Some(((a pow -2) + b*c) pow 2))
  }
}
