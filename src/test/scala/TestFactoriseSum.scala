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
    assertEquals(Factorise(s1),Some(a*(b + 1)))

    val s2 = (a pow -3)*b*c+(a pow -2)*c*d
    assertEquals(Factorise(s2),Some((a pow -2)*c*((a pow -1)*b+d)))

    val s3 = Cst(6)*(a pow -3)*b*c*(e pow 6) + Cst(9)*(a pow -2)*c*d*(e pow 4)
    assertEquals(Factorise(s3),Some(Cst(3)*(a pow -2)*(e pow 4)*c*(Cst(2)*(a pow -1)*b*(e pow 2) + 3*d)))
  }

  // Factorisation without common term
  @Test
  def nonCommonTerm() : Unit = {
    val s1 = a*c + a*d + b*c + b*d
    assertEquals(Factorise(s1), Some((a+b)*(c+d)))

    val s2 = a*d + a*e + b*d + b*e + c*d + c*e + f*a + f*b + f*c
    assertEquals(Factorise(s2), Some((a+b+c)*(d+e+f)))

    val s3 = a*c*e + a*c*f + b*c*e + b*c*f + a*d*e + a*d*f + b*d*e + b*d*f
    assertEquals(Factorise(s3), Some((a+b)*(c+d)*(e+f)))

    val s4 = a*b*d+a*b*e+c*d+c*e
    assertEquals(Factorise(s4),Some((a*b+c)*(d+e)))
  }

  @Test
  def prods(): Unit = {
    val s1 = (a pow 2) + a*b + a*c + b*c
    assertEquals(Factorise(s1),Some((a+b)*(a+c)))

    val s2 = (a pow 2) + a*d + b*a + b*d + c*a + c*d
    assertEquals(Factorise(s2), Some((a+b+c)*(a+d)))

    val s3 = (a pow 2)*d + (a pow 2)*e + a*b*d + a*b*e + a*c*d + a*c*e + b*c*d + b*c*e
    assertEquals(Factorise(s3),Some((a+b)*(a+c)*(d+e)))

    val s4 = (a pow 3) + (a pow 2)*b + (a pow 2)*c + (a pow 2)*d + a*b*c + a*b*d + a*c*d + b*c*d
    assertEquals(Factorise(s4),Some((a+b)*(a+c)*(a+d)))

    val s5 = (a*b pow 2) + a*b*c+ a*b*d + c*d
    assertEquals(Factorise(s5),Some((a*b+c)*(a*b+d)))

    val s6 = a*(b pow 2)*c + a*b*d+ b*(c pow 2) + c*d
    assertEquals(Factorise(s6),Some((a*b+c)*(b*c+d)))

    val s7 = (a pow 3) + (a pow 2)*b + (a pow 2)*c + a*(b pow 2)+ a*(c pow 2) + (b pow 3) +
      (b pow 2)*c + b*(c pow 2) + (c pow 3)
    assertEquals(Factorise(s7),Some(((a pow 2)+(b pow 2)+(c pow 2))*(a+b+c)))

    val s8 = (a pow 3)*b*e + (a pow 3)*b*f + (a pow 2)*c*d*e + (a pow 2)*c*d*f + a*(b pow 3)*e + a*(b pow 3)*f +
      a*b*(c pow 2)*e + a*b*(c pow 2)*f + (b pow 2)*c*d*e + (b pow 2)*c*d*f + (c pow 3)*d*e + (c pow 3)*d*f
    assertEquals(Factorise(s8),Some(((a pow 2)+(b pow 2)+(c pow 2))*(a*b+c*d)*(e+f)))
  }

  @Test
  def pows() : Unit = {
    val s1 = (a pow 2) + 2*a*b + (b pow 2)
    assertEquals(Factorise(s1), Some((a+b) pow 2))

    val s2 = (a pow 3) + 3*(a pow 2)*b + 3*a*(b pow 2) + (b pow 3)
    assertEquals(Factorise(s2), Some((a+b) pow 3))

    val s3 = (a pow 2) + 2*a*b + Cst(2)*a*c + (b pow 2) + Cst(2)*b*c + (c pow 2)
    assertEquals(Factorise(s3), Some((a+b+c) pow 2))

    val s4 = (a pow 3)*(b pow 3) + 3*(a pow 2)* (b pow 2)*c + 3*a*b*(c pow 2) + (c pow 3)
    assertEquals(Factorise(s4), Some((a*b+c) pow 3))
  }

  @Test
  def withNegativePowers() : Unit = {
    val s1 = (a*c pow -1) + d*(a pow -1)+b*(c pow -1) + b*d
    assertEquals(Factorise(s1), Some((Cst(1)/^a + b)*(Cst(1)/^c + d)))

    val s2 = (a pow -3) + Cst(3)*b*(a pow -2) + Cst(3)*(b pow 2)*(a pow -1) + (b pow 3)
    assertEquals(Factorise(s2), Some((Cst(1)/^a + b) pow 3))

    val s3 = (a*c pow -1) + (a*d pow -1) + (b*c pow -1) + (b*d pow -1)
    assertEquals(Factorise(s3), Some((Cst(1)/^a + Cst(1)/^b)*(Cst(1)/^c + Cst(1)/^d)))

    val s4 = (a*d pow -1) + (e*f)/^a + (b*c)/^d + b*c*e*f
    assertEquals(Factorise(s4), Some((Cst(1)/^a + b*c)*(Cst(1)/^d + e*f)))

    val s5 = c*(a pow -1) + d*(a pow -1)+c*(b pow -1) + d*(b pow -1)
    assertEquals(Factorise(s5), Some((Cst(1)/^a + Cst(1)/^b)*(c + d)))

    val s6 = (a pow -3) + Cst(3)*b*c/^(a pow 2) + Cst(3)*(b*c pow 2)/^a + (b*c pow 3)
    assertEquals(Factorise(s6), Some((Cst(1)/^a + b*c) pow 3))

    val s7 = a*b + a*(b pow -1) + (a pow -1)*b + (a*b pow -1)
    assertEquals(Factorise(s7), Some((Cst(1)/^a + a)*(Cst(1)/^b + b)))
  }

  @Test
  def withConsts() : Unit = {
    val s1 = 8*a*b + 12*a + 6*b + 9
    assertEquals(Factorise(s1), Some((4*a+3)*(2*b+3)))

//    val s2 = 6*a*b + 4*a*c + 9*(b pow 2) + 6*b*c
//    assertEquals(Factorise(s2), Some((2*a+3*b)*(3*b+2*c)))

    val s3 = 12*a*c + 9*b*c + 8*a*d + 6*b*d
    assertEquals(Factorise(s3), Some((4*a+3*b)*(3*c+2*d)))

    val s4 = 4*a*b + 4*a*c + 4*a + 3*(b pow 2) + 3*b*c + 5*b + 2*c + 2
    assertEquals(Factorise(s4), Some((4*a+3*b+2)*(1+b+c)))

    // Takes a while because of constant multiples
//    val s5 = 28*(a pow 2) + 21*a*b + 35*a*b + 24*a*d + 18*b*d + 30*c*d
//    assertEquals(Factorise(s5), Some((4*a+3*b+5*c)*(7*a+6*d)))

    val s6 = 10*b /^ a + Cst(4) /^ a  + 15*b + 6
    assertEquals(Factorise(s6), Some((Cst(2)/^a+3)*(5*b+2)))

    val s7 = 8*a*b + 2*a*c + 6*a + 4*(b pow 2) + b*c + 3*b
    assertEquals(Factorise(s7), Some((2*a+b)*(4*b+c+3)))
  }
}
