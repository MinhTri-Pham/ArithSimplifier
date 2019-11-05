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
  def commonTerm(): Unit = {
    val s1 = a*b+a
    assertEquals(Factorise(s1),Some(a*(b+Cst(1))))

    val s2 = a*b+a*c
    assertEquals(Factorise(s2),Some(a*(b+c)))

    val s3 = (a pow 2) + a*b
    assertEquals(Factorise(s3),Some(a*(a+b)))

    val s4 = a*a + a*b + a*c
    assertEquals(Factorise(s4),Some(a*(a+b+c)))

    val s5 = a*b*c + a*b*d
    assertEquals(Factorise(s5),Some(a*b*(c+d)))

    val s6 = a*b*c*d + a*b*c*e
    assertEquals(Factorise(s6),Some(a*b*c*(d+e)))

    val s7 = a*b*c*d + a*b*e
    assertEquals(Factorise(s7),Some(a*b*(c*d+e)))

    val s8 = (a pow 2)*b + (a pow 2)*c
    assertEquals(Factorise(s8),Some((a pow 2)*(b+c)))

    val s9 = (a pow 3)*b*c+(a pow 2)*c*d
    assertEquals(Factorise(s9),Some((a pow 2)*c*(a*b+d)))
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
  }

  // Factorisation not possible
  @Test
  def nonFactorisable() : Unit = {
    val s1 = a+b
    val s2 = a*b + c*d
    val s3 = a*b + a*c + b*c
    val s4 = a*b + a*c + a*d + e*f
    val s5 = a*b*c + b*c*d + c*d*e + d*e*f
    assertEquals(Factorise(s1),None)
    assertEquals(Factorise(s2),None)
    assertEquals(Factorise(s3),None)
    assertEquals(Factorise(s4),None)
    assertEquals(Factorise(s5),None)
  }

  @Test
  def withPowers(): Unit = {
    val s1 = (a pow 2) + a*b + a*c + b*c
    assertEquals(Factorise(s1),Some((a+b)*(a+c)))

    val s2 = (a pow 2) + a*d + b*a + b*d + c*a + c*d
    assertEquals(Factorise(s2), Some((a+b+c)*(a+d)))

    val s3 = (a pow 2)*e + a*b*e + a*c*e + b*c*e
    assertEquals(Factorise(s3),Some((a+b)*(a+c)*e))

    val s4 = (a pow 2)*d + (a pow 2)*e + a*b*d + a*b*e + a*c*d + a*c*e + b*c*d + b*c*e
    assertEquals(Factorise(s4),Some((a+b)*(a+c)*(d+e)))

    val s5 = (a pow 3) + (a pow 2)*b + (a pow 2)*c + (a pow 2)*d +a*b*c + a*b*d + a*c*d + b*c*d
    assertEquals(Factorise(s5),Some((a+b)*(a+c)*(a+d)))

    val s6 = (a pow 2) + Cst(2)*a*b + (b pow 2)
    assertEquals(Factorise(s6).get.toPow, Some((a+b) pow 2))

    val s7 = (a pow 3) + Cst(3)*(a pow 2)*b + Cst(3)*a*(b pow 2) + (b pow 3)
    //println(Factorise(s7))
    assertEquals(Factorise(s7).get.toPow, Some((a+b) pow 3))

    val s8 = (a pow 2) + Cst(2)*a*b + (b pow 2) + a*c + b*c
    assertEquals(Factorise(s8), Some((a+b+c)*(a+b)))

    val s9 = (a pow 2) + Cst(2)*a*b + Cst(2)*a*c + (b pow 2) + Cst(2)*b*c + (c pow 2)
    assertEquals(Factorise(s9).get.toPow, Some((a+b+c) pow 2))
  }

  // With constants
//  @Test
//  def withConstants(): Unit = {
//    val s1 = Cst(2)*a + Cst(8)
//    assertEquals(Factorise(s1),Some(Cst(2)*(a+Cst(4))))
//
//    val s2 = a*b + Cst(2)*a + Cst(2)*b + Cst(4)
//    assertEquals(Factorise(s2),Some((a+Cst(2))*(b+Cst(2))))
//
//    val s3 = Cst(2)*a*c + Cst(6)*a + b*c + Cst(3)*b
//    assertEquals(Factorise(s3),Some((Cst(2)*a+b)*(c+Cst(3))))

//    val s5 = Cst(2)*a*d + Cst(4)*a*e + b*d + Cst(2)*b*e + Cst(3)*c*d + Cst(6)*c*e
//    assertEquals(Factorise(s5),Some((Cst(2)*a+b+Cst(3)*c)*(d+Cst(2)*e)))

//    val s6 = Cst(6)*a*a + Cst(2)*a*c + Cst(3)*b*a + b*c
//    assertEquals(Factorise(s6),Some((Cst(2)*a+b)*(Cst(3)*a+c)))

//    val s7 = Cst(8)*(a pow 2)*c + Cst(8)*(a pow 2)*e + Cst(48)*(a pow 2) + Cst(4)*a*b*c + Cst(4)*a*b*e + Cst(24)*a*b +
//      Cst(32)*a*(c pow 2) + Cst(32)*a*c*e + Cst(192)*a*c + Cst(16)*b*(c pow 2) + Cst(16)*b*c*e + Cst(96)*b*c
//    assertEquals(Factorise(s7), Some(Cst(4)*(Cst(2)*a+b)*(a+Cst(4)*c)*(c+e+Cst(6))))
 // }
}
