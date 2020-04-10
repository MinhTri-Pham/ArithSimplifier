import arithmetic._
import org.junit.Test
import org.junit.Assert._

class TestSignRanges {

  @Test
  def signArithmetic(): Unit = {
    val a = Var("a") // Positive
    val b = NegVar("b") // Negative
    val e1 = a*b
    val e2 = a+b
    val e3 = b pow 2
    val e4 = b pow 3
    val e5 = a pow -1
    val e6 = b pow -1
    assertEquals(Sign.Negative, e1.sign)
    assertEquals(Sign.Unknown, e2.sign)
    assertEquals(Sign.Positive, e3.sign)
    assertEquals(Sign.Negative, e4.sign)
    assertEquals(Sign.Positive, e5.sign)
    assertEquals(Sign.Negative, e6.sign)
  }

  @Test
  def intervalArithmetic(): Unit = {
    val a = Var("a", Interval(2,4))
    val b = Var("b", Interval(-5,3))
    val e_1 = a+b
    assertEquals(Sign.Unknown,e_1.sign)
    assertEquals(Cst(-3),e_1.min)
    assertEquals(Cst(7),e_1.max)
    val e_2 = a*b
    assertEquals(Sign.Unknown,e_2.sign)
    assertEquals(Cst(-20),e_2.min)
    assertEquals(Cst(12),e_2.max)
    val e_3 = b pow 2
    assertEquals(Sign.Positive,e_3.sign)
    assertEquals(Cst(0),e_3.min)
    assertEquals(Cst(25),e_3.max)
    val c = Var("c", Interval(-7,-2))
    val e_5 = a*c
    assertEquals(Sign.Negative,e_5.sign)
    assertEquals(Cst(-28),e_5.min)
    assertEquals(Cst(-4),e_5.max)
  }

  def floorWithRange(): Unit = {
    val a = Var("a", Interval(2,4))
    val e_1 = a pow -1
    assertEquals(Cst(0),floor(e_1))
    assertEquals(Cst(1),ceil(e_1))
    val e_2 = a * ((3+a) pow -1)
    assertEquals(Cst(0), floor(e_2))
  }

  @Test
  def intDivWithRanges(): Unit = {
    val i = Var("i", Interval(2,3))
    assertEquals(Cst(1), (3+i) / (2+i))
    assertEquals(Cst(2), (4+3*i)  / (2+i))
    val j = Var("j", Interval(4,7))
    assertEquals(3+i, (7+3*j+2*i+i*j) / (2+j))
    assertEquals(3+i, (7+3*j+3*i+i*j) / (2+j))
    assertEquals(Cst(2), (6+2*i+3*j) / (3+i+j))
  }

  @Test
  def modDivWithRanges(): Unit = {
    val i = Var("i", Interval(2,3))
    assertEquals(Cst(1), (3+i) % (2+i))
    assertEquals(i, (4+3*i)  % (2+i))
    val j = Var("j", Interval(4,7))
    assertEquals(Cst(1),(7+3*j+2*i+i*j) % (2+j))
    assertEquals(1+i,(7+3*j+3*i+i*j) % (2+j))
    assertEquals(j, (6+2*i+3*j) % (3+i+j))
  }
}
