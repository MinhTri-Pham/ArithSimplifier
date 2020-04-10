package arithmetic

import java.util.concurrent.atomic.AtomicLong

import arithmetic.NotEvaluableException.NotEvaluable
import arithmetic.simplifier._

import scala.language.implicitConversions
import scala.collection.mutable.ListBuffer

// Base class for arithmetic expressions
abstract sealed class ArithExpr {
  // Addition operator
  def +(that: ArithExpr) : ArithExpr = SimplifySum(this, that)

  // Subtraction operator (x-y = x+(-1)*y)
  def -(that : ArithExpr) : ArithExpr = SimplifySum(this, Cst(-1)*that)

  // Multiply operator
  def *(that: ArithExpr) : ArithExpr = SimplifyProd(this, that)

  // Ordinary division operator (x/y = x*(y pow -1))
  def /^(that: ArithExpr) : ArithExpr = SimplifyProd(this, that pow -1)

  // Raising to power operator
  def pow(that: Int) : ArithExpr = SimplifyPow(this, that)

  // Integer division
  def /(that: ArithExpr) : ArithExpr =
    // Require both operators are positive
    if (!this.isInt || !that.isInt) throw new ArithmeticException
    else {
      (this.sign, that.sign) match
      {
        // Use floor or ceiling depending on signs
        case (Sign.Positive, Sign.Positive) | (Sign.Negative, Sign.Negative) => SimplifyFloor(this * (that pow -1))
        case (Sign.Positive, Sign.Negative) | (Sign.Negative, Sign.Positive) => SimplifyCeiling(this * (that pow -1))
        // For now use floor if signs undetermined
        case _ => SimplifyFloor(this * (that pow -1))
      }
    }


  // Modulo operator
  def %(that: ArithExpr) : ArithExpr =
    if (!this.isInt || !that.isInt) throw new ArithmeticException
    else this - ((this / that) * that)

  // Differential operator - first order and nth order
  def diff(v:Var) : ArithExpr = Differentiate(this,v)
  def diff(v:Var, n:Int) : ArithExpr = Differentiate(this,v,n)

  // Convert expression to variable if possible
  lazy val toVar : Option[Var] = this match {
    case x: Var => Some(x)
    case _ => None
  }

  // Convert expression to sum if possible
  lazy val toSum : Option[Sum] = this match {
    case x:Sum => Some(x)
    case x:Prod => x.asSum
    case x:Pow => x.asSum
    case _ => None
  }

  // Convert expression to product if possible
  lazy val toProd : Option[Prod] = this match {
    case x:Prod => Some(x)
    case x:Sum => x.asProd
    case x:Pow => x.asProd
    case _ => None
  }

  lazy val sign: Sign.Value = Sign(this)

  lazy val evalDouble: Double = ArithExpr.evalDouble(this)

  lazy val (min: ArithExpr, max: ArithExpr) = _minmax()

  // Min and max if possible
  private def _minmax(): (ArithExpr, ArithExpr) = this match {
    case c:Cst => (c,c)
    case v:Var => (v.range.intervalMin, v.range.intervalMax)
    case PosInf => (PosInf, PosInf)
    case NegInf => (NegInf, NegInf)
    case s:Sum => (s.terms.map(_.min).reduce[ArithExpr](_ + _), s.terms.map(_.max).reduce[ArithExpr](_ + _))
    case p:Prod =>
      val prodInterval = ArithExpr.computeIntervalProd(p.factors)
      (prodInterval.intervalMin, prodInterval.intervalMax)
    case c: CeilingFunction => (ceil(c.ae.min), ceil(c.ae.max))
    case f: FloorFunction => (floor(f.ae.min), floor(f.ae.max))
    case Pow(b,e) =>
      // Odd exponent - easy
      if (e > 0 && e % 2 == 1) (b.min pow e, b.max pow e)
      // Even exponent, consider sign of min/max of b
      else if (e > 0 && e % 2 == 0) {
        if (b.min.sign.equals(Sign.Positive)) (b.min pow e, b.max pow e)
        else if (b.max.sign.equals(Sign.Negative)) (b.max pow e, b.min pow e)
        // (0, max(x1^e,x2^e)), x1 = min(b), x2 = max(b)
        else {
          val x1 = b.min pow e
          val x2 = b.max pow e
          val comp = ArithExpr.isSmaller(x1,x2)
          if (comp.isDefined) {
            if (comp.get) (Cst(0), x2)
            else (Cst(0),x1)
          }
          else {
            (Cst(0), ?)
          }
        }
      }
      else {
        // Caution for 0
        if (b.min == Cst(0)) return (b.max pow e, PosInf)
        if (b.max == Cst(0)) return (b.max pow e, NegInf)
        // Both positive or both negative easy
        if (b.min.sign.equals(Sign.Positive)) return (b.max pow e, b.min pow e)
        if (b.max.sign.equals(Sign.Negative)) return (b.min pow e, b.max pow e)
        // Minimum negative and maximum positive
        // Even negative exponent
        if (e % 2 == 0) {
          // Try to evaluate min and max of base
          // Then use absolute value
          try {
            val minAbs = abs(b.min)
            val maxAbs = abs(b.max)
            val comp = ArithExpr.isSmaller(maxAbs,minAbs)
            if (comp.isDefined) {
              if (comp.get) (b.min pow e, b.max pow e)
              else (b.max pow e, b.min pow e)
            }
            else (?,?)
          } catch {
            case NotEvaluableException() => (?,?)
          }
        }
        // Odd negative easy
        else (b.min pow e, b.max pow e)
      }

    case _ => (?,?)
  }

  lazy val isEvaluable: Boolean = {
    !ArithExpr.visitUntil(this, x => {
      x.isInstanceOf[Var] || x == PosInf || x == NegInf || x == ?
    })
  }

  // Indicates whether an expression is an integer
  lazy val isInt : Boolean = this match {
    case _: Cst => true
    case v: Var => v.isInteger
    case s:Sum => s.terms.forall(_.isInt)
    case p:Prod => p.factors.forall(_.isInt)
    case Pow(b, e) => b.isInt && e >= 0
    case FloorFunction(_) => true
    case CeilingFunction(_) => true
    case AbsFunction(ae) => ae.isInt
    case PosInf => true
    case NegInf => true
    case _ => false
  }

  // Get terms for sum simplification
  lazy val getTerms : List[ArithExpr] = this match {
    case s:Sum => s.terms
    case _ => List(this)
  }

  // Get factors for product simplification
  lazy val getFactors : List[ArithExpr] = this match {
    case p:Prod => p.factors
    case _ => List(this)
  }

  // Special case when expression is a product of two factors: a sum and negative power
  // Return the sum and the inverse of the power
  // E.g. (ac+bc+d)(a+b)^{-1} returns (ac+bc+d,a+b)
  // Is used for sum, floor and ceiling simplification
  lazy val asSumFraction : Option[(Sum, ArithExpr)] = {
    if (getFactors.length != 2) None
    else (getFactors.head, getFactors(1)) match {
      case (s:Sum, p:Pow) =>
        if (p.e > 0 || p.b.isInstanceOf[Cst]) None
        else Some(s, SimplifyPow(p.b,-p.e))
      case (p:Pow, s:Sum) =>
        if (p.e > 0 || p.b.isInstanceOf[Cst]) None
        else Some(s,SimplifyPow(p.b,-p.e))
      case _ => None
    }
  }

  def visitAndRebuild(f: ArithExpr => ArithExpr): ArithExpr

  def digest(): Int

  override def hashCode: Int = digest()

  def HashSeed(): Int
}

// Constants
case class Cst(value : Long) extends ArithExpr {

  override val HashSeed: Int = java.lang.Long.hashCode(value)

  override lazy val digest: Int = java.lang.Long.hashCode(value)

  override def visitAndRebuild(f: ArithExpr => ArithExpr): ArithExpr = f(this)

  override def equals(that: Any): Boolean = that match {
    case Cst(n) => n == value
    case _ => false
  }
  override def toString: String = {
    if (value >= 0) value.toString
    else s"(${value.toString})"
  }
}

// Variables
case class Var (name : String, range: Interval = Interval(), fixedId: Option[Long] = None, isInteger:Boolean = true) extends ArithExpr {

  override lazy val hashCode: Int = 8 * 79 + id.hashCode

  override val HashSeed = 0x54e9bd5e

  override lazy val digest: Int = HashSeed ^ name.hashCode ^ id.hashCode

  // Unique id for variables, equality of variables is based on this, not name
  val id: Long = {
    if (fixedId.isDefined)
      fixedId.get
    else {
      Var.incCnt
    }
  }

  override def equals(that: Any): Boolean = that match {
    case v: Var => this.id == v.id
    case _ => false
  }

  override def toString: String = name

  override def visitAndRebuild(f: ArithExpr => ArithExpr): ArithExpr = f(this)
}


object Var {
  private val cnt = new AtomicLong(-1) // Instance counter

  // Counter incremented every time a variable is initialised
  def incCnt : Long = {
    var _id: Long = 0
    do {
      _id = Var.cnt.incrementAndGet()
      if (_id < 0) {
        Var.cnt.compareAndSet(_id,0)
      }
    } while (_id < 0)
      _id
  }

  def apply(name: String): Var = new Var(name, Interval())

  def apply(name: String, isInt: Boolean): Var = new Var(name, Interval(),None,isInt)
}

// Negative variables
object NegVar{
  def apply(name: String): Var = new Var(name, Interval(NegInf, Cst(0)))
}

// Sums
case class Sum(terms: List[ArithExpr]) extends ArithExpr {

  override val HashSeed = 0x8e535130

  override lazy val digest: Int = terms.foldRight(HashSeed)((x, hash) => hash ^ x.digest())

  lazy val cstTerm : Long = terms.head match {
    case Cst(c) => c
    case _ => 0
  }

  // Product representation if possible, e.g. ac+bc = a(b+c)
  lazy val asProd : Option[Prod] = {
    val factorisation = Factorise(this)
    if (factorisation.isDefined) {
      factorisation.get match {
        case p:Prod => Some(p)
        case _ => None
      }
    }
    else None
  }

  // Power representation if possible, e.g. a^2+2ab+b^2 = (a+b)^2
  lazy val asPow : Option[Pow] = {
    val factorisation = Factorise(this)
    if (factorisation.isDefined) {
      factorisation.get match {
        case p:Pow => Some(p)
        case _ => None
      }
    }
    else None
  }

  override def equals(that: Any): Boolean = that match {
    case Sum(terms2) => terms.length == terms2.length && terms.intersect(terms2).length == terms.length
    case _ => false
  }

  override def toString: String = s"(${terms.mkString(" + ")})"

  override def visitAndRebuild(f: ArithExpr => ArithExpr): ArithExpr =
    f(terms.map(_.visitAndRebuild(f)).reduce(_ + _))
}

object Sum {
  // Convert any expression to sum if possible
  def unapply(ae: Any): Option[List[ArithExpr]] = ae match {
    case aexpr: ArithExpr => aexpr match {
      case s: Sum => Some(s.terms)
      case pw : Pow => if (pw.asSum.isDefined) Some(pw.asSum.get.terms) else None
      case p:Prod =>
        if (p.asPartialCancelledSum.isDefined) Some(p.asPartialCancelledSum.get.getTerms )
        else if (p.asSum.isDefined) Some(p.asSum.get.terms)
        else None
      case _ => None
    }
    case _ => None
  }
}

// Products
case class Prod(factors: List[ArithExpr]) extends ArithExpr {

  override val HashSeed = 0x286be17e

  override lazy val digest: Int = factors.foldRight(HashSeed)((x, hash) => hash ^ x.digest())

  lazy val cstFactor : Long = factors.head match {
    case Cst(c) => c
    case _ => 1
  }

  // List of non-constant factors
  lazy val nonCstList : List[ArithExpr] = {
    factors.filter(!_.isInstanceOf[Cst])
  }

  // Non-constant factor
  lazy val nonCstFactor : ArithExpr = {
    if (cstFactor == 1) this
    else this.nonCstList.reduce((x, y) => x*y)
  }


  // Sum where all scalar multiples are expanded, e.g. 2a+b = a+a+b
  lazy val asNonCstFactorsSum : Option[Sum] = if (factors.length < 2 || cstFactor < 2) None else {
    val terms = ListBuffer[ArithExpr]()
    for (_ <- 0L until cstFactor) {
      terms += nonCstFactor
    }
    Some(Sum(terms.toList))
  }

  // Expansion of product if possible, e.g. a(b+c) = ab+ac
  lazy val asSum : Option[Sum] = {
    if (factors.length <= 1) None
    else {
      var accum : ArithExpr = Cst(1) // Accumulated expansion so far
      // Whether found a sum factor, this also means accumulated expression is a sum
      // Expansion function will have to be use from then on
      var expanded = false
      // Build expansion factor by factor
      for (f <- factors) f match {
        case x @ (_:Cst | _:Var) =>
          if (expanded) accum = ArithExpr.expand(accum, x).get // Expand everything out
          else accum *= x // Nothing to expand, leave as it is
        case s:Sum =>
          // Expand sum factor out
          accum = ArithExpr.expand(accum,s).get
          expanded = true
        case p:Pow =>
          if (expanded) accum = ArithExpr.expand(accum,p).get // Accumulated expansion is already a sum
          else if (p.asSum.isDefined) {
            // Power can be represented as a sum and we haven't expanded out anything yet
            accum = ArithExpr.expand(accum,p.asSum.get).get
            expanded = true
          }
          else accum *= p // Nothing to expand, leave as it is
        case _ =>
          // The factor can't be represented as a sum, so check whether accumulated expression if a sum already
          if (expanded) accum = ArithExpr.expand(accum, f).get
          else accum *= f
      }
      if (expanded) accum.toSum
      else None
    }
  }

  // Special sum representation for products of a sum and negative a power
  // Looks to partially cancel a part of the sum with the power
  // E.g. (ac+bc+d)(a+b)^{-1} = c+d(a+b)^{-1}
  lazy val asPartialCancelledSum : Option[ArithExpr] = {
    if (this.asSumFraction.isDefined) {
      // Check that the product satisfies criteria, get the sum and inverse of power
      val numer = this.asSumFraction.get._1 // Example above: numer = ac+bc+d
      val denom = this.asSumFraction.get._2 // Example above: denom = a+b
      // Expand out scalar constants and constants as ones
      // E.g. 5+3b = 1+1+1+1+1+b+b+b
      // To find all subsets of sums with constants in them
      val termsExpanded = Helper.expandTermsCst(numer.terms)
      val termSubsets = Helper.powerSet(termsExpanded).filter(_.nonEmpty)
      if (termSubsets.nonEmpty) {
        var partialCancel : Option[ArithExpr] = None // Assume this form is not possible
        var i = 0
        while (i < termSubsets.length) {
          val subset = termSubsets(i)
          val restTerms = termsExpanded.diff(subset)
          val subsetSum = subset.reduce(_ + _)
          val rest = if (restTerms.isEmpty) Cst(0) else restTerms.reduce(_ + _)
          // Try to find a subset of a sum that cancels out of
          val gcd = ComputeGCD(subsetSum, denom)
          if (gcd != Cst(1)) {
            // Return quotient plus remainder
            partialCancel = Some(subsetSum /^ denom + rest /^ denom)
            i = termSubsets.length
          }
          i += 1
        }
        partialCancel
      }
      else None
    }
    else None
  }

  override def equals(that: Any): Boolean = that match {
    case Prod(factors2) => factors.length == factors2.length && factors.intersect(factors2).length == factors.length
    case _ => false
  }

  override def toString: String = factors.mkString(" * ")

  override def visitAndRebuild(f: ArithExpr => ArithExpr): ArithExpr =
    f(factors.map(_.visitAndRebuild(f)).reduce(_ * _))
}

object Prod {
  // Convert an expression into a product if possible
  def unapply(ae: Any): Option[List[ArithExpr]] = ae match {
    case aexpr: ArithExpr => aexpr match {
      case p: Prod => Some(p.factors)
      case p: Pow if p.asProdPows.isDefined => Some(p.asProdPows.get.factors)
      case s: Sum if s.asProd.isDefined => Some(s.asProd.get.factors)
      case _ => None
    }
    case _ => None
  }
}

// Power (with integer constants)
case class Pow(b: ArithExpr, e: Int) extends ArithExpr {

  override val HashSeed = 0x63fcd7c2

  override lazy val digest: Int = HashSeed ^ b.digest() ^ Cst(e).digest

  // Expansion into sum if possible (the base must be a sum)
  lazy val asSum : Option[Sum] = if (e < 0 || !b.isInstanceOf[Sum]) None else {
    // Expand n times, starting with base itself
    var combined : ArithExpr = Cst(0)
    for (n <- 1 to e) {
      if (n == 1)
        combined = b
      else
        combined = ArithExpr.expand(combined,b).get
    }
    combined.toSum
  }


  // Product representation (a^n = a*a...*a (n times))
  lazy val asProd : Option[Prod] = {
    //assume(e > 1)
    if (e > 1) Some(Prod(List.fill(e)(b)))
    else if (e < 1) Some(Prod(List.fill(scala.math.abs(e))(Pow(b,-1))))
    // Exponent is 1 or -1
    else None
  }

  // Power as product of powers
  lazy val asProdPows : Option[Prod]= b match {
    // Product base
    // (ab)^2 = a^2*b^2
    case p:Prod =>
      val bfacts = b.getFactors
      val pfacts = bfacts.map(x => x pow e)
      // Negative constant
      if (p.cstFactor < 0) {
        val flattened = pfacts.flatMap(x => x.getFactors)
        Some(Prod(flattened))
      }
      else Some(Prod(pfacts))
    // Or when a sum can be factorised as product
    case _:Sum =>
      if (b.toProd.isDefined) {
        val bfacts = b.toProd.get.factors
        val pfacts = bfacts.map(x => x pow e)
        Some(Prod(pfacts))
      }
      else None
    case _ => None
  }

  override def equals(that: Any): Boolean = that match {
    case Pow(b2,e2) => b == b2 && e == e2
    case _ => false
  }

  override def toString: String = {
    if (e == -1 && b.isInstanceOf[Cst]) s"1/${b.toString}"
    else s"pow(${b.toString},$e)"
  }

  override def visitAndRebuild(f: ArithExpr => ArithExpr): ArithExpr =
    f(b.visitAndRebuild(f).pow(e))
}

object abs {
  def apply(ae: ArithExpr): ArithExpr = SimplifyAbs(ae)
}

case class AbsFunction(ae: ArithExpr) extends ArithExpr {

  override val HashSeed = 0x3570a2ce

  override lazy val digest: Int = HashSeed ^ ae.digest()

  override def toString: String = "Abs(" + ae + ")"

  override def visitAndRebuild(f: ArithExpr => ArithExpr): ArithExpr =
    f(abs(ae.visitAndRebuild(f)))
}

// Floor
case class FloorFunction(ae: ArithExpr) extends ArithExpr {

  override val HashSeed = 0x558052ce

  override lazy val digest: Int = HashSeed ^ ae.digest()

  override def toString: String = "Floor(" + ae + ")"

  override def visitAndRebuild(f: ArithExpr => ArithExpr): ArithExpr =
    f(floor(ae.visitAndRebuild(f)))
}

object floor {
  def apply(ae: ArithExpr): ArithExpr = simplifier.SimplifyFloor(ae)
}

// Ceiling
case class CeilingFunction(ae: ArithExpr) extends ArithExpr {

  override val HashSeed = 0xa45d23d0

  override lazy val digest: Int = HashSeed ^ ae.digest()

  override def toString: String = "Ceiling(" + ae + ")"

  override def visitAndRebuild(f: ArithExpr => ArithExpr): ArithExpr =
    f(ceil(ae.visitAndRebuild(f)))
}

object ceil {
  def apply(ae: ArithExpr): ArithExpr = SimplifyCeiling(ae)
}

// Logarithm
case class LogFunction(b:Long, ae:ArithExpr) extends ArithExpr {
  override val HashSeed = 0x370285bf

  override lazy val digest: Int = HashSeed ^ Cst(b).digest ^ ~ae.digest()

  override def toString: String = "log" + b + "(" + ae + ")"

  override def visitAndRebuild(f: ArithExpr => ArithExpr): ArithExpr =
    f(log(b, ae.visitAndRebuild(f)))
}

object log {
  def apply(b:Long, ae:ArithExpr) : ArithExpr = SimplifyLog(b,ae)
}


object ArithExpr {

  // Don't have to write Cst every time
  implicit def intToCst(i: Int): ArithExpr = Cst(i)

  // Canonical order of terms, factors for sums, resp. products
  val isCanonicallySorted: (ArithExpr, ArithExpr) => Boolean = (x: ArithExpr, y: ArithExpr) => {
    (x, y) match {
      case (Cst(a), Cst(b)) => a < b
      case (_: Cst, _) => true // constants first
      case (_, _: Cst) => false
      case (x: Var, y: Var) => x.id < y.id // order variables based on id
      case (_: Var, _) => true // variables always after constants second
      case (_, _: Var) => false
//      case (p1:Prod, p2:Prod) => p1.factors.zip(p2.factors).map(x => isCanonicallySorted(x._1, x._2)).foldLeft(false)(_ || _)
      case (p1: Prod, p2: Prod) =>
        if (p1.factors.length < p2.factors.length) true
        else if (p2.factors.length < p1.factors.length) false
        else {
          val n = p1.factors.length
          var isSorted = false
          var i = 0
          while (i < n) {
            if (p1.factors(i) != p2.factors(i)) {
              isSorted = isCanonicallySorted(p1.factors(i),p2.factors(i))
              i = n
            }
            i += 1
          }
          isSorted
        }
      case _ => x.HashSeed() < y.HashSeed() || (x.HashSeed() == y.HashSeed() && x.digest() < y.digest())
    }
  }

  def substitute(e: ArithExpr, substitutions: scala.collection.Map[ArithExpr, ArithExpr]): ArithExpr =
    e.visitAndRebuild(expr =>
      if (substitutions.isDefinedAt(expr))
        substitutions(expr)
      else
        expr
    )

  // Evaluates an expression given constant substitutions for variables
  def evaluate(expr: ArithExpr, subs : scala.collection.Map[Var, Cst]) : Long = expr match {
    case Cst(c) => c
    case v: Var => findSubstitute(v, subs)
    case s:Sum => s.terms.foldLeft(0L) { (accumulated, term) => accumulated + evaluate(term, subs)}
    case p:Prod => p.factors.foldLeft(1L) { (accumulated, factor) => accumulated * evaluate(factor, subs)}
    case Pow(b,e) => scala.math.pow(evaluate(b,subs),e).toInt
    case FloorFunction(ae) => scala.math.floor(evaluate(ae,subs)).toLong
    case CeilingFunction(ae) => scala.math.ceil(evaluate(ae,subs)).toLong
    case AbsFunction(ae) => scala.math.abs(evaluate(ae,subs))
    case _ => throw NotEvaluable
  }

  // Find if variable contained in mapping
  private def findSubstitute(variable: Var, replacements : scala.collection.Map[Var, Cst]) : Long = {
    for ((varSub, Cst(n)) <- replacements) {
      if (variable == varSub) return n
    }
    throw NotEvaluable
  }

  // Expands product of two expressions into a sum if possible
  def expand(e1: ArithExpr, e2: ArithExpr) : Option[Sum] = (e1,e2) match {
    case (s1: Sum, s2:Sum) =>
      val lts = s1.terms
      val rts = s2.terms
      var combined: ArithExpr = Cst(0)
      // Two sums, expand term by term on both sides
      for (lt <- lts) {
        for (rt <- rts) {
          val prod = lt*rt
          combined += prod
        }
      }
      Some(Sum(combined.getTerms))

    case (s: Sum, _) =>
      val lts = s.terms
      var combined: ArithExpr = Cst(0)
      // Multiply every term in e1 by e2 and add everything up
      for (lt <- lts) {
        combined += lt * e2
      }
      Some(Sum(combined.getTerms))


    case (_, s: Sum) =>
      val rts = s.terms
      var combined: ArithExpr = Cst(0)
      // Multiply every term in e2 by e1 and add everything up
      for (rt <- rts) {
        combined += e1 * rt
      }
      Some(Sum(combined.getTerms))

    case _ => None
  }

  // Check if an expression is a multiple of another expression
  def isMultipleOf(ae1: ArithExpr, ae2: ArithExpr) : Boolean = (ae1, ae2) match {
    // Check multiple of constants
    case (Cst(c1), Cst(c2)) => c1 % c2 == 0
    case (p:Prod, c:Cst) => p.cstFactor % c.value == 0
    case (Pow(b1, e1), Pow(b2, e2)) if e1 >= e2 && e2 > 0 => isMultipleOf(b1, b2)
    case (Pow(b1, e1), Pow(b2, e2)) if e1 <= e2 && e2 < 0 => isMultipleOf(b1, b2)
    case (p:Pow, _) if p.e > 0 => isMultipleOf(p.b,ae2)
    case (p1:Prod, p2:Prod) =>
      val p1Factors = p1.factors
      val p2Factors = p2.factors
      p2Factors.forall(factor => p1Factors.exists(isMultipleOf(_, factor)))
    case (p:Prod, _) => p.factors.contains(ae2) ||
      p.factors.foldLeft(false){(accum,factor) => accum || isMultipleOf(factor,ae2)}
    case (x, y) => x == y
    case _ => false
  }

  // Check if an expression is a smaller than another expression
  def isSmaller(ae1: ArithExpr, ae2: ArithExpr) : Option[Boolean] = {
    if (ae1 == ? | ae2 == ?)
      return None

    try {
      // we check to see if the difference can be evaluated
      val diff = ae2 - ae1
      if (diff.isEvaluable)
        return Some(diff.evalDouble > 0)
    } catch {
      case NotEvaluableException() =>
    }

    var lhsNonCommon = ae1
    var rhsNonCommon = ae2

    (ae1, ae2) match {
      case (s1:Sum,s2:Sum)=>
        // Two sums: filter out common terms first
        // Example: x+y < y+z if x < z (assuming y has a finite range)
        val ae1Terms = s1.terms
        val ae2Terms = s2.terms
        val commonTerms = ae1Terms.intersect(ae2Terms)
        if (commonTerms.nonEmpty) {
          val ae1diff = ae1Terms.diff(commonTerms)
          val ae2diff = ae2Terms.diff(commonTerms)
          if (ae1diff.isEmpty) lhsNonCommon = Cst(0)
          else if (ae1diff.length == 1) lhsNonCommon = ae1diff.head
          else lhsNonCommon = Sum(ae1diff)

          if (ae2diff.isEmpty) rhsNonCommon = Cst(0)
          else if (ae2diff.length == 1) rhsNonCommon = ae2diff.head
          else rhsNonCommon = Sum(ae2diff)
        }
      // Product or powers present: take out common term
      case (_:Prod, _) | (_, _:Prod) | (_:Pow, _) | (_, _:Pow) =>
        var ae1NonCommon = ae1
        var ae2NonCommon = ae2
        val gcd = ComputeGCD(ae1, ae2)
        if (gcd != Cst(1)) {
          ae1NonCommon = ae1 /^ gcd
          ae2NonCommon = ae2 /^ gcd
          // If signs of common or uncommon parts not known, can't determine
          if (gcd.sign == Sign.Unknown || ae1NonCommon.sign == Sign.Unknown || ae2NonCommon.sign == Sign.Unknown)
            return None
          // Depending on signs on gcd and uncommon parts, determine further action
          (gcd.sign, ae1NonCommon.sign, ae2NonCommon.sign) match {
            case (Sign.Positive, Sign.Positive, Sign.Positive) =>
              lhsNonCommon = ae1NonCommon
              rhsNonCommon = ae2NonCommon
            case (Sign.Positive, Sign.Positive, Sign.Negative) =>
              return Some(false)
            case (Sign.Positive, Sign.Negative, Sign.Positive) =>
              return Some(true)
            case (Sign.Positive, Sign.Negative, Sign.Negative) =>
              lhsNonCommon = ae2NonCommon
              rhsNonCommon = ae1NonCommon
            case (Sign.Negative, Sign.Positive, Sign.Positive) =>
              lhsNonCommon = ae2NonCommon
              rhsNonCommon = ae1NonCommon
            case (Sign.Negative, Sign.Positive, Sign.Negative) =>
              return Some(true)
            case (Sign.Negative, Sign.Negative, Sign.Positive) =>
              return Some(false)
            case (Sign.Negative, Sign.Negative, Sign.Negative) =>
              lhsNonCommon = ae1NonCommon
              rhsNonCommon = ae2NonCommon
            case _ =>
          }
        }
      // Infinite values
      case (PosInf, PosInf) => return None
      case (NegInf, NegInf) => return None
      case (PosInf, NegInf) => return Some(false)
      case (NegInf, PosInf) => return Some(true)
      case (PosInf, _) if ae2.isEvaluable => return Some(false)
      case (NegInf, _) if ae2.isEvaluable => return Some(true)
      case (_, NegInf) if ae1.isEvaluable => return Some(false)
      case (_, PosInf) if ae1.isEvaluable => return Some(true)
      // Keep going
      case _ =>
    }

    try {
      val lhsNonCommonMinEval = lhsNonCommon.min.evalDouble
      val lhsNonCommonMaxEval = lhsNonCommon.max.evalDouble
      val rhsNonCommonMinEval = rhsNonCommon.min.evalDouble
      val rhsNonCommonMaxEval = rhsNonCommon.max.evalDouble
      if (lhsNonCommonMinEval <= rhsNonCommonMaxEval && rhsNonCommonMinEval <= lhsNonCommonMaxEval) return None
      else return Some(lhsNonCommonMaxEval < rhsNonCommonMinEval)

    } catch {
      case NotEvaluableException() =>
    }

    None // Can't determine if ae1 < ae2
  }

  // Check if an expression is a bigger than another expression
  def isBigger(ae1: ArithExpr, ae2: ArithExpr) : Option[Boolean] = {
    if (ae1 == ? | ae2 == ?)
      return None

    try {
      // we check to see if the difference can be evaluated
      val diff = ae2 - ae1
      if (diff.isEvaluable)
        return Some(diff.evalDouble < 0)
    } catch {
      case NotEvaluableException() =>
    }

    var lhsNonCommon = ae1
    var rhsNonCommon = ae2

    (ae1, ae2) match {
      case (s1:Sum,s2:Sum)=>
        // Two sums: filter out common terms first
        // Example: x+y < y+z if x < z (assuming y has a finite range)
        val ae1Terms = s1.terms
        val ae2Terms = s2.terms
        val commonTerms = ae1Terms.intersect(ae2Terms)
        if (commonTerms.nonEmpty) {
          val ae1diff = ae1Terms.diff(commonTerms)
          val ae2diff = ae2Terms.diff(commonTerms)
          if (ae1diff.isEmpty) lhsNonCommon = Cst(0)
          else if (ae1diff.length == 1) lhsNonCommon = ae1diff.head
          else lhsNonCommon = Sum(ae1diff)

          if (ae2diff.isEmpty) rhsNonCommon = Cst(0)
          else if (ae2diff.length == 1) rhsNonCommon = ae2diff.head
          else rhsNonCommon = Sum(ae2diff)
        }
      // Product/power and product/power: take out common term
      case (_:Prod, _) | (_, _:Prod) | (_:Pow, _) | (_, _:Pow) =>
        var ae1NonCommon = ae1
        var ae2NonCommon = ae2
        val gcd = ComputeGCD(ae1, ae2)
        if (gcd != Cst(1)) {
          ae1NonCommon = ae1 /^ gcd
          ae2NonCommon = ae2 /^ gcd
          // If signs of common or uncommon parts not known, can't determine
          if (gcd.sign == Sign.Unknown || ae1NonCommon.sign == Sign.Unknown || ae2NonCommon.sign == Sign.Unknown)
            return None
          // Depending on signs on gcd and uncommon parts, determine further action
          (gcd.sign, ae1NonCommon.sign, ae2NonCommon.sign) match {
            case (Sign.Positive, Sign.Positive, Sign.Positive) =>
              lhsNonCommon = ae1NonCommon
              rhsNonCommon = ae2NonCommon
            case (Sign.Positive, Sign.Positive, Sign.Negative) =>
              return Some(true)
            case (Sign.Positive, Sign.Negative, Sign.Positive) =>
              return Some(false)
            case (Sign.Positive, Sign.Negative, Sign.Negative) =>
              lhsNonCommon = ae2NonCommon
              rhsNonCommon = ae1NonCommon
            case (Sign.Negative, Sign.Positive, Sign.Positive) =>
              lhsNonCommon = ae2NonCommon
              rhsNonCommon = ae1NonCommon
            case (Sign.Negative, Sign.Positive, Sign.Negative) =>
              return Some(false)
            case (Sign.Negative, Sign.Negative, Sign.Positive) =>
              return Some(true)
            case (Sign.Negative, Sign.Negative, Sign.Negative) =>
              lhsNonCommon = ae1NonCommon
              rhsNonCommon = ae2NonCommon
            case _ =>
          }
        }
      // Keep going
      case (PosInf, PosInf) => return None
      case (NegInf, NegInf) => return None
      case (PosInf, NegInf) => return Some(true)
      case (NegInf, PosInf) => return Some(false)
      case (PosInf, _) if ae2.isEvaluable => return Some(true)
      case (NegInf, _) if ae2.isEvaluable => return Some(false)
      case (_, NegInf) if ae1.isEvaluable => return Some(true)
      case (_, PosInf) if ae1.isEvaluable => return Some(false)
      case _ =>
    }

    // If non-common parts evaluable, evaluate min and max and find final result
    try {
      val lhsNonCommonMinEval = lhsNonCommon.min.evalDouble
      val lhsNonCommonMaxEval = lhsNonCommon.max.evalDouble
      val rhsNonCommonMinEval = rhsNonCommon.min.evalDouble
      val rhsNonCommonMaxEval = rhsNonCommon.max.evalDouble
      if (lhsNonCommonMaxEval >= rhsNonCommonMinEval && rhsNonCommonMaxEval >= lhsNonCommonMinEval) return None
      else return Some(lhsNonCommonMinEval > rhsNonCommonMaxEval)

    } catch {
      case NotEvaluableException() =>
    }

    None // Can't determine if ae1 > ae2
  }

  def isSmallerOrEqual(ae1: ArithExpr, ae2: ArithExpr) : Option[Boolean] = {
    if (ae1 == ae2) return Some(true)
    isSmaller(ae1,ae2)
  }

  def isBiggerOrEqual(ae1: ArithExpr, ae2: ArithExpr) : Option[Boolean] = {
    if (ae1 == ae2) return Some(true)
    isBigger(ae1,ae2)
  }


  // Minimum of expressions (if possible)
  def minList(aes: List[ArithExpr]) : ArithExpr = {
    // Go through list, keep picking the min
    aes.reduce((ae1,ae2) => {
      val comp = isSmaller(ae2,ae1)
      if (comp.isDefined) {
        if (comp.get) ae2
        else ae1
      }
      else ?
    })
  }

  // Minimum of expressions (if possible)
  def maxList(aes: List[ArithExpr]) : ArithExpr = {
    // Go through list, keep picking the max
    aes.reduce((ae1,ae2) => {
      val comp = isBigger(ae2,ae1)
      if (comp.isDefined) {
        if (comp.get) ae2
        else ae1
      }
      else ?
    })
  }

  def visitUntil(e: ArithExpr, f: ArithExpr => Boolean): Boolean = if (f(e)) true
  else e match {
    case Pow(base, _) =>
      visitUntil(base, f)
    case s:Sum =>
      s.terms.foreach(t => if (visitUntil(t, f)) return true)
      false
    case p:Prod =>
      p.factors.foreach(t => if (visitUntil(t, f)) return true)
      false
    case LogFunction(_, x) => visitUntil(x, f)
    case FloorFunction(expr) => visitUntil(expr, f)
    case CeilingFunction(expr) => visitUntil(expr, f)
    case _ => false
  }

  // Evaluate expression to numeric value if possible
  private def evalDouble(e: ArithExpr): Double = e match {
    case Cst(c) => c
    case Pow(base, exp) => scala.math.pow(evalDouble(base),exp)
    case s:Sum => s.terms.foldLeft(0.0)((result, expr) => result + evalDouble(expr))
    case p:Prod => p.factors.foldLeft(1.0)((result, expr) => result * evalDouble(expr))
    case FloorFunction(expr) => scala.math.floor(evalDouble(expr))
    case CeilingFunction(expr) => scala.math.ceil(evalDouble(expr))
    case AbsFunction(expr) => scala.math.abs(evalDouble(expr))
    case LogFunction(b,expr) => scala.math.log(scala.math.log(evalDouble(expr) / scala.math.log(b)))

    // Can't contain variables and special values
    case _ => throw NotEvaluable
  }

  // Product of two intervals
  private def computeIntervalProd(factors: List[ArithExpr]) : Interval = {
    val minMax = factors.map(x => Interval(x.min, x.max))
    minMax.reduce((x,y) => x*y)
  }
}

// Represents unknown expression
case object ? extends ArithExpr {
  override val HashSeed = 0x3fac31

  override val digest: Int = HashSeed

  override lazy val sign: Sign.Value = Sign.Unknown

  override def visitAndRebuild(f: ArithExpr => ArithExpr): ArithExpr = f(this)
}

case object PosInf extends ArithExpr {
  override val HashSeed = 0x4a3e87

  override val digest: Int = HashSeed

  override lazy val sign: Sign.Value = Sign.Positive

  override def visitAndRebuild(f: (ArithExpr) => ArithExpr): ArithExpr = f(this)
}

case object NegInf extends ArithExpr  {
  override val HashSeed = 0x4a3e87

  override val digest: Int = HashSeed

  override lazy val sign: Sign.Value = Sign.Negative

  override def visitAndRebuild(f: (ArithExpr) => ArithExpr): ArithExpr = f(this)
}