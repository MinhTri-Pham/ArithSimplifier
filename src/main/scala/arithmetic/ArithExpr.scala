package arithmetic

import java.util.concurrent.atomic.AtomicLong

import arithmetic.NotEvaluableException.NotEvaluable
import arithmetic.simplifier._
import scala.language.implicitConversions
import scala.collection.mutable.ListBuffer

abstract sealed class ArithExpr {
  // Addition operator
  def +(that: ArithExpr) : ArithExpr = SimplifySum(this, that)

  // Subtraction operator (x-y = x+(-1)*y)
  def -(that : ArithExpr) : ArithExpr = SimplifySum(this, Cst(-1)*that)

  // Multiply operator
  def *(that: ArithExpr) : ArithExpr = SimplifyProd(this, that)

  // Ordinal division operator (x/y = x*(y pow -1))
  def /^(that: ArithExpr) : ArithExpr = SimplifyProd(this, that pow -1)

  // Exponentiation operator
  def pow(that: Int) : ArithExpr = SimplifyPow(this, that)

  // Integer division
  def /(that: ArithExpr) : ArithExpr =
//    SimplifyFloor(this * (that pow -1))
    (this.sign, that.sign) match
    {
    case (Sign.Positive, Sign.Positive) | (Sign.Negative, Sign.Negative) => SimplifyFloor(this * (that pow -1))
    case (Sign.Positive, Sign.Negative) | (Sign.Negative, Sign.Positive) => SimplifyCeiling(this * (that pow -1))
    // Temporary
    case _ => SimplifyFloor(this * (that pow -1))
    }

  // Modulo operator
//  def %(that: ArithExpr) : ArithExpr = SimplifyMod(this, that)
  def %(that: ArithExpr) : ArithExpr = this - ((this / that) * that)

  // Differential operator
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
    case x:Prod => x.distributed
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

  private def _minmax(): (ArithExpr, ArithExpr) = this match {
    case c:Cst => (c,c)
    case v:Var => (v.range.intervalMin, v.range.intervalMax)

    case s:Sum => (s.terms.map(_.min).reduce[ArithExpr](_ + _), s.terms.map(_.max).reduce[ArithExpr](_ + _))
    case p:Prod =>
      val prodInterval = ArithExpr.computeIntervalProd(p.factors)
      (prodInterval.intervalMin, prodInterval.intervalMax)


//    case Sum(terms) => (terms.map(_.min).reduce[ArithExpr](_ + _), terms.map(_.max).reduce[ArithExpr](_ + _))
//    case Prod(factors) =>
//      val prodInterval = ArithExpr.computeIntervalProd(factors)
//      (prodInterval.intervalMin, prodInterval.intervalMax)

    case Pow(b,e) =>
      if (e == 0) (Cst(1), Cst(1))
      // Odd exponent - easy
      else if (e > 0 && e % 2 == 1) (b.min pow e, b.max pow e)
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
        // Both positive or both negative easy
        if (b.min.sign.equals(Sign.Positive)) (b.max pow e, b.min pow e)
        else if (b.max.sign.equals(Sign.Negative)) (b.min pow e, b.max pow e)
        // Minimum negative and maximum positive
        else {
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
      }

    case _ => (?,?)
  }

  lazy val isEvaluable: Boolean = {
    !ArithExpr.visitUntil(this, x => {
      x.isInstanceOf[Var] || x == ?
    })
  }

  lazy val isInt : Boolean = this match {
    case _: Cst => true
    case v: Var => v.isInteger
    case s:Sum => s.terms.forall(_.isInt)
    case p:Prod => p.factors.forall(_.isInt)
    case Pow(b, e) => b.isInt && e >= 0
    case FloorFunction(_) => true
    case CeilingFunction(_) => true
    case AbsFunction(ae) => ae.isInt
  }

  lazy val getTermsFactors : List[ArithExpr] = this match {
    case p:Prod => p.factors
    case s:Sum => s.terms
    case _ => List(this)
  }

  lazy val getTerms : List[ArithExpr] = this match {
    case s:Sum => s.terms
    case _ => List(this)
  }

  lazy val getFactors : List[ArithExpr] = this match {
    case p:Prod => p.factors
    case _ => List(this)
  }

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

  def digest(): Int

  override def hashCode: Int = digest()

  def HashSeed(): Int
}

// Class for (int) constants
case class Cst(value : Int) extends ArithExpr {

  override val HashSeed: Int = java.lang.Long.hashCode(value)

  override lazy val digest: Int = java.lang.Long.hashCode(value)

  // Prime decomposition
  lazy val asProd : Prod = Factorise(this).get.toProd.get

  override def equals(that: Any): Boolean = that match {
    case Cst(n) => n == value
    case _ => false
  }
  override def toString: String = {
    if (value >= 0) value.toString
    else s"(${value.toString})"
  }
}

// Class for variables
case class Var (name : String, range: Interval = Interval(), fixedId: Option[Long] = None, isInteger:Boolean = false) extends ArithExpr {

  override lazy val hashCode: Int = 8 * 79 + id.hashCode

  override val HashSeed = 0x54e9bd5e

  override lazy val digest: Int = HashSeed ^ name.hashCode ^ id.hashCode

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
}

// Companion object for arithmetic.Var class
object Var {
  private val cnt = new AtomicLong(-1) // Instance counter

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
  def apply(name: String): Var = new Var(name, Interval(?, Cst(-1)))
}

// Class for sums
case class Sum(terms: List[ArithExpr]) extends ArithExpr {

  override val HashSeed = 0x8e535130

  override lazy val digest: Int = terms.foldRight(HashSeed)((x, hash) => hash ^ x.digest())

  lazy val asProd : Option[Prod] = {
    val factorisation = Factorise(this)
    if (factorisation.isDefined) factorisation.get.toProd
    else None
  }

  override def equals(that: Any): Boolean = that match {
    case Sum(terms2) => terms.length == terms2.length && terms.intersect(terms2).length == terms.length
    case _ => false
  }

  override def toString: String = s"(${terms.mkString(" + ")})"
}

object Sum {
  // This is an empty extractor to support SimplifySum that tries to be safe when we represent
  // other classes as sums
  def unapply(ae: Any): Option[List[ArithExpr]] = ae match {
    case aexpr: ArithExpr => aexpr match {
      case s: Sum => Some(s.terms)
      case pw : Pow => if (pw.asSum.isDefined) Some(pw.asSum.get.terms) else None
      case p:Prod =>
        if (p.partialCancelledSum.isDefined) Some(p.partialCancelledSum.get.getTermsFactors )
        else if (p.distributed.isDefined) Some(p.distributed.get.terms)
        else None
      case _ => None
    }
    case _ => None
  }
}

// Class for products
case class Prod(factors: List[ArithExpr]) extends ArithExpr {

  override val HashSeed = 0x286be17e

  override lazy val digest: Int = factors.foldRight(HashSeed)((x, hash) => hash ^ x.digest())

  lazy val cstFactor : Int = factors.head match {
    case Cst(c) => c
    case _ => 1
  }

  lazy val nonCstList : List[ArithExpr] = {
    factors.filter(!_.isInstanceOf[Cst])
  }

  lazy val nonCstFactor : ArithExpr = {
    if (cstFactor == 1) this
    else this.nonCstList.reduce((x, y) => x*y)
  }

  lazy val asNonCstFactorsSum : Option[Sum] = if (factors.length < 2 || cstFactor < 2) None else {
    val terms = ListBuffer[ArithExpr]()
    for (_ <- 0 until cstFactor) {
      terms += nonCstFactor
    }
    Some(Sum(terms.toList))
  }

  // Expands product out
  lazy val distributed : Option[Sum] = {
    if (factors.length <= 1) None
    else {
      var expanded = false
      var accum : ArithExpr = Cst(1)
      for (f <- factors) f match {
        case x @ (_:Cst | _:Var) =>
          if (accum.isInstanceOf[Sum]) {
            accum = ArithExpr.expand(accum, x).get
            expanded = true
          }
          else accum = x * accum
        case s:Sum =>
          accum = ArithExpr.expand(accum,s).get
          expanded = true
        case p:Pow =>
          val pSum = p.asSum
          if (pSum.isDefined) {
            accum = ArithExpr.expand(accum,pSum.get).get
            expanded = true
          }
          else if (accum.isInstanceOf[Sum]) {
            accum = ArithExpr.expand(accum,p).get
            expanded = true
          }
          else {
            if (p.e > 0) accum = accum * p
            else accum /^= p.b
          }
        case _ =>
          if (accum.isInstanceOf[Sum]) {
            accum = ArithExpr.expand(accum, f).get
          }
          else accum *= f
      }
      if (expanded) accum.getTermsFactors.reduce((x, y)=>x+y).toSum
      else None
    }
  }

  // Works for case when we have product of sum and negative power expression
  // Tries to find subset of sum that can be cancelled out with the power
  lazy val partialCancelledSum : Option[ArithExpr] = {
    if (this.asSumFraction.isDefined) {
      val numer = this.asSumFraction.get._1
      val denom = this.asSumFraction.get._2
      val termsExpanded = Helper.expandTermsCst(numer.terms)
      val termSubsets = Helper.powerSet(termsExpanded).filter(_.nonEmpty)
      if (termSubsets.nonEmpty) {
        var partialCancel : Option[ArithExpr] = None
        var i = 0
        while (i < termSubsets.length) {
          val subset = termSubsets(i)
          val restTerms = termsExpanded.diff(subset)
          val sum = subset.reduce(_ + _)
          val rest = if (restTerms.isEmpty) Cst(0) else restTerms.reduce(_ + _)
          val gcd = ComputeGCD(sum, denom)
          if (gcd != Cst(1)) {
            partialCancel = Some(sum /^ denom + rest /^ denom)
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

  lazy val primitiveProd : Prod = {
    var primitiveFactors = ListBuffer[ArithExpr]()
    for (f <- factors) f match {
      case p:Pow =>
        if (p.asProdPows.isDefined) {
          val pProdPows = p.asProdPows.get
          primitiveFactors = primitiveFactors ++ pProdPows.primitiveProd.factors
        }
        else {
          val pProd = p.asProd.get
          primitiveFactors = primitiveFactors ++ pProd.factors
        }

      case _ => primitiveFactors += f
    }
    Prod(primitiveFactors.toList)
  }

  override def equals(that: Any): Boolean = that match {
    case Prod(factors2) => factors.length == factors2.length && factors.intersect(factors2).length == factors.length
    case _ => false
  }

  override def toString: String = factors.mkString(" * ")
}

object Prod {
  def unapply(ae: Any): Option[List[ArithExpr]] = ae match {
    case aexpr: ArithExpr => aexpr match {
      case p: Prod => Some(p.factors)
      // Concrete Pow that can be represented as Prod
      case p: Pow if p.asProdPows.isDefined => Some(p.asProdPows.get.factors)

      // Factorisation
      case s: Sum if s.asProd.isDefined => Some(s.asProd.get.factors)
      case _ => None
    }
    case _ => None
  }
}

// Class for powers, integer exponents
case class Pow(b: ArithExpr, e: Int) extends ArithExpr {

  override val HashSeed = 0x63fcd7c2

  override lazy val digest: Int = HashSeed ^ b.digest() ^ e

  // Expansion into sum
  lazy val asSum : Option[Sum] = if (e < 0 || !b.isInstanceOf[Sum]) None else {
    var combined : ArithExpr = Cst(0)
    for (n <- 1 to e) {
      if (n == 1)
        combined = b
      else
        combined = ArithExpr.expand(combined,b).get
    }
    Some(Sum(combined.getTermsFactors))
    //combined.toSum
  }


  // Representation as a product (a^n = a*a...*a (n times))
  lazy val asProd : Option[Prod] = {
    //assume(e > 1)
    if (e > 1) Some(Prod(List.fill(e)(b)))
    else if (e < 1) Some(Prod(List.fill(scala.math.abs(e))(Pow(b,-1))))
    // Exponent is 1 or -1
    else None
//    else Some(Prod(List(this)))
  }

  lazy val asProdPows : Option[Prod]= b match {
    case _:Prod =>
      val bfacts = b.getTermsFactors
      val pfacts = bfacts.map(x => x pow e)
      Some(Prod(pfacts))
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
}

object abs {
  def apply(ae: ArithExpr): ArithExpr = SimplifyAbs(ae)
}

case class AbsFunction(ae: ArithExpr) extends ArithExpr {

  override val HashSeed = 0x3570a2ce

  override lazy val digest: Int = HashSeed ^ ae.digest()

  override def toString: String = "Abs(" + ae + ")"
}

case class FloorFunction(ae: ArithExpr) extends ArithExpr {

  override val HashSeed = 0x558052ce

  override lazy val digest: Int = HashSeed ^ ae.digest()

  override def toString: String = "Floor(" + ae + ")"
}

object floor {
  def apply(ae: ArithExpr): ArithExpr = simplifier.SimplifyFloor(ae)
}

case class CeilingFunction(ae: ArithExpr) extends ArithExpr {

  override val HashSeed = 0xa45d23d0

  override lazy val digest: Int = HashSeed ^ ae.digest()

  override def toString: String = "Ceiling(" + ae + ")"
}

object ceil {
  def apply(ae: ArithExpr): ArithExpr = simplifier.SimplifyCeiling(ae)
}


object ArithExpr {

  implicit def intToCst(i: Int): ArithExpr = Cst(i)

  val isCanonicallySorted: (ArithExpr, ArithExpr) => Boolean = (x: ArithExpr, y: ArithExpr) => (x, y) match {
    case (Cst(a), Cst(b)) => a < b
    case (_: Cst, _) => true // constants first
    case (_, _: Cst) => false
    case (x: Var, y: Var) => x.id < y.id // order variables based on id
    case (_: Var, _) => true // variables always after constants second
    case (_, _: Var) => false
    case (p1:Prod, p2:Prod) => p1.factors.zip(p2.factors).map(x => isCanonicallySorted(x._1, x._2)).foldLeft(false)(_ || _)
    case _ => x.HashSeed() < y.HashSeed() || (x.HashSeed() == y.HashSeed() && x.digest() < y.digest())
  }

  // Evaluates an expression given constant substitutions for variables
  def evaluate(expr: ArithExpr, subs : scala.collection.Map[Var, Cst]) : Int = expr match {
    case Cst(c) => c
    case v: Var => findSubstitute(v, subs)
    case s:Sum => s.terms.foldLeft(0) { (accumulated, term) => accumulated + evaluate(term, subs)}
    case p:Prod => p.factors.foldLeft(1) { (accumulated, factor) => accumulated * evaluate(factor, subs)}
    case Pow(b,e) => scala.math.pow(evaluate(b,subs),e).toInt
  }

  private def findSubstitute(variable: Var, replacements : scala.collection.Map[Var, Cst]) : Int = {
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
      for (lt <- lts) {
        for (rt <- rts) {
          combined += lt * rt
        }
      }
      Some(Sum(combined.getTermsFactors))

    case (s: Sum, _) =>
      val lts = s.terms
      var combined: ArithExpr = Cst(0)
      for (lt <- lts) {
        combined += lt * e2
      }
      Some(Sum(combined.getTermsFactors))


    case (_, s: Sum) =>
      val rts = s.terms
      var combined: ArithExpr = Cst(0)
      for (rt <- rts) {
        combined += e1 * rt
      }
      Some(Sum(combined.getTermsFactors))

    case _ => None
  }

  // Check if an expression is a multiple of another expression
  def isMultipleOf(ae1: ArithExpr, ae2: ArithExpr) : Boolean = (ae1, ae2) match {
    // Check multiple of constants
    case (Cst(c1), Cst(c2)) => c1 % c2 == 0
    case (p:Prod, c:Cst) => p.cstFactor % c.value == 0

    case (Pow(b1, _), Pow(b2, _)) => isMultipleOf(b1, b2)
    case (p:Prod, _) => p.factors.contains(ae2) ||
      p.factors.foldLeft(false){(accum,factor) => accum || isMultipleOf(factor,ae2)}
    case (p:Pow, _) => isMultipleOf(p.b,ae2)
    case (x, y) => x == y
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
          }
        }
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

    None
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
          }
        }
      // Keep going
      case _ =>

      // Can't determine yet
      case _ =>
    }

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

    None
  }

  def isSmallerOrEqual(ae1: ArithExpr, ae2: ArithExpr) : Option[Boolean] = {
    val smaller = isSmaller(ae1,ae2)
    if (smaller.isEmpty) {
      if (ae1 != ae2) None
      else Some(true)
    }
    else if (!smaller.get) {
      Some(ae1 == ae2)
    }
    else Some(true)
  }

  def isBiggerOrEqual(ae1: ArithExpr, ae2: ArithExpr) : Option[Boolean] = {
    val bigger = isBigger(ae1,ae2)
    if (bigger.isEmpty) {
      if (ae1 != ae2) None
      else Some(true)
    }
    else if (!bigger.get) {
      Some(ae1 == ae2)
    }
    else Some(true)
  }


  // Minimum of expressions (if possible)
  def minList(aes: List[ArithExpr]) : ArithExpr = {
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
    case Sum(terms) =>
      terms.foreach(t => if (visitUntil(t, f)) return true)
      false
    case Prod(terms) =>
      terms.foreach(t => if (visitUntil(t, f)) return true)
      false
    case FloorFunction(expr) => visitUntil(expr, f)
    case CeilingFunction(expr) => visitUntil(expr, f)
    case _:Var | Cst(_) => false
    case x if x.getClass == ?.getClass => false
  }

  private def evalDouble(e: ArithExpr): Double = e match {
    case Cst(c) => c

    case Pow(base, exp) => scala.math.pow(evalDouble(base),exp)

    case s:Sum => s.terms.foldLeft(0.0)((result, expr) => result + evalDouble(expr))
    case p:Prod => p.factors.foldLeft(1.0)((result, expr) => result * evalDouble(expr))

    case FloorFunction(expr) => scala.math.floor(evalDouble(expr))
    case CeilingFunction(expr) => scala.math.ceil(evalDouble(expr))

    case `?` | _:Var => throw NotEvaluable
  }

  private def computeIntervalProd(factors: List[ArithExpr]) : Interval = {
    val minMax = factors.map(x => Interval(x.min, x.max))
    minMax.reduce((x,y) => x*y)
  }
}

case object ? extends ArithExpr {
  override val HashSeed = 0x3fac31

  override val digest: Int = HashSeed
}