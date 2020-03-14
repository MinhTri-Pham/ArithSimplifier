package arithmetic

import java.io.{File, PrintWriter}

import arithmetic.simplifier.ExprSimplifier

import scala.collection.mutable.ListBuffer
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.postfixOps

object FactoriseEvaluator {
  // Possible variables
  val av: Var = Var("a")
  val bv: Var = Var("b")
  val cv: Var = Var("c")
  val dv: Var = Var("d")
  val ev: Var = Var("e")
  val fv: Var = Var("f")
  val gv: Var = Var("g")
  val hv: Var = Var("h")
  val iv: Var = Var("i")
  val kv: Var = Var("k")

  val variables: Seq[Var] = List[Var](av,bv,cv,dv,ev,fv,gv,hv,iv,kv)
  val numPossibleVars: Int = variables.length

  val maxSumFactorLen = 3 // Max number of terms/factors in sum/product
  val minSumFactorLen = 2 // Min number of terms/factors in sum/product

  val maxPowExp = 2 // Max number of terms/factors in sum/product
  val minPowExp = 2 // Min number of terms/factors in sum/product

  val maxNumFactors = 3 // Max number of terms/factors in sum/product
  val minNumFactors = 2 // Min number of terms/factors in sum/product

  val maxPrimPowLen = 2 // Max number of terms/factors in sum/product
  val minPrimPowLen = 2 // Min number of terms/factors in sum/product

  val minCst = 2 // Bounds for single constant leaf
  val maxCst = 3

  val rGen = new scala.util.Random // Random generator

  var numVarsInExpr = 0
  var varsInExpr = new ListBuffer[ArithExpr]()
  var numTimedOut = 0

  def genCst() : Cst = {
    Cst(minCst + rGen.nextInt((maxCst - minCst) + 1))
  }

  def genVar() : Var = {
    val v = variables(rGen.nextInt(numPossibleVars))
    if (!varsInExpr.contains(v)) {
      numVarsInExpr += 1
      varsInExpr += v
    }
    v
  }

  def genSum() : ArithExpr = {
    var isSum = false
    val terms = new ListBuffer[ArithExpr]()
    var expr : ArithExpr = null
    do {
      terms.clear()
      val numTerms = minNumFactors + rGen.nextInt((maxNumFactors - minNumFactors) + 1)
      val hasCstTerm = rGen.nextBoolean()
      if (hasCstTerm) {
        terms += genCst()
        for (_ <- 1 until numTerms) {
          val isVar = rGen.nextInt(3)
          if (isVar == 0) terms += genVar()
          else if (isVar == 1) terms += genPrimProd()
          else terms += genPow()
        }
        expr = ExprSimplifier(Sum(terms.toList))
        isSum = expr.isInstanceOf[Sum]
      }
      else {
        for (_ <- 0 until numTerms) {
          val isVar = rGen.nextInt(3)
          if (isVar == 0) terms += genVar()
          else if (isVar == 1) terms += genPrimProd()
          else terms += genPow()
        }
        expr = ExprSimplifier(Sum(terms.toList))
        isSum = expr.isInstanceOf[Sum]
      }
    } while(!isSum)
    expr
  }

  def genProd() : ArithExpr  = {
    val numFactors = minNumFactors + rGen.nextInt((maxNumFactors - minNumFactors) + 1)
    val factors = new ListBuffer[ArithExpr]()
    for (_ <- 0 until numFactors) {
      factors += genSum()
    }
    factors.reduce(_*_)
  }

  def genPrimProd() : ArithExpr = {
    val factors = new ListBuffer[ArithExpr]()
    val numFactors = minPrimPowLen + rGen.nextInt((maxPrimPowLen - minPrimPowLen) + 1)
    val hasCstFactor = rGen.nextBoolean()
    if (hasCstFactor) {
      factors += genCst()
      for (_ <- 1 until numFactors) {
//        val isVar = rGen.nextBoolean()
//        if (isVar) factors += genVar()
//        else factors += genPow()
        factors += genVar()
      }
      factors.reduce(_ * _)
    }
    else {
      for (_ <- 0 until numFactors) {
//        val isVar = rGen.nextBoolean()
//        if (isVar) factors += genVar()
//        else factors += genPow()
        factors += genVar()
      }
      factors.reduce(_ * _)
    }
  }


  def genPow() : Pow  = {
    val base = genVar()
    val exp = minPowExp + rGen.nextInt( (maxPowExp - minPowExp) + 1 )
    Pow(base, exp)
  }

  def computeLen(s:Sum) : Int = {
    var len = 0
    for (term <- s.terms) {
      term match {
        case c : Cst => len += c.value.toInt
        case p:Prod => len += p.cstFactor.toInt
        case _ => len += 1
      }
    }
    len
  }

  // Function to run a block with a time out limit
  def runWithTimeout[T](timeoutMs: Long)(f: => T) : T = {
    Await.result(Future(f), timeoutMs milliseconds)
  }

  // Evaluating product simplification with at least one sum factor
  def evalFactoriseComparsion(txtw : PrintWriter, csvw: PrintWriter) : Boolean = {

    try {
      runWithTimeout(20000) {
        val randomProd = genProd()
        val randomProdAsSum = randomProd.toSum.get
        val len = computeLen(randomProdAsSum)
        txtw.write(s"Generated prod: $randomProd\n")
        txtw.write(s"Expanded form: $randomProdAsSum\n")
        val t1 = System.nanoTime
        val factorisation = Factorise(randomProdAsSum)
        val duration = (System.nanoTime - t1) / 1e6d // Runtime in ms
        val durRounded = f"$duration%.3f"
        if (factorisation.isDefined) {
          val isEq = factorisation.get == randomProd
          if (isEq) {
            csvw.write(s"$numVarsInExpr, $len, $durRounded")
          }
          else {
            txtw.write("Factorisation and original product not same!")
            csvw.write(s"$numVarsInExpr, $len, fail")
          }
          txtw.write(s"\n")
          return isEq
        }
        else {
          txtw.write(s"Couldn't factorise! \n\n")
          return false
        }
      }
    }
    catch {
      case _:TimeoutException =>
        csvw.write("20000\n")
        txtw.write("Time out problem\n\n")
        numTimedOut += 1
        false
      case _:OutOfMemoryError | _:StackOverflowError =>
        csvw.write("20000\n")
        txtw.write(s"Factorisation too long problem \n\n")
        numTimedOut += 1
        false
    }
  }

  def evaluate() : Unit = {
    val evalExprFile = new File(s"evalFactorise.txt")
    val evalRuntimeFile = new File(s"evalFactorise.csv")
    val txtWriter = new PrintWriter(evalExprFile)
    val csvWriter = new PrintWriter(evalRuntimeFile)
    val header = "Number of variables, Length of sum. Runtime\n"
    csvWriter.write(header)

    var numPassed = 0
    numTimedOut = 0
    val numTrials = 10
    for (_ <- 0 until numTrials) {
      val passed = evalFactoriseComparsion(txtWriter, csvWriter)
      if (passed) numPassed += 1
    }
    txtWriter.write(s"Evaluations passed: $numPassed\n")
    txtWriter.write(s"Evaluations timed out: $numTimedOut\n")
    txtWriter.write(s"Evaluations possibly not equal: ${numTrials - numPassed - numTimedOut}")
    txtWriter.close()
    csvWriter.close()
  }
}
