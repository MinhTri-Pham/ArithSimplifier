package arithmetic

import java.io.{File, PrintWriter}

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
  val jv: Var = Var("j")
  val kv: Var = Var("k")
  val lv: Var = Var("l")
  val mv: Var = Var("m")
  val nv: Var = Var("n")

  val variables: Seq[Var] = List[Var](av,bv,cv,dv,ev,fv,gv,hv,iv,jv,kv,lv,mv,nv)
  val numPossibleVars: Int = variables.length

  val maxSumFactorLen = 3 // Max number of terms in sum factor
  val minSumFactorLen = 3 // Min number of terms in sum factor

  val maxNumFactors = 3 // Max number of factors in original product
  val minNumFactors = 2 // Min number of factors in original product

  val rGen = new scala.util.Random() // Random generator

  var numTimedOut = 0

  def genVar() : Var = variables(rGen.nextInt(numPossibleVars))

  def genSum() : ArithExpr = {
    var terms = genSumTerms()
    while(ComputeGCD.commonTermList(terms) != Cst(1)) terms = genSumTerms()
    Sum(terms)
  }

  def genSumTerms() : List[ArithExpr] = {
    val terms = new ListBuffer[ArithExpr]()
    val numTerms = minSumFactorLen + rGen.nextInt((maxSumFactorLen - minSumFactorLen) + 1)
    val vars = new ListBuffer[Var]()
    for (_ <- 0 until numTerms) {
      var v = genVar()
      while (vars.contains(v)) v = genVar()
      vars += v
      terms += v
    }
    terms.toList
  }

  def genProd() : ArithExpr  = {
    val numFactors = minNumFactors + rGen.nextInt((maxNumFactors - minNumFactors) + 1)
    val factors = new ListBuffer[ArithExpr]()
    for (_ <- 0 until numFactors) {
      factors += genSum()
    }
    factors.reduce(_*_)
  }

  // Function to run a block with a time out limit
  def runWithTimeout[T](timeoutMs: Long)(f: => T) : T = {
    Await.result(Future(f), timeoutMs milliseconds)
  }

  def evalFactoriseComparison(txtw : PrintWriter, csvw: PrintWriter) : Boolean = {
    val timeout = 50000
    try {
      runWithTimeout(timeout) {
        val randomProd = genProd()
        val randomProdAsSum = randomProd.toSum.get
        txtw.write(s"Generated prod: $randomProd\n")
        txtw.write(s"Expanded form: $randomProdAsSum\n")
        val t1 = System.nanoTime
        val factorisation = Factorise(randomProdAsSum)
        val duration = (System.nanoTime - t1) / 1e6d // Runtime in ms
        val durRounded = f"$duration%.3f"
        if (factorisation.isDefined) {
          val numFactors = Factorise.numFactorsTried
          txtw.write(s"Factorisation of expanded form: ${factorisation.get}\n")
          val isEq = factorisation.get == randomProd
          if (isEq) {
            csvw.write(s"$numFactors, $durRounded\n")
            txtw.write(s"$numFactors, $durRounded\n")
          }
          else txtw.write("Factorisation and original product not same!\n")
          txtw.write(s"\n")
          isEq
        }
        else {
          txtw.write(s"Couldn't factorise!\n\n")
          false
        }
      }
    }
    catch {
      case _:TimeoutException =>
        txtw.write(s"${Factorise.numFactorsTried}, $timeout\n")
        txtw.write("Time out problem\n\n")
        numTimedOut += 1
        false
      case _:OutOfMemoryError | _:StackOverflowError =>
        txtw.write(s"${Factorise.numFactorsTried}, $timeout\n")
        txtw.write(s"Factorisation memory issue\n\n")
        numTimedOut += 1
        false
    }
  }

  def evaluate(id: Int) : Unit = {
    val evalExprFile = new File(s"evalFactorise$id.txt")
    val evalRuntimeFile = new File(s"evalFactorise$id.csv")
    val txtWriter = new PrintWriter(evalExprFile)
    val csvWriter = new PrintWriter(evalRuntimeFile)
    val header = "Number of backtrackings, Number of factors tried, Number of subsets tried," +
      " Number of times with common gcd, Runtime\n"
    csvWriter.write(header)

    var numPassed = 0
    numTimedOut = 0
    val offset = 10 // These first runs won't count
    val numTrialsRaw = 250
    val numTrials = numTrialsRaw + offset
    for (i <- 0 until numTrials) {
      Factorise.numFactorsTried = 0
      println(i)
      val passed = evalFactoriseComparison(txtWriter, csvWriter)
      if (passed) numPassed += 1
    }
    txtWriter.write(s"Passed: $numPassed\n")
    txtWriter.write(s"Timed out: $numTimedOut\n")
    txtWriter.write(s"Possibly failed: ${numTrials - numPassed - numTimedOut}")
    txtWriter.close()
    csvWriter.close()
  }

  def main(args: Array[String]): Unit = {
    evaluate(0)
  }
}
