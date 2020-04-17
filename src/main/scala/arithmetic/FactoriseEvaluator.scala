package arithmetic

import java.io.{File, PrintWriter}

import scala.collection.mutable.ListBuffer
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.postfixOps

// For evaluating performance of factorisation

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

  val maxSumFactorLen = 3 // Max number of terms in each factor
  val minSumFactorLen = 3 // Min number of terms in each factor

  val maxNumFactors = 3 // Max number of factors in original product
  val minNumFactors = 2 // Min number of factors in original product

  val rGen = new scala.util.Random(950) // Random generator

  var numTimedOut = 0

  def genVar() : Var = variables(rGen.nextInt(numPossibleVars))

  def genSum() : ArithExpr = Sum(genSumTerms())

  // Generates terms for each factor of generated product
  def genSumTerms() : List[ArithExpr] = {
    val terms = new ListBuffer[ArithExpr]()
    val numTerms = minSumFactorLen + rGen.nextInt((maxSumFactorLen - minSumFactorLen) + 1)
    val vars = new ListBuffer[Var]()
    for (_ <- 0 until numTerms) {
      // Choose variable at random, making sure that they are always different
      var v = genVar()
      while (vars.contains(v)) v = genVar()
      vars += v
      terms += v
    }
    terms.toList
  }

  // Generate random factorisation
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
    val randomProd = genProd()
    val randomProdAsSum = randomProd.toSum.get
    txtw.write(s"Generated prod: $randomProd\n")
    txtw.write(s"Expanded form: $randomProdAsSum\n")
    var factorisation : Option[ArithExpr] = None
    var successFirstTry = false
    var success = false
    var dur = 0.0
    try {
      runWithTimeout(timeout) {
        val t1 = System.nanoTime
        factorisation = Factorise(randomProdAsSum)
        val dur1 = (System.nanoTime - t1) / 1e6d // Runtime in ms
        dur += dur1
        successFirstTry = true
      }
    }
    catch {
      case _:TimeoutException =>
        txtw.write(s"${Factorise.numFactorsTried}, $timeout\n")
        txtw.write("Time out problem\n\n")
        numTimedOut += 1
        return false
      case _:OutOfMemoryError | _:StackOverflowError =>
        txtw.write(s"${Factorise.numFactorsTried}, $timeout\n")
        txtw.write(s"Factorisation memory issue\n\n")
        numTimedOut += 1
        return false
    }
    if (successFirstTry) {
      try {
        runWithTimeout(timeout * 2) {
          val t1 = System.nanoTime
          factorisation = Factorise(randomProdAsSum)
          factorisation = Factorise(randomProdAsSum)
          val durRest = (System.nanoTime - t1) / 1e6d // Runtime in ms
          dur += durRest
          dur /= 3
          val durRounded = f"$dur%.3f"
          if (factorisation.isDefined) {
            val numFactors = Factorise.numFactorsTried
            txtw.write(s"Factorisation of expanded form: ${factorisation.get}\n")
            success = factorisation.get == randomProd
            if (success) {
              csvw.write(s"$numFactors, $durRounded\n")
              txtw.write(s"$numFactors, $durRounded\n")
            }
            else txtw.write("Factorisation and original product not same!\n")
            txtw.write(s"\n")
            }
          else {
            txtw.write(s"Couldn't factorise!\n\n")
            success = false
          }

        }
      }
      catch {
        case _:TimeoutException =>
          txtw.write("Time out problem\n\n")
          numTimedOut += 1
          return false
        case _:OutOfMemoryError | _:StackOverflowError =>
          txtw.write(s"Factorisation memory issue\n\n")
          numTimedOut += 1
          return false
      }
    }
    success
  }

  def evaluate(id: Int) : Unit = {
    val evalExprFile = new File(s"evalFactorise$id.txt")
    val evalRuntimeFile = new File(s"evalFactorise$id.csv")
    val txtWriter = new PrintWriter(evalExprFile)
    val csvWriter = new PrintWriter(evalRuntimeFile)
    val header = "Number of factors tried, Runtime\n"
    csvWriter.write(header)

    txtWriter.write(s"Number of factor bounds: $minNumFactors and $maxNumFactors\n")
    txtWriter.write(s"Factor length bounds: $minSumFactorLen and $maxSumFactorLen\n\n")

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
}
