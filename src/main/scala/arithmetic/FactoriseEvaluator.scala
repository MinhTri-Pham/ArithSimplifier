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
  val ov: Var = Var("o")
  val pv: Var = Var("p")
  val rv: Var = Var("r")
  val sv: Var = Var("s")

  val variables: Seq[Var] = List[Var](av,bv,cv,dv,ev,fv,gv,hv,iv,jv,kv,mv,nv,ov,pv)
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
    var isAcceptable = false
    val terms = new ListBuffer[ArithExpr]()
    var expr : ArithExpr = null
    val numTerms = minSumFactorLen + rGen.nextInt((maxSumFactorLen - minSumFactorLen) + 1)
    do {
      terms.clear()
      for (_ <- 0 until numTerms) {
        val isVar = rGen.nextInt(3)
        if (isVar == 0) terms += genVar()
        else if (isVar == 1) terms += genPrimProd()
        else terms += genPow()
      }
      expr = terms.reduce(_ + _)
      isAcceptable = isAcceptableSum(expr)
    } while(!isAcceptable)
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
    for (_ <- 0 until numFactors) {
      val isVar = rGen.nextBoolean()
      if (isVar) factors += genVar()
      else factors += genPow()
    }
    factors.reduce(_ * _)
  }


  def genPow() : Pow  = {
    val base = genVar()
    val exp = minPowExp + rGen.nextInt( (maxPowExp - minPowExp) + 1 )
    Pow(base, exp)
  }

  def isAcceptableSum(ae : ArithExpr): Boolean = {
    ae match {
      case s:Sum =>
        for (term <- s.terms) {
          term match {
            case _ : Cst => return false
            case p:Prod if p.cstFactor != 1 => return false
            case _ =>
          }
        }
        true
      case _ => false
    }
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

  def evalFactoriseComparsion(txtw : PrintWriter, csvw: PrintWriter) : Boolean = {
    var len = 0
    numVarsInExpr = 0
    varsInExpr.clear()
    try {
      runWithTimeout(8000) {
        val randomProd = genProd()
        val randomProdAsSum = randomProd.toSum.get
        len = computeLen(randomProdAsSum)
        txtw.write(s"Generated prod: $randomProd\n")
        txtw.write(s"Expanded form: $randomProdAsSum\n")
        val t1 = System.nanoTime
        val factorisation = Factorise(randomProdAsSum)
        val duration = (System.nanoTime - t1) / 1e6d // Runtime in ms
        val durRounded = f"$duration%.3f"
        if (factorisation.isDefined) {
          txtw.write(s"Factorisation of expanded form: ${factorisation.get}\n")
          val isEq = factorisation.get == randomProd
          if (isEq) csvw.write(s"$numVarsInExpr, $len, $durRounded\n")
          else {
            txtw.write("Factorisation and original product not same!\n")
            csvw.write(s"$numVarsInExpr, $len, fail")
          }
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
        csvw.write(s"$numVarsInExpr, $len, 8000\n")
        txtw.write("Time out problem\n\n")
        numTimedOut += 1
        false
      case _:OutOfMemoryError | _:StackOverflowError =>
        csvw.write(s"$numVarsInExpr, $len, 8000\n")
        txtw.write(s"Factorisation too long problem \n\n")
        numTimedOut += 1
        false
    }
  }

  // Test only
  private def evalFactoriseComparsionPrint() : Boolean = {
    var len = 0
    numVarsInExpr = 0
    varsInExpr.clear()
    try {
      runWithTimeout(8000) {
        val randomProd = genProd()
        val randomProdAsSum = randomProd.toSum.get
        len = computeLen(randomProdAsSum)
        println(s"Generated prod: $randomProd")
        println(s"Expanded form: $randomProdAsSum")
        val t1 = System.nanoTime
        val factorisation = Factorise(randomProdAsSum)
        val duration = (System.nanoTime - t1) / 1e6d // Runtime in ms
        val durRounded = f"$duration%.3f"
        if (factorisation.isDefined) {
          println(s"Factorisation of expanded form: ${factorisation.get}")
          val isEq = factorisation.get == randomProd
          if (isEq) println(s"$numVarsInExpr, $len, $durRounded")
          else println("Factorisation and original product not same!")
          println()
          isEq
        }
        else {
          println(s"Couldn't factorise!\n")
          false
        }
      }
    }
    catch {
      case _:TimeoutException =>
        println("Time out problem\n")
        false
      case _:OutOfMemoryError | _:StackOverflowError =>
        println(s"Memory issue with factorisation\n")
        false
    }
  }

  def evaluate() : Unit = {
    val evalExprFile = new File(s"evalFactorise.txt")
    val evalRuntimeFile = new File(s"evalFactorise.csv")
    val txtWriter = new PrintWriter(evalExprFile)
    val csvWriter = new PrintWriter(evalRuntimeFile)
    val header = "Number of variables, Length of sum, Runtime\n"
    csvWriter.write(header)

    var numPassed = 0
    numTimedOut = 0
    val numTrials = 10
    for (i <- 0 until numTrials) {
      println(i)
      val passed = evalFactoriseComparsion(txtWriter, csvWriter)
      if (passed) numPassed += 1
    }
    txtWriter.write(s"Evaluations passed: $numPassed\n")
    txtWriter.write(s"Evaluations timed out: $numTimedOut\n")
    txtWriter.write(s"Evaluations possibly not equal: ${numTrials - numPassed - numTimedOut}")
    txtWriter.close()
    csvWriter.close()
  }

  def main(args: Array[String]): Unit = {
//    evaluate()
    
  }
}
