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

  val variables: Seq[Var] = List[Var](av,bv,cv,dv,ev,fv,gv,hv,iv,jv,kv,mv,nv,ov,pv,rv,sv)
  val numPossibleVars: Int = variables.length

  val maxSumFactorLen = 4 // Max number of terms in sum factor
  val minSumFactorLen = 2 // Min number of terms in sum factor

  val maxPowExp = 2 // Max exponent of power
  val minPowExp = 2 // Min exponent of power

  val maxNumFactors = 4 // Max number of factors in original product
  val minNumFactors = 2 // Min number of factors in original product

  val maxPrimPowLen = 3 // Max number of factors in product term
  val minPrimPowLen = 2 // Min number of factors in product term

  val rGen = new scala.util.Random() // Random generator

  var numVarsInExpr = 0
  var varsInExpr = new ListBuffer[ArithExpr]()

  var numTimedOut = 0

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
    for (i <- 0 until numFactors) {
      if (i == 0) factors += genSum()
      else {
        val isSum = rGen.nextBoolean()
        if (isSum) factors += genSum()
        else factors += genVar()
      }
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

//  def computeLen(s:Sum) : Int = {
//    var len = 0
//    for (term <- s.terms) {
//      term match {
//        case c : Cst => len += c.value.toInt
//        case p:Prod => len += p.cstFactor.toInt
//        case _ => len += 1
//      }
//    }
//    len
//  }

  // Function to run a block with a time out limit
  def runWithTimeout[T](timeoutMs: Long)(f: => T) : T = {
    Await.result(Future(f), timeoutMs milliseconds)
  }

  def evalFactoriseComparsion(txtw : PrintWriter, csvw: PrintWriter) : Boolean = {
    var len = 0
    numVarsInExpr = 0
    varsInExpr.clear()
    try {
      runWithTimeout(6000) {
        val randomProd = genProd()
        val randomProdAsSum = randomProd.toSum.get
//        len = computeLen(randomProdAsSum)
        len = randomProdAsSum.terms.length
        txtw.write(s"Generated prod: $randomProd\n")
        txtw.write(s"Expanded form: $randomProdAsSum\n")
        val t1 = System.nanoTime
        val factorisation = Factorise(randomProdAsSum)
        val duration = (System.nanoTime - t1) / 1e6d // Runtime in ms
        val durRounded = f"$duration%.3f"
        if (factorisation.isDefined) {
          txtw.write(s"Factorisation of expanded form: ${factorisation.get}\n")
          val isEq = factorisation.get == randomProd
          if (isEq) {
            csvw.write(s"$numVarsInExpr, $len, $durRounded\n")
            txtw.write(s"Runtime of factorisation: $durRounded\n")
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
        txtw.write("Time out problem\n\n")
        numTimedOut += 1
        false
      case _:OutOfMemoryError | _:StackOverflowError =>
        txtw.write(s"Factorisation too long problem\n\n")
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
      runWithTimeout(6000) {
        val randomProd = genProd()
        val randomProdAsSum = randomProd.toSum.get
//        len = computeLen(randomProdAsSum)
        len = randomProdAsSum.terms.length
        println(s"Generated prod: $randomProd")
        println(s"Expanded form: $randomProdAsSum")
        val t1 = System.nanoTime
        val factorisation = Factorise(randomProdAsSum)
        Factorise(randomProdAsSum)
        Factorise(randomProdAsSum)
        val duration = (System.nanoTime - t1) / (1e6d  * 3) // Average runtime in ms
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

  def evaluate(id:Int) : Unit = {
    val evalExprFile = new File(s"evalFactorise$id.txt")
    val evalRuntimeFile = new File(s"evalFactorise$id.csv")
    val txtWriter = new PrintWriter(evalExprFile)
    val csvWriter = new PrintWriter(evalRuntimeFile)
    val header = "Number of variables, Length of sum, Runtime\n"
    csvWriter.write(header)

    var numPassed = 0
    numTimedOut = 0
    val offset = 10 // These first runs won't count
    val numTrialsRaw = 100
    val numTrials = numTrialsRaw + offset
    for (i <- 0 until numTrials) {
      println(i)
      val passed = evalFactoriseComparsion(txtWriter, csvWriter)
      if (passed) numPassed += 1
    }
    txtWriter.write(s"Passed: $numPassed\n")
    txtWriter.write(s"Timed out: $numTimedOut\n")
    txtWriter.write(s"Possibly failed: ${numTrials - numPassed - numTimedOut}")
    txtWriter.close()
    csvWriter.close()
  }

  def evaluatePrint(): Unit = {
    val offset = 10 // These first runs won't count
    val numTrialsRaw = 50
    val numTrials = numTrialsRaw + offset
    for (i <- 0 until numTrials) {
      println(i)
      evalFactoriseComparsionPrint()
    }
  }

  def main(args: Array[String]): Unit = {
//    evaluate(0)
    evaluatePrint()
  }
}
