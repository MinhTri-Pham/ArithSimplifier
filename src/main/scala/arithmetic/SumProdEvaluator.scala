package arithmetic

import arithmetic.simplifier._

import java.io._
import java.util.concurrent.TimeoutException

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import scala.language.postfixOps

// Object to perform evaluation of simplification
object SumProdEvaluator {
  val maxSize = 5 // Max number of terms/factors in sum/product
  val minSize = 2 // Min number of terms/factors in sum/product

  val maxDepth = 3 // Maximum depth of arithmetic expression tree
  val maxExp = 5 // Max exponent of a power
  val minExp = 2 // Min exponent of a power
  val cstSingleMin: Int = -6 // Bounds for single constant leaf
  val cstSingleMax = 6
  val maxCst = 64
  val minCst: Int = -64 // Bounds for constant node (result of multiple leafs combined together)

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

  val variables: Seq[Var] = List[Var](av,bv,cv,dv,ev,fv,gv,hv,iv,jv,kv,mv,nv)
  val numPossibleVars: Int = variables.length
  val valMap = new mutable.HashMap[ArithExpr, ArithExpr]()

  // Configuration
  val offset = 10 // These first runs won't count
  val numTrialsRaw = 750
  val numTrials: Int = numTrialsRaw + offset
  var numTimedOut = 0

  var numTermsFactors = 0 // How many terms/factors at top level for sum/product

//  var numTotalTerms = 0 // How many terns in expanded form
  // How many sum factors have to be multiplied together - i.e how many factorisations have to be done
  var numSumFactors = 0

  val rGen = new scala.util.Random() // Random generator

  def genCst() : Cst = {
   Cst(cstSingleMin + rGen.nextInt(cstSingleMax - cstSingleMin + 1))
  }

  def genVar() : Var = {
    variables(rGen.nextInt(numPossibleVars))
  }

  // Generate random terminal node (leaf) of arithmetic expression tree
  // Constant or variable
  def genLeaf() : ArithExpr = {
    val isVar = rGen.nextBoolean()
    if (isVar) genVar() else genCst()
  }

  // Generate sum at a certain level of the expression tree
  def genSum(level: Int) : ArithExpr = {
    val numTerms = minSize + rGen.nextInt(maxSize - minSize + 1)
    if (level == 1) numTermsFactors = numTerms
    if (level == maxDepth) {
      val terms = List.fill(numTerms)(genLeaf())
      updateCstFactor(ExprSimplifier(Sum(terms)))
    }
    else {
      val terms = new ListBuffer[ArithExpr]()
      for (_ <- 0 until numTerms) {
        val chooseOpt = rGen.nextInt(3)
        chooseOpt match {
          case 0 =>
            terms += genLeaf()
          case 1 =>
            terms += updateCstFactor(ExprSimplifier(genProd(level + 1)))
          case 2 =>
            terms += updateCstFactor(ExprSimplifier(genPow(level + 1)))
        }
      }
      Sum(terms.toList)
    }
  }

  // Generate product at a certain level of the expression tree
  def genProd(level: Int) : ArithExpr = {
    val numFactors = minSize + rGen.nextInt(maxSize - minSize + 1)
    if (level == 1) numTermsFactors = numFactors
    if (level == maxDepth) {
      val factors = List.fill(numFactors)(genLeaf())
      updateCstFactor(ExprSimplifier(Prod(factors)))
    }
    else {
      val factors = new ListBuffer[ArithExpr]()
      for (_ <- 1 to numFactors) {
        val chooseOpt = rGen.nextInt(3)
        chooseOpt match {
          case 0 =>
            factors += genLeaf()
          case 1  =>
            factors += updateCstFactor(ExprSimplifier(genSum(level + 1)))
          case 2 =>
            factors += updateCstFactor(ExprSimplifier(genPow(level + 1)))
        }
      }
      Prod(factors.toList)
    }
  }

  def genProdWithSumFactor() : ArithExpr = {
    val numFactors = minSize + rGen.nextInt(maxSize - minSize + 1)
    val factors = new ListBuffer[ArithExpr]()
    for (i <- 1 to numFactors) {
      if (i == 1) {
        var firstFactor = ExprSimplifier(genSum(2))
        while (!firstFactor.isInstanceOf[Sum]) {
          firstFactor = ExprSimplifier(genSum(2))
        }
        factors += updateCstFactor(firstFactor)

      }
      else {
        val chooseOpt = rGen.nextInt(3)
        chooseOpt match {
          case 0 =>
            factors += genLeaf()
          case 1  =>
            factors += updateCstFactor(ExprSimplifier(genSum(2)))
          case 2 =>
            factors += updateCstFactor(ExprSimplifier(genPow(2)))
        }
      }
    }
    Prod(factors.toList)
  }

  // Generate power at a certain level of he expression tree
  def genPow(level: Int) : ArithExpr = {
    val exp = minExp + rGen.nextInt(maxExp - minExp + 1) // Positive
//    val exp = -maxPowExp + rGen.nextInt(2*maxPowExp+1) // Positive and negative
    if (level == maxDepth) {
      val base = genLeaf()
      SimplifyPow(base, exp)
    }
    else {
      var base : ArithExpr = null
      val chooseOpt = rGen.nextInt(3)
      chooseOpt match {
        case 0 => base = genLeaf()
        case 1 => base = ExprSimplifier(genSum(level + 1))
        case 2 => base = ExprSimplifier(genProd(level + 1))
      }
      SimplifyPow(base, exp)
    }
  }

  def genPrimProd(level : Int) : ArithExpr = {
    val numFactors = 2 + rGen.nextInt(maxSize - 2 + 1)
    if (level == 1) numTermsFactors = numFactors
    if (level == maxDepth) {
      val factors = List.fill(numFactors)(genLeaf())
      updateCstFactor(ExprSimplifier(Prod(factors)))

    }
    else {
      val factors = new ListBuffer[ArithExpr]()
      for (_ <- 1 to numFactors) {
        val chooseOpt = rGen.nextInt(2)
        chooseOpt match {
          case 0 =>
            factors += genLeaf()
          case 1 =>
            val nextFactor = ExprSimplifier(genPrimPow(level + 1))
            factors += nextFactor
        }
      }
      Prod(factors.toList)
    }
  }

  def genPrimPow(level: Int) : ArithExpr = {
    val exp = 0 + rGen.nextInt(maxExp + 1) // Positive
//    val exp = -maxPowExp + rGen.nextInt(2*maxPowExp+1) // Positive and negative
    if (level == maxDepth) {
      val base = genLeaf()
      updateCstFactor(SimplifyPow(base, exp))
    }
    else {
      val nestFurther = rGen.nextBoolean()
      var base : ArithExpr = null
      if (nestFurther) base = genPrimProd(level + 1)
      else base = genLeaf()
      updateCstFactor(Pow(base, exp))
    }
  }

  def genExpr() : ArithExpr = {
    val isSum = rGen.nextBoolean()
    if (isSum) genSum(1)
    else genProd(1)
  }

  // Function to run a block with a time out limit
  def runWithTimeout[T](timeoutMs: Long)(f: => T) : T = {
    Await.result(Future(f), timeoutMs milliseconds)
  }


  // Evaluating sum simplification
  def evalSumComparison(subs : scala.collection.Map[ArithExpr, ArithExpr], txtw : PrintWriter,
                        csvw: PrintWriter) : Boolean = {
    try {
      runWithTimeout(5000*3) {
        numTermsFactors = 0
//        numTotalTerms = 0
        val randomSum = genSum(level=1)
        txtw.write(s"Generated sum: $randomSum\n")
        val t1 = System.nanoTime
//        val simplifiedSum = ExprSimplifier(randomSum)
        var simplifiedSum = ExprSimplifier(randomSum)
        simplifiedSum = ExprSimplifier(randomSum)
        simplifiedSum = ExprSimplifier(randomSum)
        val duration = (System.nanoTime - t1) / 3e6d // Runtime in ms
        val durRounded = f"$duration%.3f"
        txtw.write(s"Simplified sum: $simplifiedSum\n")
        val randomSumEval = ArithExpr.substitute(randomSum, subs)
        txtw.write(s"Evaluation of gen. sum: $randomSumEval\n")
        val simplifiedSumEval = ArithExpr.substitute(simplifiedSum, subs)
        txtw.write(s"Evaluation of simpl. sum: $simplifiedSumEval\n")
        txtw.write(s"Runtime of simplification: $durRounded\n")
        val isEq = randomSumEval == simplifiedSumEval
        val len = simplifiedSum.getTerms.length
        if (isEq) csvw.write(s"$len,$durRounded\n")
        else txtw.write("Evals don't match, inspect manually!\n")
        txtw.write(s"\n")
        isEq
      }
    }
    catch {
      case _:TimeoutException =>
        txtw.write("Time out problem\n\n")
        numTimedOut += 1
        false
      case _:OutOfMemoryError | _:StackOverflowError =>
        txtw.write(s"Memory issue with factorisation\n\n")
        numTimedOut += 1
        false
    }
  }

  def evalSumTest(id:Int) : Unit = {
    // Seed
    rGen.setSeed(250)
    // Files for logging
    val evalExprFile = new File(s"evalSum$id.txt")
    val evalRuntimeFile = new File(s"evalSum$id.csv")
    val txtWriter = new PrintWriter(evalExprFile)
    val csvWriter = new PrintWriter(evalRuntimeFile)
    val header = "Number of terms,Runtime\n"
    csvWriter.write(header)

    txtWriter.write("Variable mappings\n")
    txtWriter.write(s"$valMap\n\n")

    txtWriter.write("Parameter settings\n")
    txtWriter.write(s"Size bounds: $minSize and $maxSize\n")
    txtWriter.write(s"Exponent bounds: $minExp and $maxExp\n")
    txtWriter.write(s"Constant bounds: $minCst and $maxCst\n\n")

    var numPassed = 0
    numTimedOut = 0
    for (i <- 1 to numTrials) {
      println(i)
      val passed = evalSumComparison(valMap,txtWriter, csvWriter)
      if (passed) numPassed += 1
    }
    txtWriter.write(s"Passed: $numPassed\n")
    txtWriter.write(s"Timed out: $numTimedOut\n")
    txtWriter.write(s"Possibly failed: ${numTrials - numPassed - numTimedOut}")
    txtWriter.close()
    csvWriter.close()
  }

  // Evaluating product simplification without any sum factors
  def evalProdPrimComparison(subs : scala.collection.Map[ArithExpr, ArithExpr], txtw : PrintWriter,
                         csvw: PrintWriter) : Boolean = {
    val randomProd = genPrimProd(level=1)
    txtw.write(s"Generated prod: $randomProd\n")
    val t1 = System.nanoTime
//    val simplifiedProd = ExprSimplifier(randomProd)
    var simplifiedProd = ExprSimplifier(randomProd)
    simplifiedProd = ExprSimplifier(randomProd)
    simplifiedProd = ExprSimplifier(randomProd)
    val duration = (System.nanoTime - t1) / (1e6d * 3)// Average runtime in ms
    val durRounded = f"$duration%.3f"
    txtw.write(s"Simplified prod: $simplifiedProd\n")
    val randomProdEval = ArithExpr.substitute(randomProd, subs)
    txtw.write(s"Evaluation of gen. prod: $randomProdEval\n")
    val simplifiedProdEval = ArithExpr.substitute(simplifiedProd, subs)
    txtw.write(s"Evaluation of simpl. prod: $simplifiedProdEval\n")
    val isEq = randomProdEval == simplifiedProdEval
    if (isEq) {
      csvw.write(s"$durRounded\n")
      txtw.write(s"$durRounded\n")
    }
    else txtw.write("Evals don't match, inspect manually!\n")
    txtw.write(s"\n")
    isEq
  }

  def evalProdPrimTest(id:Int) : Unit = {
    // Seed
    rGen.setSeed(400)
    // File for logging
    val evalExprFile = new File(s"evalProdPrim$id.txt")
    val evalRuntimeFile = new File(s"evalProdPrim$id.csv")
    val txtWriter = new PrintWriter(evalExprFile)
    val csvWriter = new PrintWriter(evalRuntimeFile)

    txtWriter.write("Variable mappings\n")
    txtWriter.write(s"$valMap\n\n")

    txtWriter.write("Parameter settings\n")
    txtWriter.write(s"Size bounds: $minSize and $maxSize\n")
    txtWriter.write(s"Exponent bounds: $minExp and $maxExp\n")
    txtWriter.write(s"Constant bounds: $minCst and $maxCst \n")

    val header = "Runtime\n"
    csvWriter.write(header)

    var numPassed = 0
    numTimedOut = 0
    for (i <- 0 until numTrials) {
      println(i)
      val passed = evalProdPrimComparison(valMap,txtWriter,csvWriter)
      if (passed) numPassed += 1
    }
    txtWriter.write(s"Passed: $numPassed\n")
    txtWriter.write(s"Timed out: $numTimedOut\n")
    txtWriter.write(s"Possibly failed: ${numTrials - numPassed - numTimedOut}")
    txtWriter.close()
    csvWriter.close()
  }

  // Evaluating product simplification with at least one sum factor
  def evalProdWithSumComparison(subs : scala.collection.Map[ArithExpr, ArithExpr], txtw : PrintWriter,
                             csvw: PrintWriter) : Boolean = {
    try {
      runWithTimeout(10000*3) {
        val randomProd = genProdWithSumFactor()
        txtw.write(s"Generated prod: $randomProd\n")
        val t1 = System.nanoTime
//        val simplifiedProd = ExprSimplifier(randomProd)
        var simplifiedProd = ExprSimplifier(randomProd)
        simplifiedProd = ExprSimplifier(randomProd)
        simplifiedProd = ExprSimplifier(randomProd)
        val duration = (System.nanoTime - t1) / (1e6d * 3) // Runtime in ms
        val durRounded = f"$duration%.3f"
        txtw.write(s"Simplified prod: $simplifiedProd\n")
        val randomProdEval = ArithExpr.substitute(randomProd, subs)
        txtw.write(s"Evaluation of gen. prod: $randomProdEval\n")
        val simplifiedProdEval = ArithExpr.substitute(simplifiedProd, subs)
        txtw.write(s"Evaluation of simpl. prod: $simplifiedProdEval\n")
        val isEq = randomProdEval == simplifiedProdEval
        if (isEq) {
          csvw.write(s"$durRounded\n")
          txtw.write(s"$durRounded\n")
        }
        else txtw.write("Evals don't match, inspect manually!\n")
        txtw.write(s"\n")
        isEq
      }
    }
    catch {
      case _:TimeoutException =>
        txtw.write("Time out problem\n\n")
        numTimedOut += 1
        false
      case _:OutOfMemoryError | _:StackOverflowError =>
        txtw.write(s"Memory issue with factorisation\n\n")
        numTimedOut += 1
        false
    }
  }

  def evalProdWithSumTest(id:Int) : Unit = {
    // Seed
    rGen.setSeed(550)
    // File for logging
    val evalExprFile = new File(s"evalProdWithSum$id.txt")
    val evalRuntimeFile = new File(s"evalProdWithSum$id.csv")
    val txtWriter = new PrintWriter(evalExprFile)
    val csvWriter = new PrintWriter(evalRuntimeFile)

    txtWriter.write("Variable mappings\n")
    txtWriter.write(s"$valMap\n\n")

    txtWriter.write("Parameter settings\n")
    txtWriter.write(s"Size bounds: $minSize and $maxSize\n")
    txtWriter.write(s"Exponent bounds: $minExp and $maxExp\n")
    txtWriter.write(s"Constant bounds: $minCst and $maxCst\n")

    val header = "Runtime\n"
    csvWriter.write(header)

    var numPassed = 0
    numTimedOut = 0
    for (i <- 1 to numTrials) {
      println(i)
      val passed = evalProdWithSumComparison(valMap,txtWriter,csvWriter)
      if (passed) numPassed += 1
    }
    txtWriter.write(s"Passed: $numPassed\n")
    txtWriter.write(s"Timed out: $numTimedOut\n")
    txtWriter.write(s"Possibly failed: ${numTrials - numPassed - numTimedOut}")
    txtWriter.close()
    csvWriter.close()
  }

  def evalComparison(subs : scala.collection.Map[ArithExpr, ArithExpr], txtw : PrintWriter) : Boolean = {
    try runWithTimeout(5000) {
      val randomExpr = genExpr()
      txtw.write(s"Generated expr: $randomExpr\n")
      val simplifiedExpr = ExprSimplifier(randomExpr)
      txtw.write(s"Simplified prod: $simplifiedExpr\n")
      val randomExprEval = ArithExpr.substitute(randomExpr, subs)
      txtw.write(s"Evaluation of gen. expr: $randomExprEval\n")
      val simplifiedExprEval = ArithExpr.substitute(simplifiedExpr, subs)
      txtw.write(s"Evaluation of simpl. expr: $simplifiedExprEval\n")
      val isEq = randomExprEval == simplifiedExprEval
      if (isEq) txtw.write("Evals equal\n")
      else txtw.write("Evals don't match, inspect manually!\n")
      txtw.write(s"\n")
      isEq
    }
    catch {
      case _:TimeoutException =>
        txtw.write("Time out problem\n\n")
        numTimedOut += 1
        false
      case _:OutOfMemoryError | _:StackOverflowError =>
        txtw.write(s"Memory issue with factorisation\n\n")
        numTimedOut += 1
        false
    }
  }

  def correctnessTest(id : Int) : Unit = {
    // Seed
    rGen.setSeed(100)
    // File for logging
    val evalExprFile = new File(s"evalExpr$id.txt")
    val txtWriter = new PrintWriter(evalExprFile)

    txtWriter.write("Variable mappings\n")
    txtWriter.write(s"$valMap\n\n")

    txtWriter.write("Parameter settings\n")
    txtWriter.write(s"Size bounds: $minSize and $maxSize\n")
    txtWriter.write(s"Exponent bounds: $minExp and $maxExp\n")
    txtWriter.write(s"Constant bounds: $minCst and $maxCst\n")

    var numPassed = 0
    numTimedOut = 0
    for (i <- 0 until numTrials) {
      println(i)
      val passed = evalComparison(valMap,txtWriter)
      if (passed) numPassed += 1
    }
    txtWriter.write(s"Passed: $numPassed\n")
    txtWriter.write(s"Timed out: $numTimedOut\n")
    txtWriter.write(s"Possibly failed: ${numTrials - numPassed - numTimedOut}")
    txtWriter.close()
  }

//  private def getNumTerms(ae: ArithExpr): Int = ae match {
//    case _:Cst | _:Var => 1
//    case s:Sum => s.terms.length
//    case p:Pow =>
//      if (p.b.isInstanceOf[Sum]) p.b.getTerms.length * p.e
//      else 1
//
//    case p:Prod => p.factors.map(x => getNumTerms(x)).product
//    case _ => 1
//  }

  private def updateCstFactor(ae:ArithExpr): ArithExpr = {
    ae match {
      case cst: Cst =>
        if (cst.value < 0 && cst.value < minCst) Cst(minCst)
        else if (cst.value > 0 && cst.value > maxCst) Cst(maxCst)
        else ae
      case p: Prod =>
        if (p.cstFactor < 0 && p.cstFactor < minCst){
          val updateCst = Helper.replaceAt(0,Cst(minCst),p.factors)
          Prod(updateCst)
        }
        else if (p.cstFactor > 0 && p.cstFactor > maxCst) {
          val updateCst = Helper.replaceAt(0,Cst(maxCst),p.factors)
          Prod(updateCst)
        }
        else ae
      case s: Sum =>
        if (s.cstTerm < 0 && s.cstTerm < minCst){
          val updateCst = Helper.replaceAt(0,Cst(minCst),s.terms)
          Sum(updateCst)
        }
        else if (s.cstTerm > 0 && s.cstTerm > maxCst) {
          val updateCst = Helper.replaceAt(0,Cst(maxCst),s.terms)
          Sum(updateCst)
        }
        else ae
      case _ => ae
    }
  }

  def main(args: Array[String]): Unit = {
    // Generate mappings for variables
    for (v <- variables) {
      valMap += v -> genCst()
    }
    // Run test
    correctnessTest(0)
  }
}
