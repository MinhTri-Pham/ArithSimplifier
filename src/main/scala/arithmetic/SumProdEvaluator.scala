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


object SumProdEvaluator {

  /**
   * For evaluating sum and product simplification
   */

  // Parameters
  var maxSize = 7 // Max number of terms/factors in sum/product
  var minSize = 2 // Min number of terms/factors in sum/product
  val maxDepth = 3 // Maximum depth of arithmetic expression tree
  var maxExp = 5 // Max exponent of a power
  val minExp = 2 // Min exponent of a power
  val cstSingleMin: Int = -6 // Bounds for single generated constant
  val cstSingleMax = 6
  // Bounds constants overall (sometimes multiple constants can be multiplied/added when generating a node)
  var maxCst = 128
  var minCst: Int = -128

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
  var numTimedOut = 0

  var numTermsFactors = 0 // How many terms/factors at top level for sum/product

  val rGen = new scala.util.Random() // Random generator

  def genCst() : Cst = {
   Cst(cstSingleMin + rGen.nextInt(cstSingleMax - cstSingleMin + 1))
  }

  def genVar() : Var = {
    variables(rGen.nextInt(numPossibleVars))
  }

  // Generates a random leaf - variable or a constant
  def genLeaf() : ArithExpr = {
    val isVar = rGen.nextBoolean()
    if (isVar) genVar() else genCst()
  }

  // Generates a random non-simplified sum at a certain level of the expression tree
  def genSum(level: Int) : ArithExpr = {
    val numTerms = minSize + rGen.nextInt(maxSize - minSize + 1)
    if (level == 1) numTermsFactors = numTerms
    if (level == maxDepth) {
      // Generating a sum at the maximum level, so it's a child of another node
      // Hence must be simplified
      val terms = List.fill(numTerms)(genLeaf())
      updateCstFactor(ExprSimplifier(Sum(terms)))
    }
    else {
      // Generate terms one by one, note that they must be simplified
      // (Sum itself is not simplified but its terms are)
      val terms = new ListBuffer[ArithExpr]()
      for (_ <- 0 until numTerms) {
        val chooseOpt = rGen.nextInt(3)
        // Leaf, product or power with equal chances
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

  // Generates a random non-simplified product at a certain level of the expression tree
  def genProd(level: Int) : ArithExpr = {
    val numFactors = minSize + rGen.nextInt(maxSize - minSize + 1)
    if (level == 1) numTermsFactors = numFactors
    if (level == maxDepth) {
      // Generating a product at the maximum level, so it's a child of another node
      // Hence must be simplified
      val factors = List.fill(numFactors)(genLeaf())
      updateCstFactor(ExprSimplifier(Prod(factors)))
    }
    else {
      // Generate factors one by one, note that they must be simplified
      // (Product itself is not simplified but its factors are)
      val factors = new ListBuffer[ArithExpr]()
      for (_ <- 1 to numFactors) {
        val chooseOpt = rGen.nextInt(3)
        chooseOpt match {
          // Leaf, sum or power with equal chances
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

  // Generates a random non-simplified product with at least one sum factor
  def genProdWithSumFactor() : ArithExpr = {
    val numFactors = minSize + rGen.nextInt(maxSize - minSize + 1)
    val factors = new ListBuffer[ArithExpr]()
    for (i <- 1 to numFactors) {
      // Force first factor to be a sum
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
          // Each other factor is a leaf, sum or power with equal chances
          // Again, each factor is simplified but not the whole product
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
  // Powers are generated at deeper level (never at top level), hence why they are simplified
  def genPow(level: Int) : ArithExpr = {
    val exp = minExp + rGen.nextInt(maxExp - minExp + 1) // Generate random exponent
    if (level == maxDepth) {
      val base = genLeaf()
      SimplifyPow(base, exp)
    }
    else {
      var base : ArithExpr = null
      val chooseOpt = rGen.nextInt(3)
      chooseOpt match {
        // Base can be a leaf, sum or product with equal chances
        case 0 => base = genLeaf()
        case 1 => base = ExprSimplifier(genSum(level + 1))
        case 2 => base = ExprSimplifier(genProd(level + 1))
      }
      SimplifyPow(base, exp)
    }
  }

  // Generates a random non-simplified product without sum factors
  // Similar to generating a random product (genProd) but factors can be leaf or specific power
  def genPrimProd(level : Int) : ArithExpr = {
    val numFactors = minSize + rGen.nextInt(maxSize - minSize + 1)
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

  // Similar to generating a power (genPow) but base isn't a sum
  def genPrimPow(level: Int) : ArithExpr = {
    val exp = minExp + rGen.nextInt(maxExp - minExp + 1)
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

  // Generate a random non-simplified sum or product with equal chances
  def genExpr() : ArithExpr = {
    val isSum = rGen.nextBoolean()
    if (isSum) genSum(1)
    else genProd(1)
  }

  // Function to run a block with a time out limit
  def runWithTimeout[T](timeoutMs: Long)(f: => T) : T = {
    Await.result(Future(f), timeoutMs milliseconds)
  }

  // Generate a random non-simplified sum and simplify using general simplifier
  // Evaluate both non-simplified and simplified form given mappings for variables
  // Measure (average) runtime of simplification and keep track of number of terms for the generated sum
  // Write results into files
  def evalSumComparison(subs : scala.collection.Map[ArithExpr, ArithExpr], txtw : PrintWriter,
                        csvw: PrintWriter) : Boolean = {
    try {
      runWithTimeout(5000*3) {
        val randomSum = genSum(level=1)
        txtw.write(s"Generated sum: $randomSum\n")
        // Simplify the generated sum three times and take the average runtime in ms
        val t1 = System.nanoTime
        var simplifiedSum = ExprSimplifier(randomSum)
        simplifiedSum = ExprSimplifier(randomSum)
        simplifiedSum = ExprSimplifier(randomSum)
        val duration = (System.nanoTime - t1) / 3e6d
        val durRounded = f"$duration%.3f"
        txtw.write(s"Simplified sum: $simplifiedSum\n")
        val randomSumEval = ArithExpr.substitute(randomSum, subs)
        txtw.write(s"Evaluation of gen. sum: $randomSumEval\n")
        val simplifiedSumEval = ArithExpr.substitute(simplifiedSum, subs)
        txtw.write(s"Evaluation of simpl. sum: $simplifiedSumEval\n")
        txtw.write(s"Runtime of simplification: $durRounded\n")
        val isEq = randomSumEval == simplifiedSumEval
        val len = simplifiedSum.getTerms.length // Number of terms of generated sum
        if (isEq) csvw.write(s"$len,$durRounded\n")
        else txtw.write("Evals don't match!\n")
        txtw.write(s"\n")
        isEq
      }
    }
    catch {
      // Timed out
      case _:TimeoutException =>
        txtw.write("Time out problem\n\n")
        numTimedOut += 1
        false
      // Memory issue with factorisation
      case _:OutOfMemoryError | _:StackOverflowError =>
        txtw.write(s"Memory issue with factorisation\n\n")
        numTimedOut += 1
        false
    }
  }

  // Experiment for evaluating sum simplification
  // Generates 500 + 10 random sums and simplify them. Verify correctness and analyse performance of simplification
  // Txt file for verifying correctness
  // Csv file for analysing performance
  def evalSumTest() : Unit = {
    rGen.setSeed(250)
    // Files for logging
    val evalExprFile = new File(s"evalSum.txt")
    val evalRuntimeFile = new File(s"evalSum.csv")
    val txtWriter = new PrintWriter(evalExprFile)

    // To analyse relationship between the number of terms and runtime of simplification
    val csvWriter = new PrintWriter(evalRuntimeFile)
    val header = "Number of terms,Runtime\n"
    csvWriter.write(header)

    txtWriter.write("Variable mappings\n")
    txtWriter.write(s"$valMap\n\n")

    // Parameters
    minSize = 3; maxSize = 7; minCst = -128; maxCst = 128; maxExp = 5
    val numTrialsRaw = 500
    val numTrials: Int = numTrialsRaw + offset // First offset (10) runs do not count
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

    // Correctness verification results
    txtWriter.write(s"Passed: $numPassed\n")
    txtWriter.write(s"Timed out: $numTimedOut\n")
    txtWriter.write(s"Failed: ${numTrials - numPassed - numTimedOut}")
    txtWriter.close()
    csvWriter.close()
  }

  // Generate a random non-simplified product without a sum factor and simplify using general simplifier
  // Evaluate both non-simplified and simplified form given mappings for variables
  // Measure (average) runtime of simplification
  // Write results into files
  def evalProdPrimComparison(subs : scala.collection.Map[ArithExpr, ArithExpr], txtw : PrintWriter,
                         csvw: PrintWriter) : Boolean = {
    val randomProd = genPrimProd(level=1)
    txtw.write(s"Generated prod: $randomProd\n")
    // Simplify the generated product three times and take the average runtime in ms
    val t1 = System.nanoTime
    var simplifiedProd = ExprSimplifier(randomProd)
    simplifiedProd = ExprSimplifier(randomProd)
    simplifiedProd = ExprSimplifier(randomProd)
    val duration = (System.nanoTime - t1) / (1e6d * 3)
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
    else txtw.write("Evals don't match!\n")
    txtw.write(s"\n")
    isEq
  }

  // Experiment for evaluating product simplification when no sum factors present
  // Generates 250 + 10 such products and simplify them. Verify correctness and analyse performance of simplification
  // Txt file for verifying correctness
  // Csv file for analysing performance
  def evalProdPrimTest() : Unit = {
    rGen.setSeed(400)
    // File for logging
    val evalExprFile = new File(s"evalProdPrim.txt")
    val evalRuntimeFile = new File(s"evalProdPrim.csv")
    val txtWriter = new PrintWriter(evalExprFile)
    val csvWriter = new PrintWriter(evalRuntimeFile)

    txtWriter.write("Variable mappings\n")
    txtWriter.write(s"$valMap\n\n")

    // Parameters
    minSize = 3; maxSize = 7; minCst = -128; maxCst = 128; maxExp = 5
    txtWriter.write("Parameter settings\n")
    txtWriter.write(s"Size bounds: $minSize and $maxSize\n")
    txtWriter.write(s"Exponent bounds: $minExp and $maxExp\n")
    txtWriter.write(s"Constant bounds: $minCst and $maxCst \n\n")

    // Measure runtimes
    val header = "Runtime\n"
    csvWriter.write(header)

    val numTrialsRaw = 250
    val numTrials: Int = numTrialsRaw + offset // First offset (10) runs do not count
    var numPassed = 0
    numTimedOut = 0
    for (i <- 0 until numTrials) {
      println(i)
      val passed = evalProdPrimComparison(valMap,txtWriter,csvWriter)
      if (passed) numPassed += 1
    }

    // Correctness verification results
    txtWriter.write(s"Passed: $numPassed\n")
    txtWriter.write(s"Timed out: $numTimedOut\n")
    txtWriter.write(s"Failed: ${numTrials - numPassed - numTimedOut}")
    txtWriter.close()
    csvWriter.close()
  }

  // Generate a random non-simplified product with at least one sum factor and simplify using general simplifier
  // Evaluate both non-simplified and simplified form given mappings for variables
  // Measure (average) runtime of simplification
  // Write results into files
  def evalProdWithSumComparison(subs : scala.collection.Map[ArithExpr, ArithExpr], txtw : PrintWriter,
                             csvw: PrintWriter) : Boolean = {
    try {
      runWithTimeout(10000*3) {
        val randomProd = genProdWithSumFactor()
        txtw.write(s"Generated prod: $randomProd\n")
        // Simplify the generated product three times and take the average runtime in ms
        val t1 = System.nanoTime
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
        else txtw.write("Evals don't match!\n")
        txtw.write(s"\n")
        isEq
      }
    }
    catch {
      // Timed out
      case _:TimeoutException =>
        txtw.write("Time out problem\n\n")
        numTimedOut += 1
        false
      // Memory issue with factorisation
      case _:OutOfMemoryError | _:StackOverflowError =>
        txtw.write(s"Memory issue with factorisation\n\n")
        numTimedOut += 1
        false
    }
  }

  // Experiment for evaluating product simplification when at least one sum factor present
  // Generates 250 + 10 such products and simplify them. Verify correctness and analyse performance of simplification
  // Txt file for verifying correctness
  // Csv file for analysing performance
  def evalProdWithSumTest(id:Int) : Unit = {
    rGen.setSeed(550)
    // File for logging
    val evalExprFile = new File(s"evalProdWithSum$id.txt")
    val evalRuntimeFile = new File(s"evalProdWithSum$id.csv")
    val txtWriter = new PrintWriter(evalExprFile)
    val csvWriter = new PrintWriter(evalRuntimeFile)

    txtWriter.write("Variable mappings\n")
    txtWriter.write(s"$valMap\n\n")

    // Parameters
    minSize = 3; maxSize = 5; minCst = -16; maxCst = 16; maxExp = 3
    txtWriter.write("Parameter settings\n")
    txtWriter.write(s"Size bounds: $minSize and $maxSize\n")
    txtWriter.write(s"Exponent bounds: $minExp and $maxExp\n")
    txtWriter.write(s"Constant bounds: $minCst and $maxCst\n")

    // Measure runtime
    val header = "Runtime\n"
    csvWriter.write(header)

    val numTrialsRaw = 250
    val numTrials: Int = numTrialsRaw + offset // First offset (10) runs do not count
    var numPassed = 0
    numTimedOut = 0
    for (i <- 1 to numTrials) {
      println(i)
      val passed = evalProdWithSumComparison(valMap,txtWriter,csvWriter)
      if (passed) numPassed += 1
    }

    // Correctness verification results
    txtWriter.write(s"Passed: $numPassed\n")
    txtWriter.write(s"Timed out: $numTimedOut\n")
    txtWriter.write(s"Failed: ${numTrials - numPassed - numTimedOut}")
    txtWriter.close()
    csvWriter.close()
  }

  // Generate a random non-simplified expression and simplify using general simplifier
  // Evaluate both non-simplified and simplified form given mappings for variables
  // Write results into file
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
      else txtw.write("Evals don't match!\n")
      txtw.write(s"\n")
      isEq
    }
    catch {
      // Timed out
      case _:TimeoutException =>
        txtw.write("Time out problem\n\n")
        numTimedOut += 1
        false
      // Memory issue with factorisation
      case _:OutOfMemoryError | _:StackOverflowError =>
        txtw.write(s"Memory issue with factorisation\n\n")
        numTimedOut += 1
        false
    }
  }

  // Experiment for evaluating correctness of simplification
  // Generates 750 + 10 random expressions and simplify them
  // Check that they evaluate to same value given mappings for variables
  def correctnessTest() : Unit = {
    rGen.setSeed(100)
    // File for logging
    val evalExprFile = new File(s"evalExpr.txt")
    val txtWriter = new PrintWriter(evalExprFile)

    txtWriter.write("Variable mappings\n")
    txtWriter.write(s"$valMap\n\n")

    // Parameters
    minSize = 2; maxSize = 5; minCst = -64; maxCst = 64; maxExp = 5
    txtWriter.write("Parameter settings\n")
    txtWriter.write(s"Size bounds: $minSize and $maxSize\n")
    txtWriter.write(s"Exponent bounds: $minExp and $maxExp\n")
    txtWriter.write(s"Constant bounds: $minCst and $maxCst\n\n")

    val numTrialsRaw = 750
    val numTrials: Int = numTrialsRaw + offset
    var numPassed = 0
    numTimedOut = 0
    for (i <- 0 until numTrials) {
      println(i)
      val passed = evalComparison(valMap,txtWriter)
      if (passed) numPassed += 1
    }

    // Results
    txtWriter.write(s"Passed: $numPassed\n")
    txtWriter.write(s"Timed out: $numTimedOut\n")
    txtWriter.write(s"Failed: ${numTrials - numPassed - numTimedOut}")
    txtWriter.close()
  }

  // Keep any constants between minCst and maxCst
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
}
