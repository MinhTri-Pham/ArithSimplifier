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
  val maxSizeSumProd = 5 // Max number of terms/factors in sum/product
  val minSizeSumProd = 2 // Min number of terms/factors in sum/product

  val maxNestingDepth = 3 // Maximum depth of arithmetic expression tree
  val maxPowExp = 3 // Max exponent of a power
  val minPowExp = 2 // Min exponent of a power
  val minCst: Int = -6 // Bounds for single constant leaf
  val maxCst = 6
  val cstNodeMax = 15
  val cstNodeMin: Int = -15// Bounds for constant node (result of multiple leafs combined together)

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
  val valMap = new mutable.HashMap[ArithExpr, ArithExpr]()

  // Configuration
  val numTrials = 250
  var numTimedOut = 0

  var numTermsFactors = 0 // How many terms/factors at top level for sum/product

  var numTotalTerms = 0 // How many terns in expanded form
  // How many sum factors have to be multiplied toghether - i.e how many factorisations have to be done
  var numSumFactors = 0

  val rGen = new scala.util.Random() // Random generator

  def genCst() : Cst = {
   Cst(minCst + rGen.nextInt(maxCst - minCst + 1))
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
    val numTerms = minSizeSumProd + rGen.nextInt(maxSizeSumProd - minSizeSumProd + 1)
    if (level == 1) numTermsFactors = numTerms
    if (level == maxNestingDepth) {
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
            if (level == 1) numTotalTerms += 1
          case 1 =>
            val nextTerm = updateCstFactor(ExprSimplifier(genProd(level + 1)))
            if (level == 1) numTotalTerms += getNumTerms(nextTerm)
            terms += nextTerm
          case 2 =>
            val nextTerm = updateCstFactor(ExprSimplifier(genPow(level + 1)))
            if (level == 1) numTotalTerms += getNumTerms(nextTerm)
            terms += nextTerm
        }
      }
      Sum(terms.toList)
    }
  }

  // Generate product at a certain level of the expression tree
  def genProd(level: Int) : ArithExpr = {
    val numFactors = minSizeSumProd + rGen.nextInt(maxSizeSumProd - minSizeSumProd + 1)
    if (level == 1) numTermsFactors = numFactors
    if (level == maxNestingDepth) {
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
            val expr = ExprSimplifier(genSum(level + 1))
            if (expr.isInstanceOf[Sum]) numSumFactors += 1
            factors += updateCstFactor(ExprSimplifier(expr))
          case 2 =>
            factors += updateCstFactor(ExprSimplifier(genPow(level + 1)))
        }
      }
      Prod(factors.toList)
    }
  }

  def genProdWithSumFactor() : ArithExpr = {
    val numFactors = minSizeSumProd + rGen.nextInt(maxSizeSumProd - minSizeSumProd + 1)
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
          case 1 =>
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
    val exp = 0 + rGen.nextInt(maxPowExp + 1) // Positive
//    val exp = -maxPowExp + rGen.nextInt(2*maxPowExp+1) // Positive and negative
    if (level == maxNestingDepth) {
      val base = genLeaf()
      SimplifyPow(base, exp)
    }
    else {
      val nestFurther = rGen.nextInt(3)
      var base : ArithExpr = null
      if (nestFurther != 0) {
        val chooseOpt = rGen.nextInt(2)
        chooseOpt match {
          case 0 => base = ExprSimplifier(genSum(level + 1))
          case 1 => base = ExprSimplifier(genProd(level + 1))
        }
      }
      else {
        base = genLeaf()
      }
      Pow(base, exp)
    }
  }

  def genPrimProd(level : Int) : ArithExpr = {
    val numFactors = 2 + rGen.nextInt(maxSizeSumProd - 2 + 1)
    if (level == 1) numTermsFactors = numFactors
    if (level == maxNestingDepth) {
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
    val exp = 0 + rGen.nextInt(maxPowExp + 1) // Positive
//    val exp = -maxPowExp + rGen.nextInt(2*maxPowExp+1) // Positive and negative
    if (level == maxNestingDepth) {
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

  // Function to run a block with a time out limit
  def runWithTimeout[T](timeoutMs: Long)(f: => T) : T = {
    Await.result(Future(f), timeoutMs milliseconds)
  }


  // Evaluating sum simplification
  def evalSumComparison(subs : scala.collection.Map[ArithExpr, ArithExpr], txtw : PrintWriter,
                        csvw: PrintWriter) : Boolean = {
    try {
      runWithTimeout(5000) {
        numTermsFactors = 0
        numTotalTerms = 0
        val randomSum = genSum(level=1)
        txtw.write(s"Generated sum: $randomSum\n")
        val t1 = System.nanoTime
        val simplifiedSum = ExprSimplifier(randomSum)
        val duration = (System.nanoTime - t1) / 1e6d // Runtime in ms
        val durRounded = f"$duration%.3f"
        txtw.write(s"Simplified sum: $simplifiedSum\n")
        val randomSumEval = ArithExpr.substitute(randomSum, subs)
        txtw.write(s"Evaluation of gen. sum: $randomSumEval\n")
        val simplifiedSumEval = ArithExpr.substitute(simplifiedSum, subs)
        txtw.write(s"Evaluation of simpl. sum: $simplifiedSumEval\n")
        txtw.write(s"Runtime of simplification: $durRounded\n")
        val isEq = randomSumEval == simplifiedSumEval
        if (isEq) csvw.write(s"$numTotalTerms,$durRounded\n")
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
    // Files for logging
    val evalExprFile = new File(s"evalSum$id.txt")
    val evalRuntimeFile = new File(s"evalSum$id.csv")
    val txtWriter = new PrintWriter(evalExprFile)
    val csvWriter = new PrintWriter(evalRuntimeFile)
    val header = "Num total terms with expansion,Runtime\n"
    csvWriter.write(header)

    txtWriter.write("Variable mappings\n")
    txtWriter.write(s"$valMap\n\n")

    txtWriter.write(s"Max sum and prod length: $maxSizeSumProd\n")
    txtWriter.write(s"Max nesting depth: $maxNestingDepth\n\n")

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
    val simplifiedProd = ExprSimplifier(randomProd)
    val duration = (System.nanoTime - t1) / 1e6d // Runtime in ms
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

  def evalProdPrim(id:Int) : Unit = {
    // File for logging
    val evalExprFile = new File(s"evalProdPrim$id.txt")
    val evalRuntimeFile = new File(s"evalProdPrim$id.csv")
    val txtWriter = new PrintWriter(evalExprFile)
    val csvWriter = new PrintWriter(evalRuntimeFile)

    txtWriter.write("Variable mappings\n")
    txtWriter.write(s"$valMap\n\n")

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
  def evalProdComparison(subs : scala.collection.Map[ArithExpr, ArithExpr], txtw : PrintWriter,
                                csvw: PrintWriter) : Boolean = {

    try {
      runWithTimeout(5000) {
        val randomProd = genProd(level=1)
        txtw.write(s"Generated prod: $randomProd\n")
        val t1 = System.nanoTime
        val simplifiedProd = ExprSimplifier(randomProd)
        val duration = (System.nanoTime - t1) / 1e6d // Runtime in ms
        val durRounded = f"$duration%.3f"
        txtw.write(s"Simplified prod: $simplifiedProd\n")
        val randomProdEval = ArithExpr.substitute(randomProd, subs)
        txtw.write(s"Evaluation of gen. prod: $randomProdEval\n")
        val simplifiedProdEval = ArithExpr.substitute(simplifiedProd, subs)
        txtw.write(s"Evaluation of simpl. prod: $simplifiedProdEval\n")
        val isEq = randomProdEval == simplifiedProdEval
        if (isEq) {
          csvw.write(s"$numTermsFactors,$numSumFactors,$durRounded\n")
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

  def evalProd(id:Int) : Unit = {
    // File for logging
    val evalExprFile = new File(s"evalProd$id.txt")
    val evalRuntimeFile = new File(s"evalProd$id.csv")
    val txtWriter = new PrintWriter(evalExprFile)
    val csvWriter = new PrintWriter(evalRuntimeFile)

    txtWriter.write("Variable mappings\n")
    txtWriter.write(s"$valMap\n\n")

    val header = "Number of factors,Number of factorisations,Runtime\n"
    csvWriter.write(header)

    var numPassed = 0
    numTimedOut = 0
    for (i <- 1 to numTrials) {
      numSumFactors = 0
      println(i)
      val passed = evalProdComparison(valMap,txtWriter,csvWriter)
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
      runWithTimeout(8000) {
        val randomProd = genProdWithSumFactor()
        txtw.write(s"Generated prod: $randomProd\n")
        val t1 = System.nanoTime
        val simplifiedProd = ExprSimplifier(randomProd)
        val duration = (System.nanoTime - t1) / 1e6d // Runtime in ms
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

  def evalProdWithSum(id:Int) : Unit = {
    // File for logging
    val evalExprFile = new File(s"evalProdWithSum$id.txt")
    val evalRuntimeFile = new File(s"evalProdWithSum$id.csv")
    val txtWriter = new PrintWriter(evalExprFile)
    val csvWriter = new PrintWriter(evalRuntimeFile)

    txtWriter.write("Variable mappings\n")
    txtWriter.write(s"$valMap\n\n")

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

  private def getNumTerms(ae: ArithExpr): Int = ae match {
    case _:Cst | _:Var => 1
    case s:Sum => s.terms.length
    case p:Pow =>
      if (p.b.isInstanceOf[Sum]) p.b.getTerms.length * p.e
      else 1

    case p:Prod => p.factors.map(x => getNumTerms(x)).product
    case _ => 1
  }

  private def updateCstFactor(ae:ArithExpr): ArithExpr = {
    ae match {
      case cst: Cst =>
        if (cst.value < 0 && cst.value < cstNodeMin) Cst(cstNodeMin)
        else if (cst.value > 0 && cst.value > cstNodeMax) Cst(cstNodeMax)
        else ae
      case p: Prod =>
        if (p.cstFactor < 0 && p.cstFactor < cstNodeMin){
          val updateCst = Helper.replaceAt(0,Cst(cstNodeMin),p.factors)
          Prod(updateCst)
        }
        else if (p.cstFactor > 0 && p.cstFactor > cstNodeMax) {
          val updateCst = Helper.replaceAt(0,Cst(cstNodeMax),p.factors)
          Prod(updateCst)
        }
        else ae
      case s: Sum =>
        if (s.cstTerm < 0 && s.cstTerm < cstNodeMin){
          val updateCst = Helper.replaceAt(0,Cst(cstNodeMin),s.terms)
          Sum(updateCst)
        }
        else if (s.cstTerm > 0 && s.cstTerm > cstNodeMax) {
          val updateCst = Helper.replaceAt(0,Cst(cstNodeMax),s.terms)
          Sum(updateCst)
        }
        else ae
      case _ => ae
    }
  }

  def main(args: Array[String]): Unit = {
    // Add mappings for variables
    for (v <- variables) {
      valMap += v -> genCst()
    }
    evalProdWithSum(1)
  }
}
