package arithmetic

import java.io._
import java.util.concurrent.TimeoutException

import arithmetic.simplifier._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._

import scala.language.postfixOps

// Object to perform evaluation of simplification using evaluator
object Evaluator {
  val maxSizeSumProd = 6 // Max number of terms/factors in sum/product
  val maxNestingDepth = 3 // Maximum depth of arithmetic expression tree
  val maxPowExp = 3 // Max exponent of a power
  val minCst: Int = -10 // Bounds for constants
  val maxCst = 10

  // Possible variables
  val av: Var = Var("a")
  val bv: Var = Var("b")
  val cv: Var = Var("c")
  val dv: Var = Var("d")
  val ev: Var = Var("e")
  val fv: Var = Var("f")
  val gv: Var = Var("g")
  val hv: Var = Var("h")
//  val iv: Var = Var("i")
//  val kv: Var = Var("k")

  val variables: Seq[Var] = List[Var](av,bv,cv,dv,ev,fv,gv,hv)
  val numPossibleVars: Int = variables.length
  val valMap = new mutable.HashMap[ArithExpr, ArithExpr]()

  // Configuration
  val numTrials = 100
  var numTimedOut = 0

  // Keeping track of information about tree
  var depth = 0
  var numLeafNodes = 0 // Constants or variables
  // The sum operator node
  // Result of applying operator on children might not be a sum
  var numSumNodes = 0
  // The product operator node
  // Result of applying operator on children might not be a product
  var numProdNodes = 0
  // The power operator node
  // Result of applying operator on children might not be a power
  var numPowNodes = 0

  val rGen = new scala.util.Random // Random generator

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
    val numTerms = 2 + rGen.nextInt(maxSizeSumProd - 2 + 1)
    if (level == maxNestingDepth) {
      val terms = List.fill(numTerms)(genLeaf())
      numLeafNodes += numTerms
      numSumNodes += 1
      ExprSimplifier(Sum(terms))
    }
    else {
      val terms = new ListBuffer[ArithExpr]()
      for (_ <- 1 to numTerms) {
        val chooseOpt = rGen.nextInt(3)
        chooseOpt match {
          case 0 =>
            terms += genLeaf()
            numLeafNodes += 1
          case 1 =>
            depth = level + 1
            terms += ExprSimplifier(genProd(level + 1))

          case 2 =>
            depth = level + 1
            terms += ExprSimplifier(genPow(level + 1))
        }
      }
      numSumNodes += 1
      Sum(terms.toList)
    }
  }

  // Generate product at a certain level of the expression tree
  def genProd(level: Int) : ArithExpr = {
    val numFactors = 2 + rGen.nextInt(maxSizeSumProd - 2 + 1)
    if (level == maxNestingDepth) {
      val factors = List.fill(numFactors)(genLeaf())
      numLeafNodes += numFactors
      numProdNodes += 1
      ExprSimplifier(Prod(factors))
    }
    else {
      val factors = new ListBuffer[ArithExpr]()
      for (_ <- 1 to numFactors) {
        val chooseOpt = rGen.nextInt(3)
        chooseOpt match {
          case 0 =>
            factors += genLeaf()
            numLeafNodes += 1
          case 1 =>
            depth = level + 1
            factors += ExprSimplifier(genSum(level + 1))
          case 2 =>
            depth = level + 1
            factors += ExprSimplifier(genPow(level + 1))
        }
      }
      numProdNodes += 1
      Prod(factors.toList)
    }
  }

  // Generate power at a certain level of the expression tree
  def genPow(level: Int) : ArithExpr = {
//    val exp = 0 + rGen.nextInt(maxPowExp) // Positive
    val exp = -maxPowExp + rGen.nextInt(2*maxPowExp+1) // Positive and negative
    if (level == maxNestingDepth) {
      val base = genLeaf()
      numLeafNodes += 1
      numPowNodes += 1
      SimplifyPow(base, exp)
    }
    else {
      val nestFurther = rGen.nextBoolean()
      var base : ArithExpr = null
      if (nestFurther) {
        depth = level + 1
        val chooseOpt = rGen.nextInt(2)
        chooseOpt match {
          case 0 => base = ExprSimplifier(genSum(level + 1))
          case 1 => base = ExprSimplifier(genProd(level + 1))
        }
      }
      else {
        base = genLeaf()
        numLeafNodes += 1
      }
      numPowNodes += 1
      Pow(base, exp)
    }
  }

  def genExpr() : ArithExpr = {
    depth = 1
    numLeafNodes = 0
    numSumNodes = 0
    numProdNodes = 0
    numPowNodes = 0
    val chooseOpt = rGen.nextInt(3)
    chooseOpt match {
      case 0 => genSum(1)
      case 1 => genProd(1)
      case 2 => genPow(1)
    }
  }

  // Function to run a block with a time out limit
  def runWithTimeout[T](timeoutMs: Long)(f: => T) : T = {
    Await.result(Future(f), timeoutMs milliseconds)
  }


  // Evaluating sum simplification
  def evalSumComparison(subs : scala.collection.Map[ArithExpr, ArithExpr], pw : PrintWriter) : Boolean = {
    try {
      runWithTimeout(3500) {
        val randomSum = genSum(level=1)
        val numNodes = numLeafNodes + numSumNodes + numSumNodes + numProdNodes
        pw.write(s"Generated sum: $randomSum\n")
        pw.write(s"Number of prod nodes in gen. sum: $numProdNodes\n")
        pw.write(s"Number of nodes in gen. sum: $numNodes\n")
        pw.write(s"Depth of gen. sum. tree: $depth\n")
        val t1 = System.nanoTime
        val simplifiedSum = ExprSimplifier(randomSum)
        val duration = (System.nanoTime - t1) / 1e6d // Runtime in ms
        pw.write(s"Simplified sum: $simplifiedSum\n")
        pw.write(s"Runtime of simplification: $duration ms\n")
        val randomSumEval = ArithExpr.substitute(randomSum, subs)
        pw.write(s"Evaluation of gen. sum: $randomSumEval\n")
        val simplifiedSumEval = ArithExpr.substitute(simplifiedSum, subs)
        pw.write(s"Evaluation of simpl. sum: $simplifiedSumEval\n")
        val isEq = randomSumEval == simplifiedSumEval ||
          randomSumEval.toSum == simplifiedSumEval.toSum
        if (!isEq) pw.write("Evals don't match, inspect manually!\n")
        pw.write(s"\n")
        isEq
      }
    }
    catch {
      case _:TimeoutException =>
        pw.write("Time out problem\n\n")
        numTimedOut += 1
        false
      case _:OutOfMemoryError | _:StackOverflowError =>
        pw.write(s"Factorisation too long problem \n\n")
        numTimedOut += 1
        false
    }
  }

  def evalSumTest(id:Int) : Unit = {
    // File for logging
    val logFile = new File(s"evalSum$id.txt")
    val printWriter = new PrintWriter(logFile)

    printWriter.write("Variable mappings\n")
    printWriter.write(s"$valMap\n\n")

    printWriter.write(s"Max sum and prod length: $maxSizeSumProd\n")
    printWriter.write(s"Max nesting depth: $maxNestingDepth\n\n")

    var numPassed = 0
    numTimedOut = 0
    for (i <- 1 to numTrials) {
      println(i)
      val passed = evalSumComparison(valMap,printWriter)
      if (passed) numPassed += 1
    }
    printWriter.write(s"Evaluations passed: $numPassed\n")
    printWriter.write(s"Evaluations timed out: $numTimedOut\n")
    printWriter.write(s"Evaluations possibly not equal: ${numTrials - numPassed - numTimedOut}")
    printWriter.close()
  }

  // Evaluating sum simplification
  def evalProdComparison(subs : scala.collection.Map[ArithExpr, ArithExpr], pw : PrintWriter) : Boolean = {
    try {
      runWithTimeout(3500) {
        val randomProd = genProd(level=1)
        val numNodes = numLeafNodes + numSumNodes + numSumNodes + numProdNodes
        pw.write(s"Generated prod: $randomProd\n")
        pw.write(s"Number of sum nodes in gen. prod: $numSumNodes\n")
        pw.write(s"Number of nodes in gen. prod: $numNodes\n")
        pw.write(s"Depth of gen. prod. tree: $depth\n")
        val t1 = System.nanoTime
        val simplifiedProd = ExprSimplifier(randomProd)
        val duration = (System.nanoTime - t1) / 1e6d // Runtime in ms
        pw.write(s"Simplified prod: $simplifiedProd\n")
        pw.write(s"Runtime of simplification: $duration ms\n")
        val randomProdEval = ArithExpr.substitute(randomProd, subs)
        pw.write(s"Evaluation of gen. prod: $randomProdEval\n")
        val simplifiedProdEval = ArithExpr.substitute(simplifiedProd, subs)
        pw.write(s"Evaluation of simpl. prod: $simplifiedProdEval\n")
        val isEq = randomProdEval == simplifiedProdEval ||
          randomProdEval.toSum == simplifiedProdEval.toSum
        if (!isEq) pw.write("Evals don't match, inspect manually!\n")
        pw.write(s"\n")
        isEq
      }
    }
    catch {
      case _:TimeoutException =>
        pw.write("Time out problem\n\n")
        numTimedOut += 1
        false
      case _:OutOfMemoryError | _:StackOverflowError =>
        pw.write(s"Factorisation too long problem \n\n")
        numTimedOut += 1
        false
    }
  }

  def evalProdTest(id:Int) : Unit = {
    // File for logging
    val logFile = new File(s"evalProd$id.txt")
    val printWriter = new PrintWriter(logFile)

    printWriter.write("Variable mappings\n")
    printWriter.write(s"$valMap\n\n")

    printWriter.write(s"Max sum and prod length: $maxSizeSumProd\n")
    printWriter.write(s"Max nesting depth: $maxNestingDepth\n\n")

    var numPassed = 0
    numTimedOut = 0
    for (i <- 1 to numTrials) {
      println(i)
      val passed = evalSumComparison(valMap,printWriter)
      if (passed) numPassed += 1
    }
    printWriter.write(s"Evaluations passed: $numPassed\n")
    printWriter.write(s"Evaluations timed out: $numTimedOut\n")
    printWriter.write(s"Evaluations possibly not equal: ${numTrials - numPassed - numTimedOut}")
    printWriter.close()
  }


  // Evaluating expression simplification
  def evalExprComparison(subs : scala.collection.Map[ArithExpr, ArithExpr], pw : PrintWriter) : Boolean = {
    try {
      runWithTimeout(3500) {
        val randomExpr = genExpr()
        val numNodes = numLeafNodes + numSumNodes + numSumNodes + numProdNodes
        pw.write(s"Generated expr: $randomExpr\n")
        pw.write(s"Number of nodes in gen. expr: $numNodes\n")
        pw.write(s"Depth of gen. expr. tree: $depth\n")
        val t1 = System.nanoTime
        val simplifiedExpr = ExprSimplifier(randomExpr)
        val duration = (System.nanoTime - t1) / 1e6d // Runtime in ms
        pw.write(s"Simplified expr: $simplifiedExpr\n")
        pw.write(s"Runtime of simplification: $duration ms\n")
        val randomExprEval = ArithExpr.substitute(randomExpr, subs)
        pw.write(s"Evaluation of gen. expr: $randomExprEval\n")
        val simplifiedExprEval = ArithExpr.substitute(simplifiedExpr, subs)
        pw.write(s"Evaluation of simpl. expr: $simplifiedExprEval\n")
        val isEq = randomExprEval == simplifiedExprEval ||
          randomExprEval.toSum == simplifiedExprEval.toSum
        if (!isEq) pw.write("Evals don't match, inspect manually!\n")
        pw.write(s"\n")
        isEq
      }
    }
    catch {
      case _:TimeoutException =>
        pw.write("Time out problem\n\n")
        numTimedOut += 1
        false
      case _:OutOfMemoryError | _:StackOverflowError =>
        pw.write(s"Factorisation too long problem \n\n")
        numTimedOut += 1
        false
    }
  }

  def evalExprTest(id: Int) : Unit = {
    // File for logging
    val logFile = new File(s"eval$id.txt")
    val printWriter = new PrintWriter(logFile)

    printWriter.write("Variable mappings\n")
    printWriter.write(s"$valMap\n\n")

    printWriter.write(s"Max sum and prod length: $maxSizeSumProd\n")
    printWriter.write(s"Max nesting depth: $maxNestingDepth\n\n")

    var numPassed = 0
    numTimedOut = 0
    for (i <- 1 to numTrials) {
      println(i)
      val passed = evalExprComparison(valMap,printWriter)
      if (passed) numPassed += 1
    }
    printWriter.write(s"Evaluations passed: $numPassed\n")
    printWriter.write(s"Evaluations timed out: $numTimedOut\n")
    printWriter.write(s"Evaluations possibly not equal: ${numTrials - numPassed - numTimedOut}")
    printWriter.close()
  }

  def main(args: Array[String]): Unit = {
    // Add mappings for variables
    for (v <- variables) {
      valMap += v -> genCst()
    }
  }
}
