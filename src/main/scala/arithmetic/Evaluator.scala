package arithmetic

import java.io._
import java.util.concurrent.TimeoutException

import arithmetic.simplifier._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._

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
  val iv: Var = Var("i")
  val kv: Var = Var("k")
  val lv: Var = Var("l")

  val variables: Seq[Var] = List[Var](av,bv,cv,dv,ev,fv,gv,hv,iv,kv,lv)
  val numVars: Int = variables.length
  val valMap = new mutable.HashMap[ArithExpr, ArithExpr]()

  val rGen = new scala.util.Random // Random generator

  def genCst() : Cst = {
   Cst(minCst + rGen.nextInt(maxCst - minCst + 1))
  }

  def genVar() : Var = {
    variables(rGen.nextInt(numVars))
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
    if (level >= maxNestingDepth) {
      val terms = List.fill(numTerms)(genLeaf())
      ExprSimplifier(Sum(terms))
    }
    else {
      val terms = new ListBuffer[ArithExpr]()
      for (_ <- 1 to numTerms) {
        val chooseOpt = rGen.nextInt(3)
        chooseOpt match {
          case 0 => terms += genLeaf()
          case 1 => terms += ExprSimplifier(genProd(level + 1))
          case 2 => terms += genPow(level + 1)
        }
      }
      Sum(terms.toList)
    }
  }

  // Generate product at a certain level of the expression tree
  def genProd(level: Int) : ArithExpr = {
    val numTerms = 2 + rGen.nextInt(maxSizeSumProd - 2 + 1)
    if (level >= maxNestingDepth) {
      val factors = List.fill(numTerms)(genLeaf())
      ExprSimplifier(Prod(factors))
    }
    else {
      val factors = new ListBuffer[ArithExpr]()
      for (_ <- 1 to numTerms) {
        val nestWithSum = rGen.nextBoolean()
        if (nestWithSum) factors += ExprSimplifier(genSum(level + 1))
        else factors += genLeaf()
      }
      Prod(factors.toList)
    }
  }

  // Generate power at a certain level of the expression tree
  def genPow(level: Int) : ArithExpr = {
//    val exp = 0 + rGen.nextInt(maxPowExp) // Positive
    val exp = -maxPowExp + rGen.nextInt(2*maxPowExp+1) // Positive and negative

    if (level >= maxNestingDepth) {
      val base = genLeaf()
      SimplifyPow(base, exp)
    }
    else {
      val nestFurther = rGen.nextBoolean()
      var base : ArithExpr = null
      if (nestFurther) {
        val chooseOpt = rGen.nextInt(2)
        chooseOpt match {
          case 0 => base = ExprSimplifier(genSum(level + 1))
          case 1 => base = ExprSimplifier(genProd(level + 1))
        }
      }
      else base = genLeaf()
      SimplifyPow(base, exp)
    }
  }

  def genExpr() : ArithExpr = {
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

  // Evaluating expression simplification
  def evalExprComparison(subs : scala.collection.Map[ArithExpr, ArithExpr], pw : PrintWriter) : Boolean = {
    try {
      val randomExpr = genExpr()
      pw.write(s"Generated expr: $randomExpr\n")
      val simplifiedExpr = simplifier.ExprSimplifier(randomExpr)
      pw.write(s"Simplified expr: $simplifiedExpr\n")
      val randomExprEval = ArithExpr.substitute(randomExpr,subs)
      pw.write(s"Evaluation of gen. expr: $randomExprEval\n")
      val simplifiedExprEval = ArithExpr.substitute(simplifiedExpr,subs)
      pw.write(s"Evaluation of simpl. expr: $simplifiedExprEval\n")
      if (randomExprEval != simplifiedExprEval) pw.write("Evals don't match!\n")
      pw.write(s"\n")
      randomExprEval == simplifiedExprEval
    }
    catch {
      case _:OutOfMemoryError | _:StackOverflowError => pw.write(s"Factorisation too long problem \n\n")
      false
    }
  }

  def evalExprTest() : Unit = {
    // File for logging
    val logFile = new File("eval.txt")
    val printWriter = new PrintWriter(logFile)

    printWriter.write("Variable mappings\n")
    printWriter.write(s"$valMap\n\n")

    printWriter.write(s"Max sum and prod length: $maxSizeSumProd\n")
    printWriter.write(s"Max nesting depth: $maxNestingDepth\n\n")

    var numPassed = 0
    val numTrials = 200
    var timeOut = 0
    for (_ <- 1 to numTrials) {
      try {
        val passed = runWithTimeout(5000)(evalExprComparison(valMap,printWriter))
        if (passed) numPassed += 1
      }
      catch {
        case _:TimeoutException =>
          timeOut += 1
          printWriter.write("Time out problem\n\n")
        case _: Throwable => printWriter.write("Other problem\n\n")
      }
    }
    printWriter.write(s"Evaluations passed: $numPassed\n")
    printWriter.write(s"Evaluations timed out: $timeOut\n")
    printWriter.write(s"Evaluations failed: ${numTrials - numPassed - timeOut}")
    printWriter.close()
  }

  def main(args: Array[String]): Unit = {
    // Add mappings for variables
    for (v <- variables) {
      valMap += v -> genCst()
    }
    evalExprTest()
  }
}
