package arithmetic
package experiment

import java.util.concurrent.TimeoutException
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._

import simplifier._

// Object to perform evaluation of simplification using evaluator
object Evaluator {
  val maxSizeSumProd = 5 // Max number of terms/factors in sum/product
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
    val exp = 0 + rGen.nextInt(maxPowExp) // Positive
//    val exp = -maxPowExp + rGen.nextInt(2*maxPowExp+1) // Positive and negative

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

  // Function to run a block with a time out limit
  def runWithTimeout[T](timeoutMs: Long)(f: => T) : T = {
    Await.result(Future(f), timeoutMs milliseconds)
  }

  // Evaluating sum simplification
  def evalSumTest(subs : scala.collection.Map[ArithExpr, ArithExpr]) : Boolean = {
    val randomSum = genSum(1)
    println(s"Generated sum: $randomSum")
    val simplifiedSum = simplifier.ExprSimplifier(randomSum)
    println(s"Simplified sum: $simplifiedSum")
//    val randomSumEval = ArithExpr.evaluate(randomSum,subs)
    val randomSumEval = ArithExpr.substitute(randomSum,subs)
    println(s"Evaluation of gen. sum: $randomSumEval")
//    val simplifiedSumEval = ArithExpr.evaluate(simplifiedSum,subs)
    val simplifiedSumEval = ArithExpr.substitute(simplifiedSum,subs)
    println(s"Evaluation of simpl. sum: $simplifiedSumEval")
    if (randomSumEval != simplifiedSumEval) println("Evals don't match!\n")
    else println()
    randomSumEval == simplifiedSumEval
  }

  // Evaluating product simplification
  def evalProdTest(subs : scala.collection.Map[ArithExpr, ArithExpr]) : Boolean = {
    val randomProd = genProd(1)
    println(s"Generated product: $randomProd")
    val simplifiedProd = ExprSimplifier(randomProd)
    println(s"Simplified product: $simplifiedProd")
//    val randomProdEval = ArithExpr.evaluate(randomProd,subs)
    val randomProdEval = ArithExpr.substitute(randomProd,subs)
    println(s"Evaluation of gen. product: $randomProdEval")
//    val simplifiedProdEval = ArithExpr.evaluate(simplifiedProd,subs)
    val simplifiedProdEval = ArithExpr.substitute(simplifiedProd,subs)
    println(s"Evaluation of simpl. product: $simplifiedProdEval \n")
    randomProdEval == simplifiedProdEval
  }

  def main(args: Array[String]): Unit = {
    // Add mappings for variables
    for (v <- variables) {
      valMap += v -> genCst()
    }

    println("Variable mappings")
    println(valMap)
    println()

    for (_ <- 1 to 5000) {
      // Try some evaluations (just for now)
      try {
        runWithTimeout(5000)(evalSumTest(valMap))
//        runWithTimeout(5000)(evalProdTest(valMap))
      }
      catch {
        case _:TimeoutException => println("Time out problem\n")
        case _:IllegalArgumentException => println("Canonical order problem\n")
        case _: Throwable => println("Other problem \n")
      }
    }
  }
}
