import scala.collection.mutable.ListBuffer
import scala.util.Random

object RandomGenerator {
  // Possible variables
  val av = Var("a")
  val bv = Var("b")
  val cv = Var("c")
  val dv = Var("d")
  val ev = Var("e")
  val fv = Var("f")
  val gv = Var("g")
  val hv = Var("h")
  val iv = Var("i")
  val jv = Var("j")
  val kv = Var("k")
  val lv = Var("l")
  val mv = Var("m")
  val nv = Var("n")
  val ov = Var("o")
  val pv = Var("p")
  val qv = Var("q")
  val rv = Var("r")
  val sv = Var("s")
  val tv = Var("t")

  val variables: List[Var] = List(av,bv,cv,dv,ev,gv,hv,iv,jv,kv,lv,mv,nv,ov,pv,qv,rv,sv,tv)
  val numVars: Int = variables.length

  // Below are some min/max values for types of expressions used in the random product
  // For now fairly small for testing purposes
  val maxCst = 5
  // Min and max length of sum in factor
  val minSumLen = 2
  val maxSumLen = 3

  // Generates sum with no constants and different variables
  def genNonConstSum(length: Int): Sum = {
    val terms = new ListBuffer[Var]()
    val rand = new Random
    for (_ <- 0 until length) {
      var randVar = variables(rand.nextInt(numVars))
      while(terms.contains(randVar)) {
        randVar = variables(rand.nextInt(numVars))
      }
      terms += randVar
    }
    Sum(terms.toList.sortWith(ArithExpr.isCanonicallySorted))
  }

  // Generate product sums with no repeating variable across whole expression
  def genProdSums(depth : Int) : Prod = {
    val rand = new Random
    var accumVariables = ListBuffer[Var]()
    val factors = new ListBuffer[ArithExpr]()
    // Generate sums
    for (_ <- 0 until depth) {
      var randSum = genNonConstSum(minSumLen + rand.nextInt(maxSumLen - minSumLen+1))
//      while(!randSum.terms.intersect(accumVariables).isEmpty) {
//        randSum = genNonConstSum(minSumLen + rand.nextInt(maxSumLen - minSumLen+1))
//      }
      factors += randSum
      for (sumVar <- randSum.terms) {
        accumVariables += sumVar.toVar.get
      }
    }
    Prod(factors.toList)
  }

  // Generate a product of positive constants, variables or powers of variables
  // Represents a "common term" in factorisation
  private def genCommonFactor : ArithExpr = {
    val rand = new Random
    val multiplications = 3
    var commonTerm : ArithExpr = Cst(1)
    for (_ <- 0 until multiplications) {
      val exprType = rand.nextInt(3)
      var genExpr : ArithExpr = av
      exprType match {
        case 0 => genExpr = Cst(2 + rand.nextInt(maxCst - 2 + 1))
        case 1 => genExpr = variables(rand.nextInt(numVars))
        // Variable squared (later to higher power as well)
        case 2 => genExpr = variables(rand.nextInt(numVars)) pow 2
      }
      commonTerm = commonTerm * genExpr
    }
    commonTerm
  }

  private def listContainsVariable(list: List[ArithExpr], v : Var): Boolean = {
    list.foldLeft(false)((flag,factor) => flag || containsVariable(factor,v))
  }

  @scala.annotation.tailrec
  private def containsVariable(expr: ArithExpr, v: Var) : Boolean =  expr match {
    case _:Var => expr == v
    case s:Sum => listContainsVariable(s.terms,v)
    case Pow(b,_) => containsVariable(b,v)
    case _ => false

  }

  // For i in 2,..,depth, generates a product of sums with no constant term
  // Expands it into a sum and compares factorisation with the original product
  private def tryProdsOfSums(depth: Int): Unit = {
    println("Trying a few random tests \n")
    for (d <- 2 to depth) {
      val p = genProdSums(d)
      println(s"Generated product:$p")
      val pSum = p.asExpandedSum.get
      println(s"Expanded product: $pSum")
      val start = System.nanoTime()
      val sumFactorisation = Factorise(pSum)
      val end = System.nanoTime()
      println(s"Factorisation: ${sumFactorisation.get}")
      println(s"Factorisation equivalent to generated product: ${p == sumFactorisation.get}")
      println(s"Runtime: ${(end - start) / 1e6d} ms \n")
      println(s"-------------------------------- End of iteration with depth $d --------------------------------\n")
    }
  }

  // For i in 2,..,depth
  // *  generates a product of sums with no constant term
  // *  multiplies the product obtained in previous by a common term
  // Expands the whole expression into a sum and compares factorisation with the original product
  private def tryProdsOfSumsWithCF(depth: Int): Unit = {
    println("Trying a few random tests \n")
    for (d <- 2 to depth) {
      val p = genProdSums(d)
      val cf = genCommonFactor
      val pcf = cf * p.toProd.get
      println(s"Generated product with a common factor: $pcf")
      val pcfSum = pcf.toSum.get
      println(s"Expanded product: $pcfSum")
      val sumFactorisation = Factorise(pcfSum)
      println(s"Factorisation: ${sumFactorisation.get}")
      println(s"Factorisation equivalent to generated product: ${pcf == sumFactorisation.get}")
      println(s"-------------------------------- End of iteration with depth $d --------------------------------\n")
    }
  }

  def main(args: Array[String]): Unit = {
    tryProdsOfSums(4)
  }
}
