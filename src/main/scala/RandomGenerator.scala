import scala.collection.mutable.ListBuffer
import scala.util.Random

object RandomGenerator {
  // Variables
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

  // Min and max length of sum in factor
  val minSumLen = 2
  val maxSumLen = 4

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


  def genNonRepeatVarRandomProd(depth : Int) : Prod = {
    val rand = new Random
    var accumVariables = ListBuffer[Var]()
    val factors = new ListBuffer[ArithExpr]()
    // Start with sum so that there's always a factorisation (assuming depth > 1)
    val startSum = genNonConstSum(minSumLen + rand.nextInt(maxSumLen - minSumLen+1))
    factors += startSum
    for (sumVar <- startSum.terms) {
      accumVariables += sumVar.asVar.get
    }
    // Generate next terms
    for (_ <- 1 until depth) {
      val factorType = rand.nextInt(2)
      factorType match {
        case 0 =>
          var randVar = variables(rand.nextInt(numVars))
          while(listContainsVariable(accumVariables.toList,randVar)) {
            randVar = variables(rand.nextInt(numVars))
          }
          factors += randVar
          accumVariables += randVar
        case 1 =>
          var randSum = genNonConstSum(minSumLen + rand.nextInt(maxSumLen - minSumLen+1))
          while(accumVariables.foldLeft(false)((flag, v) => flag || listContainsVariable(randSum.terms,v))) {
            randSum = genNonConstSum(minSumLen + rand.nextInt(maxSumLen - minSumLen+1))
          }
          factors += randSum
          for (sumVar <- randSum.terms) {
            accumVariables += sumVar.asVar.get
          }
      }
    }
    Prod(factors.reduce((x,y) => x*y).getSumProdList)
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


  def main(args: Array[String]): Unit = {
    println("Trying a few random tests \n")
    for (depth <- 2 to 6) {
      val p = genNonRepeatVarRandomProd(depth)
      println(s"Generated product:$p")
      val pSum = p.asSum
      if (pSum.isDefined) {
        println(s"Expanded product: ${pSum.get}")
        val sumFactorisation = Factorise(pSum.get)
        println(s"Factorisation: ${sumFactorisation.get}")
        println(s"Factorisation equivalent to generated product: ${p == sumFactorisation.get}")
      }
      else {
        println("p can't be expanded into a sum")
      }
      println(s"-------------------------------- End of iteration with depth $depth --------------------------------\n")
    }
  }
}
