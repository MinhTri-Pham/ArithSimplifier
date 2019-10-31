object FactoriseRuntime {
  val a = Var("a")
  val b = Var("b")
  val c = Var("c")
  val d = Var("d")
  val e = Var("e")
  val variables = List(a,b,c,d,e)

  def runtimeSquared(n : Int) : Unit = {
    val expandSquare = (Sum(variables.take(n)) pow 2).toSum.get
    println(expandSquare)
    val start = System.nanoTime()
    Factorise(expandSquare)
    val end = System.nanoTime()
    println(s"Runtime: ${(end - start) / 1e6d} ms \n")
  }

  def termsSquared(n: Int) : Unit = {
    for (i <- 2 to n) {
      println(s"Measuring runtime of factoring expansion of $i terms squared")
      runtimeSquared(i)
    }
  }

  def runtimeBinomPower(n : Int) : Unit = {
    val expandBinom = (Sum(variables.take(2)) pow n).toSum.get
    println(expandBinom)
    val start = System.nanoTime()
    Factorise(expandBinom)
    val end = System.nanoTime()
    println(s"Runtime: ${(end - start) / 1e6d} ms \n")
  }

  def binomIncreasePower(n : Int) : Unit = {
    for (i <- 2 to n) {
      println(s"Measuring runtime of factoring expansion of a+b to power $i")
      runtimeBinomPower(i)
    }
  }

  def main(args: Array[String]): Unit = {
    // n = 4 already takes very long
    termsSquared(5)
    binomIncreasePower(5)
  }
}
