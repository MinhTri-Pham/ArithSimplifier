object FactoriseRuntime {
  val a = Var("a")
  val b = Var("b")
  val c = Var("c")
  val d = Var("d")
  val e = Var("e")
  val variables = List(a,b,c,d,e)

  def runtimeSquared(n : Int) : Double = {
    val expandSquare = (Sum(variables.take(n)) pow 2).toSum.get
    println(expandSquare)
    val start = System.nanoTime()
    Factorise(expandSquare)
    val end = System.nanoTime()
    (end - start) / 1e6d
  }

  def termsSquared(n: Int) : Unit = {
    for (i <- 2 to n) {
      println(s"Measuring runtime of factoring expansion of $i terms squared")
      println(s"Runtime: ${runtimeSquared(i)} ms \n")
    }
  }

  def runtimeBinomPower(n : Int) : Double = {
    val expandBinom = (Sum(variables.take(2)) pow n).toSum.get
    println(expandBinom)
    val start = System.nanoTime()
    Factorise(expandBinom)
    val end = System.nanoTime()
    (end - start) / 1e6d
  }

  def binomIncreasePower(n : Int) : Unit = {
    for (i <- 2 to n) {
      println(s"Measuring runtime of factoring expansion of a+b to power $i")
      println(s"Runtime: ${runtimeBinomPower(i)} ms \n")
    }
  }

  def main(args: Array[String]): Unit = {
    termsSquared(3)
    binomIncreasePower(3)
  }
}
