object FactoriseRuntime {
  val a = Var("a")
  val b = Var("b")
  val c = Var("c")
  val d = Var("d")
  val e = Var("e")
  val variables = List(a,b,c,d,e)

  def runtimeSquared(n : Int) : Unit = {
    val expandSquare = (Sum(variables.take(n)) pow 2).toSum.get
    val start = System.nanoTime()
    Factorise(expandSquare)
    val end = System.nanoTime()
    println(s"Runtime: ${(end - start) / 1e6d} ms \n")
  }

  def runtimeBinomPower(n : Int) : Unit = {
    val expandBinom = (Sum(variables.take(2)) pow n).toSum.get
    val start = System.nanoTime()
    Factorise(expandBinom)
    val end = System.nanoTime()
    println(s"Runtime: ${(end - start) / 1e6d} ms \n")
  }
  def main(args: Array[String]): Unit = {
    // n = 4 already takes very long
    runtimeBinomPower(4)
  }
}
