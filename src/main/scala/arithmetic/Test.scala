package arithmetic

object Test {

  def main(args: Array[String]): Unit = {
    val a = Var("a",Interval(NegInf, 0))
    val e = a pow -1
    println(e.min, e.max)
  }
}
