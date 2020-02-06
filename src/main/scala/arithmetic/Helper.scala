package arithmetic

import scala.collection.mutable.ListBuffer

object Helper {

  // Expands constant multiple terms, i.e a^2 + 2ab + b^2 -> a^2 + ab + ab + b^2
  def expandTerms(terms: List[ArithExpr]) : List[ArithExpr] = {
    val expanded = ListBuffer[ArithExpr]()
    for (term <- terms) term match {
      case p:Prod =>
        val expandCst = p.asNonCstFactorsSum
        if (expandCst.isDefined) {
          expanded ++= expandCst.get.terms
        }
        else {
          expanded += p
        }
      case _ => expanded += term
    }
    expanded.toList
  }

  // Same as above but expands constants as well, i.e. 3 + 2a -> 1 + 1 + 1 + a + a
  def expandTermsCst(terms: List[ArithExpr]) : List[ArithExpr] = {
    val expanded = ListBuffer[ArithExpr]()
    for (term <- terms) term match {
      case Cst(c) =>
        if (c == 1 || c == -1) expanded += term
        else if (c>1) expanded ++= List.fill(c)(Cst(1))
        else expanded ++= List.fill(scala.math.abs(c))(Cst(-1))
      case p:Prod =>
        val expandCst = p.asNonCstFactorsSum
        if (expandCst.isDefined) {
          expanded ++= expandCst.get.terms
        }
        else {
          expanded += p
        }
      case _ => expanded += term
    }
    expanded.toList
  }

  def powerSet[A](xs: List[A]): List[List[A]] =
    xs.foldLeft(List(Nil: List[A]))((accum, elem) => accum.flatMap(l => Seq(l, elem :: l))).distinct.reverse

  def removeAt[T](i: Int, from: ListBuffer[T]): ListBuffer[T] = from.take(i) ++ from.drop(i + 1)
  def replaceAt[T](i: Int, replacement: T, from: ListBuffer[T]): ListBuffer[T] = from.zipWithIndex.map(element =>
    if (element._2 == i) replacement else element._1)


}
