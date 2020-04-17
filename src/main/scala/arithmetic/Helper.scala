package arithmetic

import scala.collection.mutable.ListBuffer

object Helper {

  // Couple of helper methods

  // Expands constant multiple terms, i.e [a^2,2ab,b^2] -> [a^2,ab,ab,b^2]
  def expandTerms(terms: List[ArithExpr]) : List[ArithExpr] = {
    val expanded = ListBuffer[ArithExpr]()
    // Build up result list by going through input list
    for (term <- terms) term match {
      // Product - check if can expand using asNonCstFactorsSum attribute
      case p:Prod =>
        val expandCst = p.asNonCstFactorsSum // E.g. 2a -> a+a but a gives None
        if (expandCst.isDefined) expanded ++= expandCst.get.terms
        else expanded += p
      // Not a product - nothing to expand
      case _ => expanded += term
    }
    expanded.toList
  }

  // Same as above but expands constants as well, i.e. [3,2a] -> [1,1,1,a,a]
  def expandTermsCst(terms: List[ArithExpr]) : List[ArithExpr] = {
    val expanded = ListBuffer[ArithExpr]()
    // Build up result list by going through input list
    for (term <- terms) term match {
      case Cst(c) =>
        if (c == 1 || c == -1) expanded += term // Nothing to expand
        // Make a list of abs(c) ones and merge with result list being built
        else if (c>1) expanded ++= List.fill(c.toInt)(Cst(1))
        else expanded ++= List.fill(scala.math.abs(c.toInt))(Cst(-1))
      // Product - check if can expand using asNonCstFactorsSum attribute
      case p:Prod =>
        val expandCst = p.asNonCstFactorsSum // E.g. 2a -> a+a but a gives None
        if (expandCst.isDefined) expanded ++= expandCst.get.terms
        else expanded += p
      // Not a constant or a product - nothing to expand
      case _ => expanded += term
    }
    expanded.toList
  }

  // Power set of a list Taken from https://gist.github.com/dwins/3795548
  def powerSet[A](xs: List[A]): List[List[A]] =
    xs.foldLeft(List(Nil: List[A]))((accum, elem) => accum.flatMap(l => Seq(l, elem :: l))).distinct.reverse

  // Replace from(i) with replacement
  def replaceAt[T](i: Int, replacement: T, from: ListBuffer[T]): ListBuffer[T] = from.zipWithIndex.map(element =>
    if (element._2 == i) replacement else element._1)

  // Replace from(i) with replacement
  def replaceAt[T](i: Int, replacement: T, from: List[T]): List[T] = from.zipWithIndex.map(element =>
    if (element._2 == i) replacement else element._1)


}
