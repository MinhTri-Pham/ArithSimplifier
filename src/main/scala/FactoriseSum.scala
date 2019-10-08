import scala.collection.mutable.ListBuffer

object FactoriseSum {

  def apply(s:Sum) : Option[Prod] = factorise(s)

  def factorise(s: Sum) : Option[Prod] = {
    if (s.terms.length < 2) return None
    val asProds = s.asProds
    factoriseTerms(asProds)
  }

  private def factoriseTerms(terms : List[ArithExpr]) : Option[Prod] = {
    if (terms.length < 2) return None
    val leadTerm = terms.head
    for (f <- leadTerm.getSumProdList) {
      val containsF = ListBuffer[ArithExpr]()
      val rest = ListBuffer[ArithExpr]()
      for (term <- terms) {
        if (term.getSumProdList.contains(f)) containsF += term
        else rest += term
      }
      if (containsF.length > 1) {
        val fDivision = containsF.map(x => x /^ f)
        val factorisedDivision = factoriseTerms(fDivision.toList)
        val restDivision = factoriseTerms(rest.toList)
        (factorisedDivision,restDivision) match {
          case (None,None) =>
            //val fTerm = Prod(List[ArithExpr](f,Sum(fDivision.toList)))
            val fTerm = Prod(List[ArithExpr](f,fDivision.reduce((x,y)=>x+y)))
            if (rest.isEmpty) return Some(fTerm)
            else {
              val restTerm = Sum(rest.toList)
              return factoriseTerms(List(fTerm,restTerm))
            }

          case (Some(_), None) =>
            val fTerm = Prod(f :: factorisedDivision.get.factors)
            if (rest.isEmpty) return Some(fTerm)
            else {
              val restTerm = Sum(rest.toList)
              return factoriseTerms(List(fTerm,restTerm))
            }

          case (None, Some(_)) =>
            //val fTerm = Prod(List[ArithExpr](f,Sum(fDivision.toList)))
            val fTerm = Prod(List[ArithExpr](f,fDivision.reduce((x,y)=>x+y)))
            val restTerm = restDivision.get
            return factoriseTerms(List(fTerm,restTerm))

          case (Some(_), Some(_)) =>
            val fTerm = Prod(f :: factorisedDivision.get.factors)
            val restTerm = restDivision.get
            return factoriseTerms(List(fTerm,restTerm))
        }
      }
    }
    None
  }
}
