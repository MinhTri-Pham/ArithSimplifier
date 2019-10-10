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
    var i = 0
    while (i < terms.length) {
      val term = terms(i)
      for (f <- term.getSumProdList) {
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
              //val fTerm = Prod(List[ArithExpr](f,fDivision.reduce((x,y)=>x+y)))
              //val fTerm = Prod(SimplifyProd(f,fDivision.reduce((x,y)=>x+y)).getSumProdList)
              var fTerm : Prod = Prod(List())
              val fDivSum = Sum(fDivision.reduce((x,y)=>x+y).getSumProdList)
              val fDivSumFactorised = factoriseTerms(fDivSum.asProds)
              if (fDivSumFactorised.isDefined) fTerm = Prod(SimplifyProd(f,fDivSumFactorised.get).getSumProdList)
              else fTerm = Prod(SimplifyProd(f,fDivSum).getSumProdList)

              if (rest.isEmpty) return Some(fTerm)
              else {
                val restTerm = Sum(rest.toList)
                val combinedFactorisation = factoriseTerms(List(fTerm,restTerm))
                if (combinedFactorisation.isDefined) return combinedFactorisation
                else i+=1
              }

            case (Some(_), None) =>
              //val fTerm = Prod(f :: factorisedDivision.get.factors)
              val fTerm = Prod(SimplifyProd(f,factorisedDivision.get).getSumProdList)
              if (rest.isEmpty) return Some(fTerm)
              else {
                val restTerm = Sum(rest.toList)
                val combinedFactorisation = factoriseTerms(List(fTerm,restTerm))
                if (combinedFactorisation.isDefined) return combinedFactorisation
                else i+=1
              }

            case (None, Some(_)) =>
              //val fTerm = Prod(List[ArithExpr](f,fDivision.reduce((x,y)=>x+y)))
              //val fTerm = Prod(SimplifyProd(f,fDivision.reduce((x,y)=>x+y)).getSumProdList)
              var fTerm : Prod = Prod(List())
              val fDivSum = Sum(fDivision.reduce((x,y)=>x+y).getSumProdList)
              val fDivSumFactorised = factoriseTerms(fDivSum.asProds)
              if (fDivSumFactorised.isDefined) fTerm = Prod(SimplifyProd(f,fDivSumFactorised.get).getSumProdList)
              else fTerm = Prod(SimplifyProd(f,fDivSum).getSumProdList)

              val restTerm = restDivision.get
              val combinedFactorisation = factoriseTerms(List(fTerm,restTerm))
              if (combinedFactorisation.isDefined) return combinedFactorisation
              else i+=1

            case (Some(_), Some(_)) =>
              //val fTerm = Prod(f :: factorisedDivision.get.factors)
              val fTerm = Prod(SimplifyProd(f,factorisedDivision.get).getSumProdList)
              val restTerm = restDivision.get
              val combinedFactorisation = factoriseTerms(List(fTerm,restTerm))
              if (combinedFactorisation.isDefined) return combinedFactorisation
              else i+=1
          }
        }
      }
      i+=1
    }
    None
  }
}
