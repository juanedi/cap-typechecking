package ar.uba.dc.cap

import ar.uba.dc.cap._

object Term {
  type Branch = (Pattern, cap.Ctx, Term)
}

abstract class Term {
  def $(other: Term) : Term = {
    App(this, other)
  }
}
case class Var(label: String) extends Term
case class Const(label: String) extends Term
case class App(left: Term, right: Term) extends Term
case class Case(branches: Array[Term.Branch]) extends Term

abstract class Pattern {
  def freeMatchables() : List[cap.Label] = this match {
    case PMatch(label) => List(label)
    case PConst(label) => List()
    case PComp(left, right) => left.freeMatchables() ++ right.freeMatchables()
  }
  
  def $(other: Pattern) : Pattern = {
    PComp(this, other)
  }
}
case class PMatch(label: String) extends Pattern
case class PConst(label: String) extends Pattern
case class PComp(left: Pattern, right: Pattern) extends Pattern