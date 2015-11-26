package ar.uba.dc.cap

import ar.uba.dc.cap.Term.Branch

object dsl {
  // types
  def v(index : cap.Index)  = TVar(index, cap.T)
  def dv(index : cap.Index) = TVar(index, cap.D)
  
  def c(label: cap.Label)   = TConst(label)
  def µ(t: Type) = TRec(t)
  
  // terms
  def _v(label: cap.Label) = Var(label)
  def _c(label: cap.Label) = Const(label)
  def _case(branches: Branch*) : Term = Case(Array(branches:_*))
  
  // patterns
  def __v(label: cap.Label) = PMatch(label)
  def __c(label: cap.Label) = PConst(label)
  
}