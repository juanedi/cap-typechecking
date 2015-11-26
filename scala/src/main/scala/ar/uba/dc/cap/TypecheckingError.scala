package ar.uba.dc.cap

import cap.Assignment

class TypecheckingError(message: String, cause: Throwable = null) extends RuntimeException(message, cause)

object TypecheckingError {
  
  def raise(m: String) : Nothing = {
    throw new TypecheckingError(m)
  }
  
  def varNotFound(label: String, ctx: cap.Ctx) : Nothing = {
    raise(s"Expected variable '$label' to be defined in context $ctx")
  }
  
  def matchableNotFound(label: String, ctx: cap.Ctx) : Nothing = {
    raise(s"Expected matchable '$label' to have a type assigned in binding context $ctx")
  }
  
  def app(a: TypeNode, b: TypeNode) : Nothing = {
    raise(s"Invalid application of ${a.toType} to ${b.toType}")
  }
  
  def compatibility(a1: Assignment, a2: Assignment) : Nothing = {
    val (p1, t1) = a1
    val (p2, t2) = a1
    raise(s"Assignments ${(a1, t1.toType)} ${(a2, t2.toType)} are incompatible")
  }
  
  def patternDomainCheck(ctx : cap.Ctx, p: Pattern) : Nothing = {
    raise(s"Matchables in pattern ${p} do not match domain of ${ctx}")
  }
  
  def patternInvalidCompound(ctx: cap.Ctx, p: Pattern, t: Type) : Nothing = {
    raise(s"Invalid pattern: $p. Expected left side of compound to be a datatype but was assigned type: $t under context $ctx")
  }
  
  def invalidUnfold(t: Type) : Nothing = {
    raise(s"Type $t is not equivalent to a function")
  }
}