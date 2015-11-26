package ar.uba.dc.cap

import ar.uba.dc.cap.conversions.typeToNode

abstract class Type {
  
  def toMUType: MUType = {
    MUType.toMUType(this)
  }

  def toNode : TypeNode = {
    typeToNode.convert(this)
  }
  
  override def toString() = {
    this match {
      case TVar(index, _) => index.toString
      case TConst(label) => label
      case TComp(left, right) => s"(${left} @ ${right})"
      case TFunc(left, right) => s"(${left} > ${right})"
      case TUnion(left, right) => s"(${left} (+) ${right})"
      case TRec(body) => s"(µ. ${body})"
    }
  }
  
  def ~~(other: Type) : Boolean = {
    equivalence.check(this, other)
  }
  
  def <=(other: Type) : Boolean = {
    subtyping.check(this, other)
  }
  
  def +(other: Type) : Type = {
    TUnion(this, other)
  }
  
  def ->(other: Type) : Type = {
    TFunc(this, other)
  }
  
  def $(other: Type) : Type = {
    TComp(this, other)
  }
}

case class TVar(index: cap.Index, sort: cap.Sort) extends Type
case class TConst(label: cap.Label) extends Type
case class TComp(left: Type, right: Type) extends Type
case class TFunc(left: Type, right: Type) extends Type
case class TUnion(left: Type, right: Type) extends Type
case class TRec(body: Type) extends Type