package ar.uba.dc.cap

import ar.uba.dc.cap.conversions.typeToNode

abstract class MUType {
  override def toString = {
    this match {
      case MUTVar(index, _)      => index.toString
      case MUTConst(label)       => label
      case MUTComp(left, right)  => s"(${left} @ ${right})"
      case MUTFunc(left, right)  => s"(${left} > ${right})"
      case MUTUnion(components)  => s"(${components.mkString(" (+) ")})"
      case MUTRec(body)          => s"(µ. ${body})"
    }
  }
}

case class MUTVar(index: cap.Index, sort: cap.Sort) extends MUType
case class MUTConst(label: cap.Label) extends MUType
case class MUTComp(left: MUType, right: MUType) extends MUType
case class MUTFunc(left: MUType, right: MUType) extends MUType
case class MUTUnion(components: List[MUType]) extends MUType { assert(components.length > 1) }
case class MUTRec(body: MUType) extends MUType

object MUType {
  def toMUType(t: Type) : MUType = {
    t match {
      case TVar(index, s)      => MUTVar(index, s)
      case TConst(label)       => MUTConst(label)
      case TComp(left, right)  => MUTComp(toMUType(left), toMUType(right))
      case TFunc(left, right)  => MUTFunc(toMUType(left), toMUType(right))
      case TUnion(left, right) => flatUnion(List(toMUType(left), toMUType(right)))
      case TRec(body)          => MUTRec(toMUType(body))
    }
  }
  
  private def flatUnion(types: List[MUType]) : MUType = {
    MUTUnion(types.flatMap {
      case MUTUnion(ts) => ts
      case t            => List(t)
    })
  }

}