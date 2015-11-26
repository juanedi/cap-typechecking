package ar.uba.dc.cap

import ar.uba.dc.cap.conversions.nodeToType

/**
 * TypeNodes represent a node in the term automata for n-ary type expressions.
 *
 * It is an explicit representation of node links with loops instead of
 * recursive mu-types.
 */

abstract class TypeNode {

  def toType() : Type = {
   nodeToType.convert(this) 
  }

  /* derived equality and toString don't play well with cyclic structures */
  override def toString = {
    s"${this.getClass.getSimpleName}[$hashCode]"
  }
  
  override def equals(other: Any): Boolean = other match {
    case that: TypeNode => this eq that
    case _              => false
  }

  override def hashCode(): Int = System.identityHashCode(this)

  def <=(other: TypeNode): Boolean = {
    subtyping.check(this, other)
  }
  
  def toDOT : String = {
    new DOTBuilder(this).build()
  }
  
  def printDOT() {
    println(toDOT)
  }

  def isDataType(): Boolean = {
    sort() == cap.D
  }

  def sort(): cap.Sort = {
    def aux(t: TypeNode, visited: Set[TypeNode]): cap.Sort = t match {
      case VarNode(index, s) => s
      case ConstNode(label)  => cap.D
      case CompNode(left, right) => cap.D
      case FuncNode(left, right) => cap.T
      case UnionNode(components) => {
        var v = visited + t
        
        val sorts = for(c <- components if !v.contains(c)) yield {
          v = v + c;
          aux(c, v + t)
        }
        
        if (sorts.forall(_ == cap.D)) {
          cap.D
        } else {
          cap.T
        }
      }
    }

    aux(this, Set())
  }
}

case class VarNode(index: cap.Index, _sort: cap.Sort) extends TypeNode
case class ConstNode(label: cap.Label) extends TypeNode
case class CompNode(var left: TypeNode, var right: TypeNode) extends TypeNode
case class FuncNode(var left: TypeNode, var right: TypeNode) extends TypeNode
case class UnionNode(var children: List[TypeNode]) extends TypeNode

case class RefNode(var target: TypeNode) extends TypeNode

