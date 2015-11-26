package ar.uba.dc.cap

import scala.collection.mutable.HashSet
import scala.collection.mutable.StringBuilder

/**
 * Build a DOT representation of a type's term automaton to be displayed by graphviz.
 *
 * Automata are expected to be clean of RefNodes
 */
class DOTBuilder(t: TypeNode) {

  var showSort = false 
  val nodes : HashSet[TypeNode] = HashSet()
  val edges : HashSet[(TypeNode, TypeNode, String)] = HashSet()
  
  def build() : String = {
	  val builder = new StringBuilder()
	  builder.append("digraph G {")

    process(t)
    
    builder.append("\n  {")
    
    for(n <- nodes) {
      builder.append("\n    ")
      builder.append(s"""${n.hashCode} [label="${label(n)}"];""")
    }
    
    builder.append("\n  }")
    
    for((n1,n2, label) <- edges) {
      builder.append(s"\n  ${n1.hashCode} -> ${n2.hashCode}")
      if (label != null) {
    	  builder.append(s""" [label=" ${label}"]""")
      }
      builder.append(";")
    }
    
    builder.append("\n}")
    builder.toString
  }
  
  def showingSort() : DOTBuilder = {
    showSort = true
    return this
  }
  
  private def process(n: TypeNode) {
    if (!nodes.contains(n)) {
      nodes += n
      
      n match {
        case CompNode(left, right) => {
          addEdge(n, left, "L")
          addEdge(n, right, "R")
        }
        case FuncNode(left, right) => {
          addEdge(n, left, "L")
          addEdge(n, right, "R")
        }
        case UnionNode(components) => {
          components.foreach(addEdge(n, _))          
        }
        case _ => ;
      }
    }
  }
  
  private def addEdge(a: TypeNode, b: TypeNode, label: String = null) {
    edges += ((a,b,label))
    
    process(a)
    process(b)
  }

  private def symbol(n: TypeNode) : String = n match {
      case VarNode(index, s) => s"v$index"
      case ConstNode(label)  => label
      case CompNode(_, _)    => "@"
      case FuncNode(_, _)    => ">"
      case UnionNode(_)      => "+"
  }
 
  private def label(n: TypeNode) : String = {
    if (showSort) {
    	s"${symbol(n)} | ${n.sort()}"
    } else {
      s"${symbol(n)}"
    }
  }
  
}