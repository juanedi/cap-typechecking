package ar.uba.dc.cap.conversions

import ar.uba.dc.cap._
import scala.collection.mutable.Map

/**
 * Conversion of TypeNode, a cyclic node structures representation of types, to the standard
 * representation using µ binders.
 * 
 * The algorithm consists of two traversals over the node structure, The first pass marks
 * "loop nodes". These are nodes where cycles are closed (this is, nodes with more than one
 * incomming edge).
 *  
 * The second pass builds the returned type, respecting the type constructors, recursing over
 * the links with the following considerations:
 * 
 *  - If a link points to a loop node that hasn't already been visited in the second pass,
 *    insert a µ before the recursive result (and keep track that a new µ has been inserted).

 *  - If a link points to a loop node that has already been visited, insert a variable with
 *    the corresponding index and stop recursion. This index is the amount of µ's inserted
 *    between (and including) the loop node and the current node.
 */
object nodeToType {

  def convert(n: TypeNode) : Type  = {
    val state = new State()
    val ref = RefNode(n);
    state(ref)
    
    detectLoops(ref, state)
    buildType(ref, state)
  } 
 
  def detectLoops(n: TypeNode, state: State) = n match {
    case CompNode(left, right) => {
      link(left, state)
      link(right, state)
    }
    case FuncNode(left, right) => {
      link(left, state)
      link(right, state)
    }
    case UnionNode(children) => children.foreach { c => link(c, state) }
    case RefNode(target)     => link(target, state)
    case _                   => ;
  }

  def link(n: TypeNode, state: State) {
    if (!state(n).visited) {
      state(n).visited = true
      detectLoops(n, state)
    } else {
      state(n).loopNode = true
    }
  }

  def buildType(n: TypeNode, state: State): Type = n match {
    case VarNode(index, s)     => TVar(index + state.depth, s)
    case ConstNode(label)      => TConst(label)
    case CompNode(left, right) => TComp(linkResult(left, state), linkResult(right, state))
    case FuncNode(left, right) => TFunc(linkResult(left, state), linkResult(right, state))
    case UnionNode(children)   => children.toStream
                                          .map(linkResult(_, state))
                                          .reduceLeft(TUnion(_, _))
    case RefNode(target)       => linkResult(target, state)
  }

  def linkResult(n: TypeNode, state: State): Type = {
    val nodeState = state(n)
    if (nodeState.loopNode) {
      // the first pass detected this is a loop node

      nodeState.loopIndex match {
        case None    => {
          /* 
           * this is the first time we see this loop node in the second pass
           * insert a µ binder and mark it so that any future appearance of
           * this node is replaced by the corresponding variable
           */
          nodeState.loopIndex = Some(state.depth)
          state.depth += 1
          TRec(buildType(n, state))
        }
      
        case Some(i) => {
          /*
           * the node was already visited in the second pass. insert a variable
           * indicating how many binders we passed
           */
    			TVar(state.depth - i, n.sort())
        }
      }
    } else {
      // not a loopNode, continue
      buildType(n, state)
    }
  }
 
  
  class State {
    
    case class NodeState(
      /* tells if a node was visited on the loop detection step */
      var visited: Boolean = false,
      
      /* tells if a node is node closes a loop */
      var loopNode: Boolean = false,
      
      /* tells the index associated with the variable bound by the corresponding mu operator */
      var loopIndex: Option[cap.Index] = None
    )
  
    val nodeStates: Map[TypeNode, NodeState] = Map()
    
    /* Counts how many binders were passed during the buildType step */
    var depth = 0
    
    // cache last retrieved element to avoid storing
    // it in a variable for repeated access
    var last : Option[(TypeNode, NodeState)] = None
    
    def apply(n: TypeNode) : NodeState = {
      if (last.isDefined && last.get._1 == n) {
        last.get._2
      } else {
        val ret = nodeStates.getOrElseUpdate(n, new NodeState)
        last = Some(n, ret)
        ret
      }
    }
  }
  
}