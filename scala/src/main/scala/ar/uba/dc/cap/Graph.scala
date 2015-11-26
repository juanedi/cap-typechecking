package ar.uba.dc.cap

import scala.collection.mutable.HashSet
import scala.collection.mutable.Queue
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack

/**
 * Generic directed graph of elements.
 *
 * Provides fast access to the parents of a certain node (we say p is a parent of q
 * if there exists an edge from p to q).
 * 
 * This representation allows to compute a reverse topological sort over the elements.
 * i.e.: for any two strongly connected components C1 and C2 such that there is an edge:
 * 
 *              C1 ~~~~~~~> C2
 * 
 * Then C2 will appear first in the reverse topological sort. In a non-cyclic graph
 * this means children will appear before their parents.
 */
class Graph[T] {
  
  /* since nodes are stateful we need to always use the same node for a given element */
  val nodeIndex : Map[T, Node] = HashMap()
  
  /*
   * during graph construction an edge can be added but without explicitly adding
   * the node. since we need to distinguish this intermediate state, contains(p)
   * could return false even if p is present in nodeIndex.
   */
  val nodes : Set[T] = HashSet()

  /*
   * strongly connected components in reverse topological order, which means that
   * "sink" SCCs appear before "source" SCCs.
   * 
   * this order will try to process child nodes before parents.
   */
  val sccs: ListBuffer[ListBuffer[Node]] = ListBuffer()
  
  private def node(p: T) : Node = {
    nodeIndex.getOrElseUpdate(p, new Node(p))
  }
  
  private def scc(p : T) : ListBuffer[Node] = {
    val n = node(p)
    val discoveryIndex = n.sccId.get
    val topologicalIndex = sccs.length - 1 - discoveryIndex
    sccs(topologicalIndex)
  }
  
  def addElement(p: T) {
    node(p)
    nodes += p
  }

  /** Adds an edge from p to q */
  def addEdge(p: T, q: T) {
    node(q).addIncommingEdge(node(p))
  }
  
  def contains(p: T) : Boolean = {
    nodes.contains(p)
  }
  
  def elements() : Set[T] = {
    nodes.clone()
  }
  
  /** Elements in reverse topological order */
  def sortedElements() : ListBuffer[T] = {
    sccs.flatMap { scc => scc.map { n => n.element } }
  }
  
  def parents(p: T) : ListBuffer[T] = {
    node(p).incommingEdges.map(_.element)
  }
  
  def parentsInScc(p: T): ListBuffer[T] = {
    scc(p).intersect(node(p).incommingEdges)
          .map(_.element)
  }
  
  /**
   *  Implementation of Tarjan's algorithm to detect strongly connected components
   *  in a directed graph.
   *  
   *  Components will be sorted in reverse topological order.
   */
  def reverseTopologicalSort() {
    // indices of discovery. discovery order is the inverse of topological order
    var sccId = 0
    var index = 0
    val s : Stack[Node] = Stack()

    for (p <- nodeIndex.values
        if !p.visited) {
      strongConnect(p)
    }
    
    def strongConnect(p: Node) {
      p.index = Some(index)
      p.lowlink = p.index
      index += 1
      
      s.push(p)
      p.onStack = true
      
      for(q <- p.incommingEdges) {
        if (!q.visited) {
          // Successor w has not yet been visited; recurse on it
          strongConnect(q)
          p.lowlink = Some(Math.min(p.lowlink.get, q.lowlink.get))
        } else if (q.onStack){
          // Successor w is in stack S and hence in the current SCC
          p.lowlink = Some(Math.min(p.lowlink.get, q.index.get))
        }
      }
      
      if (p.lowlink.get == p.index.get) {
        var scc : ListBuffer[Node] = ListBuffer()
        
        var q : Node = null
        do {
          q = s.pop()
          q.onStack = false
          q.sccId = Some(sccId)
          scc += q
        } while (q != p)
        
        sccs.prepend(scc)
        
        sccId += 1
      }
      
    }
  }

  class Node(val element: T) {

    // order in which the node was visited
    var index: Option[Int] = None

    // index of the node with the lowest index that can be reached traversing an incomming edgefrom this node
    var lowlink: Option[Int] = None

    // a node remains in the stack after exploration iff it has a path to some earlier node in the stack
    var onStack: Boolean = false

    // id of strongly connected component
    var sccId: Option[Int] = None

    val incommingEdges: ListBuffer[Node] = ListBuffer()

    def addIncommingEdge(n: Node) {
      incommingEdges += n
    }

    def visited: Boolean = index.isDefined

  }
  
}


