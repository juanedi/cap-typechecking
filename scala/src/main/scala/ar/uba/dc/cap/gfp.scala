package ar.uba.dc.cap

import scala.collection.mutable.Queue
import scala.collection.mutable.HashSet
import scala.collection.mutable.Set
import scala.collection.mutable.Map
import scala.collection.mutable.Queue
import scala.collection.mutable.ListBuffer

/**
 * Verify that a pair of types belongs to a coinductively defined relation.
 * 
 * The algorithm is parametrized with two functions:
 * 
 *  - children: determines the children of a pair during the first phase (construction
 *              of "universe" graph)
 * 
 *  - validate: given the set of current invalid pairs, checks if a pair can
 *              be invalidated or can still be considered (conditionally) valid 
 * 
 */
object gfp {

  type Pair = (TypeNode, TypeNode)
  
  type MatchCount = (Map[TypeNode, Int], Map[TypeNode, Int])
  type MatchCountIndex = Map[Pair, MatchCount]
  
  trait Instance {
    def children(p: Pair) : Iterable[Pair]
    def check(p: Pair, f: Set[Pair], mc: Option[MatchCount]) : Boolean
  }
  
  def check(p0: Pair, instance: Instance) : Boolean = {
    val mci : MatchCountIndex = Map()
    val u = buildUniverse(p0, instance, mci)
    val w = u.sortedElements
    val s : Set[Pair] = HashSet()
    val f : Set[Pair] = HashSet()
  
    while(!w.isEmpty) {
      val p : Pair = w.head
      w -= p

      if (instance.check(p, f, mci.get(p)))
        s += p
      else
        invalidate(p, u, s, w, f, mci)
    }
    
    return !f.contains(p0)
  }
  
  def buildUniverse(p0: Pair, instance: Instance, mci: MatchCountIndex) : Graph[Pair] =  {
    val pending = Queue(p0)
    val u = new Graph[Pair]
    
    while (!pending.isEmpty) {
      val p = pending.dequeue
      
      if (!u.contains(p)) {
        u.addElement(p)
        
        for { q <- instance.children(p) } {
          pending.enqueue(q)
          u.addEdge(p, q)
        }
        
        initMatchCount(p, mci)
      }
    }
    
    u.reverseTopologicalSort()
    return u
  }

  def invalidate(p: Pair, u: Graph[Pair], s: Set[Pair], w: ListBuffer[Pair], f: Set[Pair], mci : MatchCountIndex) {
    f.add(p)

    for (parent <- u.parentsInScc(p)
         if s.contains(parent)) {
    	s.remove(parent)
    	w += parent
    }
    
    for (parent    <- u.parents(p);
        (mc1, mc2) <- mci.get(parent)) {
    	val (a,b) = p
			mc1.update(a, mc1(a) -1)
			mc2.update(b, mc2(b) -1)
    }
  }

  def initMatchCount(p: Pair, mci: MatchCountIndex) : Unit = p match {
    case (UnionNode(as), UnionNode(bs)) => initMatchCount(p, as.length, bs.length, mci)
    case (a1, UnionNode(bs))            => initMatchCount(p, 1, bs.length, mci)
    case (UnionNode(as), b1)            => initMatchCount(p, 1, as.length, mci)
    case _ => ;
  }
  
  def initMatchCount(p: Pair, n: Int, m: Int, mci: MatchCountIndex) : Unit = {
    val mc1 = Map[TypeNode,Int]().withDefaultValue(m)
    val mc2 = Map[TypeNode,Int]().withDefaultValue(n)
    mci.put(p, (mc1, mc2))
  }
  
}
