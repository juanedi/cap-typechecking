package ar.uba.dc.cap

import org.scalatest.FunSuite

class GraphTest extends FunSuite {
  
	
	test("computes reverse topological sort (without cycles)") {
    val g = new Graph[Char]
    g.addElement('a')
    g.addElement('b')
    g.addElement('c')

    val edges = List(
      ('b', 'a'),
      ('c', 'a'),
      ('d', 'c')
    )
    
    edges.foreach { case (p,q) =>
      g.addEdge(p,q)
    }

    g.reverseTopologicalSort()
    val result = g.sortedElements()

    edges.foreach { case (p,q) =>
      assert(result.indexOf(q) < result.indexOf(p))
    }
	}
  
  test("computes reverse topological sort (with)") {
    val g = new Graph[Char]
    g.addElement('a')
    g.addElement('b')
    g.addElement('c')
    g.addElement('d')

    g.addEdge('b', 'c')
    g.addEdge('c', 'b')
    g.addEdge('b', 'a')
    g.addEdge('d', 'c')

    g.reverseTopologicalSort()

    val result = g.sortedElements()
    
    assert(result(0) == 'a')
    assert(result(3) == 'd')
  }  
  
}