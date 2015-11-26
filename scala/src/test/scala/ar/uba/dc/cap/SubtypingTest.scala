package ar.uba.dc.cap

import org.scalatest.FunSuite
import ar.uba.dc.cap.dsl._

class SubtypingTest extends FunSuite {
  
  def deny(expresion: Boolean) { assert(!expresion) }
  
  test("variables") {
    assert (v(1) <= v(1))
    deny   (v(1) <= v(2))
  }
  
  test("constants") {
    assert (c("a") <= c("a"))
    deny   (c("a") <= c("b"))
  }
  
  test("unions") {
    assert (v(1) <= (v(1) + v(2)))
    assert (v(1) + v(2) <= (v(1) + v(2) + v(3)))
    deny ((v(1) + v(2) <= v(1)))
    assert ((v(1) + v(2)) <= (v(2) + v(1)))
  }
  
  test("functions") {
	  val s = v(1)
    val t = v(1) + v(2)
    val x = v(3)
    
    assert ((s -> t) <= (s -> t))
    
    assert ((t -> x) <= (s -> x))
    deny   ((s -> x) <= (t -> x))
    
    assert ((x -> s) <= (v(3) -> t))
    deny   ((x -> t) <= (x -> s))
  }
  
  test("compounds") {
    val d1 = c("a")
    val d2 = c("a") + c("b")
    
    val s = v(1)
    val t = v(1) + v(2)
    
    assert ((d1 $ s) <= (d2 $ s))
    deny   ((d2 $ s) <= (d1 $ s))
    
    assert ((d1 $ s) <= (d1 $ t))
    deny   ((d1 $ t) <= (d1 $ s))
  }
  
  test("rec") {
    val t1 = µ(v(1) -> v(2))
    val t2 = µ(v(1) -> v(2)) -> v(1)
    
    assert (t1 <= t2)
    assert (t2 <= t1)
    assert (t1 <= t1)
    assert (t2 <= t2)
  }
}