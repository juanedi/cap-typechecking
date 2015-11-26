package ar.uba.dc.cap

import org.scalatest.FunSuite
import ar.uba.dc.cap.dsl._

class EquivalenceTest extends FunSuite {

  def deny(expresion: Boolean) { assert(!expresion) }
  
  test("atoms") {
    assert (v(1) ~~ v(1))
    deny   (v(1) ~~ v(2))
  }

  test("functions") {
    assert ((v(1) -> v(2)) ~~ (v(1) -> v(2)))
    deny   ((v(1) -> v(2)) ~~ (v(2) -> v(2)))
    deny   ((v(1) -> v(2)) ~~ (v(1) -> v(1)))
  }
  
  test("unions") {
    assert (v(1) ~~ (v(1) + v(1)))
    assert ((v(1) + v(1)) ~~ v(1))
    assert ((v(1) + v(1)) ~~ (v(1) + v(1)))
    assert ((v(1) + v(1)) ~~ (v(1) + v(1) + v(1)))
    assert (v(1) ~~ (v(1) + v(1) + v(1)))
    
    assert ((v(1) + v(2)) ~~ (v(1) + v(2)))
    assert ((v(1) + v(2)) ~~ (v(1) + v(2) + v(1)))
    assert ((v(1) + v(2)) ~~ (v(2) + v(1)))
    deny   ((v(1) + v(2)) ~~ (v(1) + v(1)))
    
    assert ((v(1) + (v(2) + v(3))) ~~ ((v(1) + v(2)) + v(3)))
  }
  
  test("mu's") {
    assert (µ(v(1) -> v(2)) ~~ µ(v(1) -> v(2)))
    assert (µ(v(1) -> v(2)) ~~ (µ(v(1) -> v(2)) -> v(1)))
    deny   (µ(v(1) -> v(2)) ~~ µ(v(1) -> v(3)))
    
    val t1 = µ(v(10) -> (µ(v(1) -> v(2))))
    val t2 = v(9) -> µ(v(1) -> µ(v(11) -> (µ(v(1) -> v(2)))))
    assert (t1 ~~ t2)
  }
  
}