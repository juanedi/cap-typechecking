package ar.uba.dc.cap

import org.scalatest.FunSuite
import ar.uba.dc.cap.dsl._

class CompatibilityTest extends FunSuite {

  type TypeAssignment = (Pattern, Type)
  
  test("p subsumes q") {
	  // --- Check that patterns are only compatible if B <= A

    val patternPairs : List[(Pattern, Pattern)] = List(
        (__v("x"), __c("c")),
        (__c("c"), __c("c")),
        (__v("x"), __c("c") $ __c("d")),
        (__v("x"), __v("y") $ __v("z"))
    )
    
    for((p1, p2) <- patternPairs) {
      assertCompatible((p1, c("e")),
                       (p2, c("e")))
    }
    
    for((p1, p2) <- patternPairs) {
      assertIncompatible((p1, c("e")),
                         (p2, c("f")))
    }
  }  
  
  test("p doesn't subsume q, disjoint patterns") {
    // --- Check that patterns are only compatible even if B </= A
    
    val patternPairs : List[(Pattern, Pattern)] = List(
        (__c("c"), __c("d")),
        (__c("c"), __v("x") $ __v("y")),
        (__v("x") $ __v("y"), __c("d"))
    )
    
    for((p1, p2) <- patternPairs) {
      assertCompatible((p1, c("e")),
                       (p2, c("f")))
    }
  }
  
  test("p doesn't subsume q, non disjoint patterns") {
    // no common symbol at mismatching position
    assertCompatible((__v("x") $ __v("y"), c("c")),
                     (__v("y"),            c("e")))
    
    assertCompatible((__c("c"), c("c")),
                     (__v("y"), c("e")))
                     
                     
    // common symbol at mismatching position, B </= A
    assertIncompatible((__v("x") $ __v("y"), c("c")),
                       (__v("y"),            c("c") + c("e")))
                                          
    assertIncompatible((__c("c"), c("c")),
                       (__v("y"), c("c") + c("e")))
                       
    
    // common symbol at mismatching position, B <= A
    assertCompatible((__v("x") $ __v("y"), c("c") + c("e")),
                     (__v("y"),            c("c")))
                   
    assertCompatible((__c("c"), c("c") + c("e")),
                     (__v("y"), c("c")))
  }

  
  def assertCompatible(a1: TypeAssignment, a2: TypeAssignment) {
    assert(check(a1, a2))
  }
  
  def assertIncompatible(a1: TypeAssignment, a2: TypeAssignment) {
    assert(!check(a1, a2))
  }

  def check(a1: TypeAssignment, a2: TypeAssignment) = {
	  val (p1,t1) = a1
			  val (p2,t2) = a2
			  
			  compatibility.check((p1, t1.toNode), (p2, t2.toNode))
  }
}