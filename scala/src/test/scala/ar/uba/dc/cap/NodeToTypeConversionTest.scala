package ar.uba.dc.cap

import ar.uba.dc.cap.dsl._
import org.scalatest.FunSuite
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

class NodeToTypeConversionTest extends FunSuite {

  def checkConversion(t: Type) {
    assert(t.toNode.toType ~~ t)
  }
  
  test("constants") {
	  checkConversion(c("c"))
  }
  
  test("compounds and functions") {
    List(
        c("c") $ c("d"),
        c("c") -> c("d")
    ).foreach(checkConversion(_))
  }
  
  test("unions") {
    List(
        c("c") + c("d") + c("e"),
        c("c") + (c("d") + c("e"))
    ).foreach(checkConversion(_))
  }
  
  test("free variables") {
    List(
        v(1),
        µ(dv(2) $ v(3))
    ).foreach(checkConversion(_))
  }
  
  test("bound variables") {
    List(
        µ(v(1) -> c("c")),
        µ(c("c") -> v(1)),
        µ(v(1) -> µ(v(2) -> v(1) -> v(3)))
    ).foreach(checkConversion(_))
  }

}