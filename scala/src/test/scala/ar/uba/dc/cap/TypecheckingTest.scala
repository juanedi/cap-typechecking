package ar.uba.dc.cap

import org.scalatest.FunSuite
import ar.uba.dc.cap.dsl._

class TypecheckingTest extends FunSuite {

	def checkOk(ctx: cap.Ctx, term: Term) {
		typechecking.typeOf(ctx, term)
	}

  def checkOk(ctx: cap.Ctx, term: Term, _type: Type) {
    val assigned = typechecking.typeOf(ctx, term)
    assert(assigned ~~ _type)
  }
  
  def checkFail(ctx: cap.Ctx, term: Term) {
    intercept[TypecheckingError] { typechecking.typeOf(ctx, term) }
  }

  test("atoms") {
    checkOk(Map(), _c("c"), c("c"))
    checkOk(Map("a" -> c("c")), _v("a"), c("c"))
    
    checkFail(Map(), _v("a"))
  }
 
  test("case") {
   checkOk(
     Map(),
     _case((__c("c"), Map(), _c("d"))),
     c("c") -> c("d")
   )
   
   checkFail(
     Map(),
     _case((__c("c"), Map(), _v("a")))
   )
   
   checkOk(
     Map("a" -> v(1)),
     _case((__c("c"), Map(), _v("a"))),
     c("c") -> v(1)
   )
   
   checkOk(
     Map(),
     _case((__v("a"), Map("a" -> v(1)), _c("d"))),
     v(1) -> c("d")
   )
   
   checkOk(
     Map(),
     _case((__v("a"), Map("a" -> v(1)), _v("a"))),
     v(1) -> v(1)
   )
   
   // invalid binding context for branch
   checkFail(
     Map(),
     _case((__c("c"), Map("a" -> v(1)), _c("c")))
   )
   
   // invalid binding context for branch
   checkFail(
     Map(),
     _case((__v("a"), Map(), _c("c")))
   )
   
   checkOk(
     Map(), 
     _case(
       (__c("c"), Map(), _c("e")),
       (__c("d"), Map(), _c("e"))
     ),
     (c("c") + c("d")) -> c("e")
   )
   
   // overlapping incompatible branches
   checkFail(
     Map(),
     _case(
       (__c("c") $ __v("x"), Map("x" -> v(1)), _c("e")),
       (__c("c") $ __v("y"), Map("y" -> v(2)), _c("e"))
     )
   )
   
   checkOk(
     Map(), 
     _case(
       (__c("c"), Map(), _c("e")),
       (__c("d"), Map(), _c("f"))
     ),
     (c("c") + c("d")) -> (c("e") + c("f"))
   )
  }
  
  test("application") {
    checkOk(
      Map("a" -> v(1)),
      _c("c") $ _v("a"),
      c("c") $ v(1)
    )
    
    checkOk(
      Map(),
      _case((__c("c"), Map(), _c("d"))) $ _c("c"),
      c("d")
    )
    
    // argument type not compatible with single branch
    checkFail(
      Map(),
      _case((__c("c"), Map(), _c("d"))) $ _c("e")
    )
    
    // argument type not compatible with one branch
    checkFail(
      Map(),
      _case(
        (__c("c"), Map(), _c("e")),
        (__c("d"), Map(), _c("e"))
      ) $ _c("f")
    )
  }
  
}