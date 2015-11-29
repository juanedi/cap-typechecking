package ar.uba.dc.cap

import org.scalatest.FunSuite
import ar.uba.dc.cap.dsl._

class PathPolymorphismTest extends FunSuite {

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

  /** Builds Curry's fix point combinator Y for a particular function type */
  def fix(t: Type) : Term = {
    val fxx = _case(
      (__v("x"), Map("x" -> µ(v(1) -> t)), _v("f") $ (_v("x") $ _v("x")))
    )
    
    _case(
       (__v("f"), Map("f" -> (t -> t)), fxx $ fxx)
    )
  }
  
  test("path polymorphism") {
    val cc = c("c")
    val d = µ((dv(1) $ dv(1)) + cc)
    val a = d -> d
    
    val y = fix(a)    
    
    val t = _case(
        (__v("f"), Map("f" -> a), _case(
            (__v("x") $ __v("y"), Map("x" -> d, "y" -> d), _v("f") $ _v("x") $ (_v("f") $ _v("y"))),
            (__v("w"), Map("w" -> cc), _v("w"))
        ))
    )
    
    checkOk(Map(), y, (a -> a) -> a)
    checkOk(Map(), t)
    checkOk(Map(), y $ t)
  }
 
 
  /**
  * generic size function
  */
  test("path polymorphism - generic size") {
    val nat = c("nat")
    val sum = _v("+")
    val tSum = nat -> (nat -> nat)
    
    val a = c("A")
    val listOrTree = c("nil") + c("cons") + c("node")
    val d = µ(a + listOrTree + (dv(1) $ dv(1)))
    
    val tSize = d -> nat
    
    val size = _case(
      (__v("size"), Map("size" -> tSize), _case(
          (__v("y") $ __v("z"), Map("y" -> d, "z" -> d), _v("+") $ (_v("size") $ _v("y")) $ (_v("size") $ _v("y"))),
          (__v("x"), Map("x" -> (a + listOrTree)), _v("1"))
      ))
    )
    
    val ctx = Map("+" -> tSum, "1" -> nat)
    checkOk(ctx, fix(tSize) $ size)
  }
  
  /**
   * generic update function that traverses lists or trees to update points
   */
  test("path polymorphism - generic update") {
    val a = c("A")
    val b = c("B")
    val listOrTree = c("nil") + c("cons") + c("node")
    
    def d(t: Type) : Type = {
      µ((c("pt") $ t) + (dv(1) $ dv(1)) + listOrTree)
    }
    
    val tUpd = (a -> b) -> (d(a) -> d(b))
   
   val upd = _case(
       (__v("upd"), Map("upd" -> tUpd), _case(
           (__v("f"), Map("f" -> (a -> b)), _case(
               
               (__c("pt") $ __v("z"), Map("z" -> a), _c("pt") $ (_v("f") $ _v("z"))),
               
               (__v("x") $ __v("y"), Map("x" -> d(a), "y" -> d(a)), (_v("upd") $ _v("f") $ _v("x")) $ (_v("upd") $ _v("f") $ _v("y"))),
               
               (__v("w"), Map("w" -> listOrTree), _v("w"))
           ))
       ))
   )
   
   checkOk(Map(), fix(tUpd), (tUpd -> tUpd) -> tUpd)
   checkOk(Map(), fix(tUpd) $ upd)
  } 
  
}