package ar.uba.dc.cap

import ar.uba.dc.cap.gfp._
import scala.collection.mutable.Set
import scala.collection.mutable.Map

object subtyping extends gfp.Instance {

  def check(a: Type, b: Type) : Boolean = {
    check(a.toNode, b.toNode)
  }
  
  def check(a: TypeNode, b: TypeNode) : Boolean = {
    gfp.check((a,b), this)
  }
  
  def children(p: Pair) : Iterable[Pair] = p match {
    case (CompNode(d1, a1), CompNode(d2, b1)) => Array((d1, d2), (a1,b1))
    case (FuncNode(a1, a2), FuncNode(b1, b2)) => Array((b1, a1), (a2,b2))
    case (UnionNode(as), UnionNode(bs))       => for (ai <- as; bj <- bs) yield (ai,bj)
    case (a1, UnionNode(bs))                  => for (bj <- bs) yield (a1,bj)
    case (UnionNode(as), b1)                  => for (ai <- as) yield (ai,b1)
    case _                                    => List()
  }

  def check(p: Pair, f: Set[Pair], mco: Option[MatchCount]) : Boolean = p match {
    case (VarNode(i, _), VarNode(j, _)) =>
      i.equals(j)

    case (ConstNode(l1), ConstNode(l2)) =>
      l1.equals(l2)
      
    case (CompNode(d1, a1), CompNode(d2, b1)) =>
      !f.contains((d1, d2)) && !f.contains((a1, b1))

    case (FuncNode(a1, a2), FuncNode(b1, b2)) =>
      !f.contains((b1, a1)) && !f.contains((a2, b2))

    case (UnionNode(as), UnionNode(bs)) =>
      as.forall(mco.get._1(_) > 0)

    case (UnionNode(as), b1) =>
      as.forall(mco.get._1(_) == 1)

    case (a1, UnionNode(bs)) =>
      bs.exists(mco.get._2(_) > 0)

    case _ =>
      false
  }
  
}