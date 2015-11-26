package ar.uba.dc.cap

import cap.Assignment
import cap.Index
import cap.Label

object compatibility {

  abstract class Symbol
  case class SVar(i: Index) extends Symbol
  case class SConst(l: Label) extends Symbol
  case object SComp extends Symbol
  case object SFunc extends Symbol
  
  def check(a1: Assignment, a2: Assignment): Boolean = {
    val (p, a) = a1
    val (q, b) = a2
 
    !pcomp(a1, a2) || (b <= a)
  }

  /**
   * Returns <code>true</code> iff there is a mismatch position at which
   * there is no common allowed symbol between the types assigned to the
   * sub-patterns.
   * 
   * This predicate ensures that assignments are compatible without imposing
   * any restriction between the types.
   */
  def pcomp(a1: Assignment, a2: Assignment): Boolean = (a1, a2) match {
    
    case ((PComp(p1, p2), CompNode(a1, a2)), (PComp(q1, q2), CompNode(b1, b2))) =>
      pcomp((p1, a1), (q1, b1)) && pcomp((p2, a2), (q2, b2))

    case ((p, a), (q, b)) => {
      // at least one pattern is a leaf
      val s1 = admittedSymbols(a)
      val s2 = admittedSymbols(b)
      
      headMatch(p, q) || symbolMatch(s1, s2)
    }
  }

  def admittedSymbols(t: TypeNode): List[Symbol] = t match {
    case VarNode(v, _)     => List(SVar(v))
    case ConstNode(l)      => List(SConst(l))
    case CompNode(_, _)    => List(SComp)
    case FuncNode(_, _)    => List(SFunc)
    case UnionNode(ts)     => ts.flatMap(admittedSymbols(_))
  }

  def headMatch(p: Pattern, q: Pattern): Boolean = (p, q) match {
    case (PMatch(_), _)                   => true
    case (PConst(c), PConst(d)) if c == d => true
    case _                                => false
  }

  def symbolMatch(ts1: List[Symbol], ts2: List[Symbol]): Boolean = {
    ts1.exists { s1 =>
      ts2.exists { s2 =>
        s1 == s2
      }
    }
  }

}