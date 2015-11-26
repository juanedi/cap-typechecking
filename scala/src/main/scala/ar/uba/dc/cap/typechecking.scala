package ar.uba.dc.cap

import cap.Assignment
import scala.collection.mutable.ListBuffer

object typechecking {

  def typeOf(ctx : cap.Ctx, term : Term) : Type = {
    check(ctx, term).toType()
  }

  private def check(ctx: cap.Ctx, term: Term): TypeNode = term match {
    case Var(label) =>
      ctx.get(label) match {
        case Some(t) => t.toNode
        case None    => TypecheckingError.varNotFound(label, ctx)
      }

    case Const(label) =>
      ConstNode(label)

    case App(a, b) => {
      val tA = check(ctx, a)
      val tB = check(ctx, b)

      if (tA.isDataType()) {
        CompNode(tA, tB)
      } else {
        val unfolded = unfold(tA)
        val funcs = toList(unfolded).asInstanceOf[List[FuncNode]] // unfold returns either a single function or a union of functions

        val (domains, results) = splitFuncs(funcs)
        if (domains.forall { d => tB <= d }) {
          fromList(results.toList)
        } else {
          TypecheckingError.app(tA, tB)
        }
      }
    }

    case Case(branches) => {
      val assignments: Array[Assignment] = branches.map { case (p, c, _) => (p, typeOfPattern(c, p)) }
      checkCompatibility(assignments)
      val resultTypes = branches.map { b => checkBody(ctx, b) }
      val domainTypes = assignments.map { case (p, t) => t }

      FuncNode(fromList(domainTypes.toList), fromList(resultTypes.toList))
    }
  }

  private def unfold(t: TypeNode): TypeNode = {
    t match {
      case FuncNode(a, b) => t
      case UnionNode(ts)  => UnionNode(ts.map(unfold(_)))
      case _              => TypecheckingError.invalidUnfold(t.toType)
    }
  }

  private def toList(t: TypeNode): List[TypeNode] = {
    t match {
      case UnionNode(ts) => ts
      case _             => List(t)
    }
  }
  
  private def fromList(ts : List[TypeNode]) : TypeNode = {
    ts match {
      case (h :: Nil) => h
      case (h :: _)   => flatUnion(ts) 
    }
  }

  private def flatUnion(types: List[TypeNode]): TypeNode = {
    UnionNode(types.flatMap(toList(_)))
  }
  
  private def splitFuncs(ts: List[FuncNode]) : (List[TypeNode], List[TypeNode]) = {
    val z : (List[TypeNode], List[TypeNode]) = (List(), List())

    ts.foldLeft(z) {
      case ((domains, results), FuncNode(a, b)) => (a :: domains, b :: results)
    }
  }
  
  private def typeOfPattern(ctx: cap.Ctx, p: Pattern): TypeNode = p match {
    case PMatch(label) =>
      ctx.get(label) match {
        case Some(t) => t.toNode
        case None    => TypecheckingError.matchableNotFound(label, ctx)
      }
    
    case PConst(label) => ConstNode(label)
    
    case PComp(a, b) => {
      val tA = typeOfPattern(ctx, a)
      val tB = typeOfPattern(ctx, b)
      if (!tA.isDataType()) {
        TypecheckingError.patternInvalidCompound(ctx, a, tA.toType)
      }
      CompNode(tA, tB)
    }
  }

  private def checkCompatibility(js: Array[Assignment]) {
    for (
      i <- (0 until js.length);
      j <- (i + 1 until js.length) if !compatible(js(i), js(j))
    ) {
      TypecheckingError.compatibility(js(i), js(j))
    }
  }
  
   
  private def compatible(a1: Assignment, a2: Assignment) : Boolean = {
    compatibility.check(a1, a2)
  }
  
  private def checkBody(ctx: cap.Ctx, branch: Term.Branch) : TypeNode = {
    branch match {
      case (p,c,t) => {
        checkDomain(c, p)
        check(merge(ctx, c), t)
      }
    }
  }
  
  private def merge(ctx1: cap.Ctx, ctx2: cap.Ctx) : cap.Ctx = {
    ctx2.foldLeft(ctx1)(_ + _)
  }
  
  private def checkDomain(ctx : cap.Ctx, p: Pattern) {
    if (p.freeMatchables().toSet != ctx.keys.toSet) {
      TypecheckingError.patternDomainCheck(ctx, p)
    }
  }
  
}

