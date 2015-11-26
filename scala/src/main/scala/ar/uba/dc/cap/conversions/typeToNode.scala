package ar.uba.dc.cap.conversions

import ar.uba.dc.cap._
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer

/**
 * Conversion of the standard type representation with µ binders to automata.
 * 
 * The algorithm consists of two traversals over the type. The first one converts
 * the type to an intermediate node structure that includes "ref nodes", which just
 * point to another node and will be deleted in the second pass.
 * 
 * Whenever we visit a µ during the first traversal we:
 *  
 *  1. Build a ref node pointing nowhere, and associate this refnode to the current
 *     depth (the number of µ's seen by now).
 *  
 *  2. Recurse over the body.
 *  
 *  3. Return the ref node pointing to the recursive result.
 *  
 * Note that inside step (2) we might encounter variables. Knowing the current depth
 * and the depth at which we saw each µ we can replace a bound variable by a link to 
 * the ref node created when we found the variable's binder.
 * 
 * During the second pass we traverse the node structure, skipping links to ref nodes
 * (marking nodes already visited along the way to avoid entering loops) 
 * 
 */
object typeToNode {
  
  def convert(t: Type) : TypeNode = {
    convert(t.toMUType)
  }
  
  def convert(t: MUType) : TypeNode = {
    cleanRefs(build(t))
  }

  /**
   * Builds the node structure that corresponds to the MUType
   *
   * Returns an intermediate representation where RefNodes are inserted
   * for every Rec and should be interpreted as a bridge to the target node.
   */
  private def build(t: MUType): TypeNode = {
    build(t, ArrayBuffer())
  }

  /*
   * Invariant:
   *  - all variables <= depth(ctx) will be present in ctx
   *  - variables > currentDepth are those that appear free in the original term
   */
  private def build(t: MUType, ctx: ArrayBuffer[TypeNode]): TypeNode = t match {

    case MUTVar(index, _) if index <= depth(ctx) =>
      getNode(index, ctx)

    case MUTVar(index, s) if index > depth(ctx) =>
      VarNode(index - depth(ctx), s)

    case MUTConst(label)      => ConstNode(label)
    case MUTComp(left, right) => CompNode(build(left, ctx), build(right, ctx))
    case MUTFunc(left, right) => FuncNode(build(left, ctx), build(right, ctx))
    case MUTUnion(components) => UnionNode(components.map(build(_, ctx)))

    /*
     * add RefNode to ctx so anytime a variable bound by this MUTRec
     * appears we add a link to this node.
     *
     * once we finish processing the nested structure we set the target
     * of the RefNode
     */
    case MUTRec(body) => {
      val n = RefNode(null)
      enterRec(n, ctx)
      n.target = build(body, ctx)
      leaveRec(ctx)
      n
    }

  }

  def enterRec(node: RefNode, recs: ArrayBuffer[TypeNode]) {
    recs += node
  }

  def leaveRec(recs: ArrayBuffer[TypeNode]) {
    recs.remove(recs.length - 1)
  }

  def getNode(index: cap.Index, recs: ArrayBuffer[TypeNode]): TypeNode = {
    recs(recs.length - index)
  }
  
  def depth(recs: ArrayBuffer[TypeNode]) : Integer = {
    recs.length
  }
  
  
  private def cleanRefs(t: TypeNode): TypeNode = {
    cleanRefs(t, Set())
  }
  
  private def cleanRefs(t: TypeNode, clean: Set[TypeNode]): TypeNode = t match {
    case RefNode(target) => cleanRefs(target, clean)
    case _ => {
      if (!clean.contains(t)) {
        clean += t 
        cleanSubterms(t, clean)
      }
      t
    }
  }

  private def cleanSubterms(t: TypeNode, clean: Set[TypeNode]) = t match {
    case tt @ CompNode(left, right) => {
      tt.left = cleanRefs(left, clean)
      tt.right = cleanRefs(right, clean)
    }
    case tt @ FuncNode(left, right) => {
      tt.left = cleanRefs(left, clean)
      tt.right = cleanRefs(right, clean)
    }
    case tt @ UnionNode(components) => {
      tt.children = tt.children.map(cleanRefs(_, clean))
    }
    case _ => ;
  }
  
}