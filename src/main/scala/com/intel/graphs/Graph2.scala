import scala.collection.immutable.IntMap

import scalaz.{Node => _, _}
import scalaz.std.anyVal._
import scalaz.syntax.equal._

object Gr {
  type GraphRep[A,B] = IntMap[GrContext[A,B]]
  type GrContext[A,B] = (IntMap[Vector[B]], A, IntMap[Vector[B]])

  /** An empty graph */
  def empty[A,B]: Gr[A,B] = Gr(IntMap.empty[GrContext[A,B]])

  /** Create a graph from lists of labeled nodes and edges */
  def mkGraph[A,B](vs: Seq[LNode[A]], es: Seq[LEdge[B]]): Gr[A,B] =
    empty.addNodes(vs).addEdges(es)

  /** Unlabeled node */
  type Node = Int

  /** Labeled node */
  type LNode[A] = (Node, A)

  /** Quasi-unlabeled node */
  type UNode = LNode[Unit]

  /** Unlabeled Edge */
  type Edge = (Node, Node)

  /** Labeled Edge */
  type LEdge[A] = (Node, Node, A)

  /** Quasi-unlabaled edge */
  type UEdge = LEdge[Unit]

  /**
   * The decomposition of a graph into a detached context focused on one node
   * and the rest of the graph.
   */
  type Decomp[A,B] = (Option[Context[A, B]], Gr[A,B])

  /** The same as Decomp, only more sure of itself */
  type GDecomp[A,B] = (Context[A, B], Gr[A, B])

  /** Incoming links, the node itself, a label, and outgoing links. */
  type Context[A,B] = (Adj[B], Node, A, Adj[B])

  /** Labeled links to or from a node */
  type Adj[B] = Vector[(B, Node)]

  /** Modify the value under a key in an intmap with the function `f` */
  def adjust[A](m: IntMap[A], i: Int, f: A => A): IntMap[A] =
    m.get(i).map(v => m.updated(i, f(v))).getOrElse(m)

  def clear[A,B](g: GraphRep[A,B], v: Node, ns: Vector[Node],
                 f: GrContext[A,B] => GrContext[A,B]): GraphRep[A,B] =
    if (ns.isEmpty) g else clear(adjust(g, ns.head, f), v, ns.tail, f)

  def addSucc[A,B](g: GraphRep[A, B], v: Node, lps: Vector[(B, Node)]): GraphRep[A,B] =
    if (lps.isEmpty) g else addSucc(adjust(g, lps.head._2, { (x: GrContext[A,B]) => x match {
      case (ps, lp, ss) => (ps, lp, ss.updateWith(v, Vector(lps.head._1), _ ++ _))
    }}), v, lps.tail)

  def addPred[A,B](g: GraphRep[A, B], v: Node, lss: Vector[(B, Node)]): GraphRep[A,B] =
    if (lss.isEmpty) g else addPred(adjust(g, lss.head._2, { (x: GrContext[A,B]) => x match {
      case (ps, lp, ss) => (ps.updateWith(v, Vector(lss.head._1), _ ++ _), lp, ss)
    }}), v, lss.tail)

  def clearPred[A,B](g: GraphRep[A,B], v: Node, ns: Vector[Node]): GraphRep[A,B] =
    clear(g, v, ns, { case (ps, l, ss) => (ps - v, l, ss) })

  def clearSucc[A,B](g: GraphRep[A,B], v: Node, ns: Vector[Node]): GraphRep[A,B] =
    clear(g, v, ns, { case (ps, l, ss) => (ps, l, ss - v) })

  /** Turn an intmap of vectors of labels into an adjacency list of labeled edges */
  def toAdj[B](bs: IntMap[Vector[B]]): Adj[B] = bs.toVector.flatMap {
    case (n, ls) => ls.map(m => (m, n))
  }

  /** Turn an adjacency list of labeled edges into an intmap of vectors of labels */
  def fromAdj[B](adj: Adj[B]): IntMap[Vector[B]] =
    adj.foldLeft(IntMap.empty[Vector[B]]) {
      case (m, (b, n)) => m + (n -> (m.get(n).toVector.flatten :+ b))
    }
}

import Gr._

/**
 * An efficient implementation of an inductive graph using `IntMap` (i.e. Patricia Trees).
 * Nodes are labeled with `A`, and edges are labeled with `B`.
 */
case class Gr[A,B](rep: GraphRep[A,B]) {
  def isEmpty = rep.isEmpty

  /**
   * Returns a context focused on the given node, if present,
   * and the graph with that node removed.
   */
  def decomp(n: Node): Decomp[A, B] = rep.get(n) match {
    case None => (None, this)
    case Some((p, label, s)) =>
      val g1 = rep - n
      val pp = p - n
      val sp = s - n
      val g2 = clearPred(g1, n, sp.keys.toVector)
      val g3 = clearSucc(g2, n, pp.keys.toVector)
      (Some(toAdj(pp), n, label, toAdj(s)), Gr(g3))
  }

  /**
   * Merge the given context into the graph. The context consists of a vertex, its label,
   * its successors, and its predecessors.
   */
  def &(ctx: Context[A,B]): Gr[A,B] = {
    val (p, v, l, s) = ctx
    val g1 = rep + (v -> (fromAdj(p), l, fromAdj(s)))
    val g2 = addSucc(g1, v, p)
    val g3 = addPred(g2, v, s)
    Gr(g3)
  }

  /*
  def addNode(n: LNode[A]): Gr[A,B] =
    &((Vector(), n._1, n._2, Vector()))

  def addEdge(e: LEdge[B]): Gr[A,B] = {
    val (v, w, l) = e
    val (Some((pr, _, la, su)), gp) = decomp(e._1)
    gp & ((pr, v, la, (l, w) +: su))
  }
  */

  /** Add a node to this graph */
  def addNode(n: LNode[A]): Gr[A,B] = {
    val (v, l) = n
    Gr(rep + (v -> (IntMap.empty, l, IntMap.empty)))
  }

  /** Add an edge to this graph */
  def addEdge(e: LEdge[B]): Gr[A,B] = {
    val (v, w, l) = e
    def addSuccP(p: GrContext[A,B]) = {
      val (ps, lp, ss) = p
      (ps, lp, ss.updateWith(w, Vector(l), _ ++ _))
    }
    def addPredP(p: GrContext[A,B]) = {
      val (ps, lp, ss) = p
      (ps.updateWith(v, Vector(l), _ ++ _), lp, ss)
    }
    val g1 = adjust(rep, v, addSuccP)
    Gr(adjust(g1, w, addPredP))
  }

  /** Add multiple nodes to this graph */
  def addNodes(vs: Seq[LNode[A]]): Gr[A,B] =
    vs.foldLeft(this)(_ addNode _)

  /** Add multiple edges to this graph */
  def addEdges(es: Seq[LEdge[B]]): Gr[A,B] =
    es.foldLeft(this)(_ addEdge _)

  /** Remove a node from this graph */
  def removeNode(v: Node): Gr[A,B] =
    removeNodes(Seq(v))

  /** Remove multiple nodes from this graph */
  def removeNodes(vs: Seq[Node]): Gr[A,B] =
    if (vs.isEmpty) this else decomp(vs.head)._2.removeNodes(vs.tail)

  /** Remove an edge from this graph */
  def removeEdge(e: Edge): Gr[A,B] = decomp(e._1) match {
    case (None, _) => this
    case (Some((p, v, l, s)), gp) =>
      gp & ((p, v, l, s.filter { case (_, n) => n /== e._2 }))
  }

  /** Remove an edge from this graph only if the label matches */
  def removeLEdge(e: LEdge[B])(implicit eq: Equal[B]): Gr[A,B] = decomp(e._1) match {
    case (None, _) => this
    case (Some((p, v, l, s)), gp) =>
      gp & ((p, v, l, s.filter { case (x, n) => (x /== e._3) || (n /== e._2) }))
  }

  /** Remove multiple edges from this graph */
  def removeEdges(es: Seq[Edge]): Gr[A,B] =
    es.foldLeft(this)(_ removeEdge _)

  /** A list of all the nodes in the graph */
  def nodes: Vector[LNode[A]] =
    rep.toVector map { case (node, (_, label, _)) => (node, label) }

  /**
   * Decompose this graph into the context for an arbitrarily chosen node
   * and the rest of the graph.
   */
  def decompAny: GDecomp[A,B] = nodes match {
    case Vector() => sys.error("Cannot decompose an empty graph")
    case vs =>
      val (Some(c), g) = decomp(vs.head._1)
      (c, g)
  }

  /** The number of nodes in this graph */
  def countNodes: Int = nodes.length

  /** The minimum and maximum node in a graph */
  def nodeRange: (Node, Node) = {
    val vs = nodes.map(_._1)
    (vs.min, vs.max)
  }

  /** A list of all the edges in the graph */
  def edges: Vector[LEdge[B]] =
    fold(Vector[LEdge[B]]()) {
      case ((_, v, _, s), x) => s.map { case (l, w) => (v, w, l) } ++ x
    }

  /** Fold a function over the graph */
  def fold[C](u: C)(f: (Context[A,B], C) => C): C = {
    val (c, g) = decompAny
    if (isEmpty) u else f(c, g.fold(u)(f))
  }

  def gmap[C,D](f: Context[A,B] => Context[C,D]): Gr[C,D] =
    fold(Gr.empty[C,D]) { (c, g) => g & f(c) }

}

