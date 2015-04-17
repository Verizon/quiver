package oncue

import scalaz._
import scalaz.syntax.std.map._
import scalaz.syntax.monoid._
import scalaz.std.vector._

package object quiver {

  /** The internal representation of a graph */
  type GraphRep[N,A,B] = Map[N, GrContext[N,A,B]]

  /** Quasi-unlabeled node */
  type UNode[N] = LNode[N,Unit]

  /** Quasi-unlabaled edge */
  type UEdge[N] = LEdge[N,Unit]

  /** Labeled links to or from a node */
  type Adj[N,B] = Vector[(B, N)]


  implicit def nodeOrder[N,A](implicit N: Order[N], A: Order[A]): Order[LNode[N,A]] =
    Order.order { (a, b) =>
      N.order(a.vertex, b.vertex) |+| A.order(a.label, b.label)
    }

  implicit def ledgeOrder[N,A](implicit N: Order[N], A: Order[A]): Order[LEdge[N,A]] =
    Order.order { (a, b) =>
      N.order(a.from, b.from) |+| N.order(a.to, b.to) |+| A.order(a.label, b.label)
    }

  implicit def edgeOrder[N,A](implicit N: Order[N]): Order[Edge[N]] =
    Order.order { (a, b) =>
      N.order(a.from, b.from) |+| N.order(a.to, b.to)
    }

  implicit def graphOrder[N,A,B](implicit N: Order[N], A: Order[A], B: Order[B]): Order[Graph[N,A,B]] =
    Order.order { (a, b) =>
      implicit val L = Order[LNode[N,A]].toScalaOrdering
      implicit val E = Order[LEdge[N,B]].toScalaOrdering
      Order[Vector[LNode[N,A]]].order(a.labNodes.sorted, b.labNodes.sorted) |+|
      Order[Vector[LEdge[N,B]]].order(a.labEdges.sorted, b.labEdges.sorted)
    }

  /** An empty graph */
  def empty[N,A,B]: Graph[N,A,B] = Graph(Map.empty[N, GrContext[N,A,B]])

  /** Create a graph from lists of labeled nodes and edges */
  def mkGraph[N,A,B](vs: Seq[LNode[N,A]], es: Seq[LEdge[N,B]]): Graph[N,A,B] =
    empty.addNodes(vs).addEdges(es)

  def safeMkGraph[N,A,B](vs: Seq[LNode[N,A]], es: Seq[LEdge[N,B]]): Graph[N,A,B] =
    empty.addNodes(vs).safeAddEdges(es)

  /** Build a graph from a list of contexts */
  def buildGraph[N,A,B](ctxs: Seq[Context[N,A,B]]): Graph[N,A,B] =
    ctxs.foldLeft(empty[N,A,B])(_ & _)

  def clear[N,A,B](g: GraphRep[N,A,B], v: N, ns: Vector[N],
                   f: GrContext[N,A,B] => GrContext[N,A,B]): GraphRep[N,A,B] =
    if (ns.isEmpty) g else clear(g.alter(ns.head)(_ map f), v, ns.tail, f)

  def addSucc[N,A,B](g: GraphRep[N,A,B], v: N, lps: Vector[(B, N)]): GraphRep[N,A,B] =
    if (lps.isEmpty) g else addSucc(g.alter(lps.head._2)(_ map { (x: GrContext[N,A,B]) => x match {
      case GrContext(ps, lp, ss) =>
        GrContext(ps, lp, ss.insertWith(v, Set(lps.head._1))(_ ++ _))
    }}), v, lps.tail)

  def addPred[N,A,B](g: GraphRep[N,A,B], v: N, lss: Vector[(B, N)]): GraphRep[N,A,B] =
    if (lss.isEmpty) g else addPred(g.alter(lss.head._2)(_ map { (x: GrContext[N,A,B]) => x match {
      case GrContext(ps, lp, ss) =>
        GrContext(ps.insertWith(v, Set(lss.head._1))(_ ++ _), lp, ss)
    }}), v, lss.tail)

  def clearPred[N,A,B](g: GraphRep[N,A,B], v: N, ns: Vector[N]): GraphRep[N,A,B] =
    if (ns.isEmpty) g else clearPred(g.alter(ns.head)(_ map {
        case GrContext(ps, l, ss) => GrContext(ps - v, l, ss)
      }), v, ns.tail)

  def clearSucc[N,A,B](g: GraphRep[N,A,B], v: N, ns: Vector[N]): GraphRep[N,A,B] =
    if (ns.isEmpty) g else clearSucc(g.alter(ns.head)(_ map {
        case GrContext(ps, l, ss) => GrContext(ps, l, ss - v)
      }), v, ns.tail)

  /** Turn an intmap of sets of labels into an adjacency list of labeled edges */
  def toAdj[N,B](bs: Map[N, Set[B]]): Adj[N,B] = bs.toVector.flatMap {
    case (n, ls) => ls.map(m => (m, n))
  }

  /** Turn an adjacency list of labeled edges into an intmap of sets of labels */
  def fromAdj[N,B](adj: Adj[N,B]): Map[N, Set[B]] =
    adj.foldLeft(Map.empty[N, Set[B]]) {
      case (m, (b, n)) => m + (n -> (m.get(n).toSet.flatten + b))
    }

  implicit def graphMonoid[N,A,B]: Monoid[Graph[N,A,B]] = new Monoid[Graph[N,A,B]] {
    def zero = empty
    def append(g1: Graph[N,A,B], g2: => Graph[N,A,B]) = g1 union g2
  }
}
