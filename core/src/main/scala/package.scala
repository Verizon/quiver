//: ----------------------------------------------------------------------------
//: Copyright (C) 2015 Verizon.  All Rights Reserved.
//:
//:   Licensed under the Apache License, Version 2.0 (the "License");
//:   you may not use this file except in compliance with the License.
//:   You may obtain a copy of the License at
//:
//:       http://www.apache.org/licenses/LICENSE-2.0
//:
//:   Unless required by applicable law or agreed to in writing, software
//:   distributed under the License is distributed on an "AS IS" BASIS,
//:   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//:   See the License for the specific language governing permissions and
//:   limitations under the License.
//:
//: ----------------------------------------------------------------------------

import cats.{Monoid, Order}
import cats.free.Cofree
import cats.implicits._

/**
 * @groupname creation Graph Construction
 * @groupprio creation 10
 *
 * @groupname types Type Aliases
 * @groupprio types 20
 *
 * @groupname instances Type Class Instances
 * @groupprio instances 30
 */
package object quiver {

  /**
   * The internal representation of a graph
   * @group types
   */
  type GraphRep[N,A,B] = Map[N, GrContext[N,A,B]]

  /**
   * Quasi-unlabeled node
   * @group types
   */
  type UNode[N] = LNode[N,Unit]

  /**
   * Quasi-unlabaled edge
   * @group types
   */
  type UEdge[N] = LEdge[N,Unit]

  /**
   * Labeled links to or from a node
   * @group types
   */
  type Adj[N,B] = Vector[(B, N)]

  /**
   * Inward directed tree as a list of paths
   * @group types
   */
  type RTree[N] = Stream[Path[N]]

  /**
   * Inward directed tree as a list of labeled paths
   * @group types
   */
  type LRTree[N,A] = Stream[LPath[N,A]]

  /**
   * Unlabeled path through a graph
   * @group types
   */
  type Path[N] = Vector[N]

  /**
   * Labeled path through a graph
   * @group types
   */
  type LPath[N,A] = (N,Vector[(N,A)])

  /** @group instances */
  implicit def nodeOrder[N,A](implicit N: Order[N], A: Order[A]): Order[LNode[N,A]] =
    Order.whenEqual(
      Order.by[LNode[N,A], N](_.vertex),
      Order.by[LNode[N,A], A](_.label))

  /** @group instances */
  implicit def ledgeOrder[N,A](implicit N: Order[N], A: Order[A]): Order[LEdge[N,A]] =
    Order.whenEqualMonoid.combineAll(List(
      Order.by(_.from),
      Order.by(_.to),
      Order.by(_.label)))

  /** @group instances */
  implicit def edgeOrder[N,A](implicit N: Order[N]): Order[Edge[N]] =
    Order.whenEqualMonoid.combineAll(List(
      Order.by(_.from),
      Order.by(_.to)))

  /** @group instances */
  implicit def graphOrder[N,A,B](implicit N: Order[N], A: Order[A], B: Order[B]): Order[Graph[N,A,B]] = {
    implicit val L = Order[LNode[N,A]].toOrdering
    implicit val E = Order[LEdge[N,B]].toOrdering
    Order.whenEqual(
      Order.by(_.labNodes.sorted),
      Order.by(_.labEdges.sorted))
  }

  implicit def contextOrder[N,A,B](implicit N: Order[N], A: Order[A], B: Order[B]): Order[Context[N,A,B]] = {
    implicit val adj = Order[(B, N)].toOrdering
    Order.whenEqualMonoid.combineAll(List(
      Order.by(_.vertex),
      Order.by(_.label),
      Order.by(_.inAdj.sorted),
      Order.by(_.outAdj.sorted)))
  }

  implicit def gDecompOrder[N:Order,A:Order,B:Order]: Order[GDecomp[N,A,B]] =
    Order.whenEqual(
      Order.by[GDecomp[N,A,B], Context[N,A,B]](_.ctx),
      Order.by[GDecomp[N,A,B], Graph[N,A,B]](_.rest))

  /**
   * An empty graph
   * @group creation
   */
  def empty[N,A,B]: Graph[N,A,B] = Graph(Map.empty[N, GrContext[N,A,B]])

  /**
   * Create a graph from lists of labeled nodes and edges
   * @group creation
   */
  def mkGraph[N,A,B](vs: Seq[LNode[N,A]], es: Seq[LEdge[N,B]]): Graph[N,A,B] =
    empty.addNodes(vs).addEdges(es)

  /**
   * Create a graph that is a cycle of the given nodes
   * @group creation
   */
  def cycle[N](vs: Seq[N]): Graph[N,Unit,Unit] =
    mkGraph(vs.map(LNode(_, ())),
      if (vs.isEmpty) Seq()
      else (vs, vs.tail ++ Seq(vs.head)).zipped.map(LEdge[N,Unit](_, _, ())))

  /**
   * Create a directed star graph of degree `n`
   * @group creation
   */
  def star(n: Int): Graph[Int,Unit,Unit] =
    mkGraph(Range(0,n).map(LNode(_, ())), Range(1,n).map(v => LEdge(0,v,())))

  /**
   * Create an `(n,k)`-banana tree, which is an undirected graph obtained by connecting one leaf
   * of each of `n` copies of a `k`-star graph with a single root vertex that is distinct
   * from all the stars.
   * @group creation
   */
  def banana(n: Int, k: Int): Graph[Int,Unit,Unit] =
    Range(0,n).map { n =>
      val j = n * k + 1
      star(k).vmap(_ + j).addNode(LNode(0, ())).addEdge(LEdge(0, n * k + 2, ()))
    }.foldLeft(empty[Int,Unit,Unit])(_ union _).undir

  /**
   * Build a graph from lists of labeled nodes and edges, ignoring edges that
   * reference missing nodes
   * @group creation
   */
  def safeMkGraph[N,A,B](vs: Seq[LNode[N,A]], es: Seq[LEdge[N,B]]): Graph[N,A,B] =
    empty.addNodes(vs).safeAddEdges(es)

  /**
   * Build a graph from a list of contexts
   * @group creation
   */
  def buildGraph[N,A,B](ctxs: Seq[Context[N,A,B]]): Graph[N,A,B] =
    ctxs.foldLeft(empty[N,A,B])(_ & _)

  /**
   * Build a graph from elements of a partially ordered set.
   * The resulting graph has an edge from vertex `x` to vertex `y` precisely when `x <= y`.
   * @group creation
   */
  def poset[N,A](ns: Seq[(N,A)])(implicit N: PartialOrdering[N]) =
    mkGraph(
      ns.map { case (n, a) => LNode(n, a) },
      for {
        x <- ns
        y <- ns.filter(n => N.lteq(x._1,n._1))
      } yield LEdge(x._1, y._1, ()))

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

  /** Find the first path in a search tree that starts with the given node */
  def getPath[N](v: N, t: RTree[N]): Option[Path[N]] =
    t.find(_.headOption == Option(v)).map(_.reverse)

  /**
   * Find the first path in a labeled search tree that starts with the given node
   */
  def getLPath[N,A](v: N, t: LRTree[N,A]): Option[LPath[N,A]] =
    t.find(_._1 == v).map(reverseLPath)

  def reverseLPath[N,A](p: LPath[N,A]): LPath[N,A] = p match {
    case (s, op) if op.isEmpty => (s, op)
    case (s, op) => (op.last._1, op.foldLeft[(N, Vector[(N,A)])]((s, Vector.empty[(N,A)])){
      case ((on, pa), (n, l)) => (n, (on -> l) +: pa)
    }._2)
  }


  /**
   * Find all paths in a labeled search tree that start with the given node
   */
  def getLPaths[N,A](v: N, t: LRTree[N,A]): Stream[LPath[N,A]] =
    t.filter(_._1 == v).map(reverseLPath)

  /**
   * The monoid of graph unions
   * @group instances
   */
  implicit def graphMonoid[N,A,B]: Monoid[Graph[N,A,B]] = new Monoid[Graph[N,A,B]] {
    def empty = quiver.empty
    def combine(g1: Graph[N,A,B], g2: Graph[N,A,B]) = g1 union g2
  }

  // Map utilities lost when porting the old implementation
  private[quiver] implicit class MapOps[A, B](val self: Map[A, B]) extends AnyVal {
    def alter(k: A)(f: Option[B] => Option[B]): Map[A, B] =
      f(self.get(k)) match {
        case Some(v0) => self.updated(k, v0)
        case None => self - k
      }

    def insertWith(k: A, v: B)(f: (B, B) => B): Map[A, B] =
      self.get(k) match {
        case Some(v0) => self.updated(k, f(v0, v))
        case None => self.updated(k, v)
      }

    def mapKeys[A1](f: A => A1): Map[A1, B] =
      self.map { case (k, v) => f(k) -> v }
  }


  type Tree[A] = Cofree[Stream, A]

  def flattenTree[A](tree: Tree[A]): Stream[A] = {
    def go(tree: Tree[A], xs: Stream[A]): Stream[A] =
      Stream.cons(tree.head, tree.tail.value.foldRight(xs)(go(_, _)))
    go(tree, Stream.Empty)
  }
}
