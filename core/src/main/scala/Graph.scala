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
package quiver

import scalaz._
import scalaz.syntax.std.map._
import scalaz.syntax.monoid._
import scalaz.std.vector._

/** Unlabeled Edge */
case class Edge[N](from: N, to: N)

/** Labeled Edge */
case class LEdge[N,A](from: N, to: N, label: A) {
  def map[B](f: A => B): LEdge[N,B] = LEdge(from, to, f(label))
  def edge: Edge[N] = Edge(from, to)
}

/** Labeled Node */
case class LNode[N,A](vertex: N, label: A) {
  def map[B](f: A => B): LNode[N,B] = LNode(vertex, f(label))
}

/**
 * An implementation of an inductive graph where nodes of type
 * `N `are labeled with `A`, and edges are labeled with `B`.
 */
case class Graph[N,A,B](rep: GraphRep[N,A,B]) {
  def isEmpty = rep.isEmpty

  /**
   * Returns a context focused on the given node, if present,
   * and the graph with that node removed.
   */
  def decomp(n: N): Decomp[N,A,B] = rep.get(n) match {
    case None => Decomp(None, this)
    case Some(GrContext(p, label, s)) =>
      val g1 = rep - n
      val pp = p - n
      val sp = s - n
      val g2 = clearPred(g1, n, sp.keys.toVector)
      val g3 = clearSucc(g2, n, pp.keys.toVector)
      Decomp(Some(Context(toAdj(pp), n, label, toAdj(s))), Graph(g3))
  }

  def bidecomp(first: N, last: N): Option[BiDecomp[N,A,B]] = {
    val Decomp(c1, r1) = decomp(first)
    val Decomp(c2, _) = decomp(last)
    for {
      x <- c1
      y <- c2
    } yield BiDecomp(x, y, r1.decomp(y.vertex).rest)
  }

  /**
   * Embed the given context in the graph. If the context's vertex is already in
   * the graph, removes the old context from the graph first. This operation is
   * the deterministic inverse of `decomp` and obeys the following laws:
   *
   * `(g & c) decomp c.vertex == Decomp(Some(c), g)`
   * `(g decomp c.vertex).rest & c == (g & c)`
   */
  def &(ctx: Context[N,A,B]): Graph[N,A,B] = {
    val Context(p, v, l, s) = ctx
    val r = decomp(v).rest.rep
    val g1 = r + (v -> GrContext(fromAdj(p), l, fromAdj(s)))
    val g2 = addSucc(g1, v, p)
    val g3 = addPred(g2, v, s)
    Graph(g3)
  }

  /** Alias for `&` */
  def embed(ctx: Context[N,A,B]): Graph[N,A,B] = &(ctx)

  /**
   * Add a node to this graph. If this node already exists with a different label,
   * its label will be replaced with this new one.
   */
  def addNode(n: LNode[N,A]): Graph[N,A,B] = {
    val LNode(v, l) = n
    val Decomp(ctx, g) = decomp(v)
    g & ctx.map {
      case x:Context[N,A,B] => x.copy(label = l)
    }.getOrElse(Context(Vector(), v, l, Vector()))
  }

  /**
   * Add an edge to this graph.
   * Throws an error if the source and target nodes don't exist in the graph.
   */
  def addEdge(e: LEdge[N,B]): Graph[N,A,B] =
    safeAddEdge(e,
      sys.error(s"Can't add edge $e since the source and target nodes don't both exist in the graph."))

  /**
   * Add an edge to this graph. If the source and target nodes don't exist in this graph,
   * return the given `failover` graph.
   */
  def safeAddEdge(e: LEdge[N,B], failover: => Graph[N,A,B] = this): Graph[N,A,B] = {
    val LEdge(v, w, l) = e
    val ks = rep.keySet
    if (ks.contains(v) && ks.contains(w)) {
      def addSuccP(p: GrContext[N,A,B]) = {
        val GrContext(ps, lp, ss) = p
        GrContext(ps, lp, ss.insertWith(w, Set(l))(_ ++ _))
      }
      def addPredP(p: GrContext[N,A,B]) = {
        val GrContext(ps, lp, ss) = p
        GrContext(ps.insertWith(v, Set(l))(_ ++ _), lp, ss)
      }
      val g1 = rep.alter(v)(_ map addSuccP)
      Graph(g1.alter(w)(_ map addPredP))
    } else failover
  }

  /** Add multiple nodes to this graph */
  def addNodes(vs: Seq[LNode[N,A]]): Graph[N,A,B] =
    vs.foldLeft(this)(_ addNode _)

  /** Add multiple edges to this graph */
  def addEdges(es: Seq[LEdge[N,B]]): Graph[N,A,B] =
    es.foldLeft(this)(_ addEdge _)

  /**
   * Add multiple edges to this graph, ignoring edges whose source and target nodes
   * don't already exist in the graph.
   */
  def safeAddEdges(es: Seq[LEdge[N,B]]): Graph[N,A,B] =
    es.foldLeft(this)(_ safeAddEdge _)

  /** Adds all the nodes and edges from one graph to another. */
  def union(g: Graph[N,A,B]): Graph[N,A,B] =
    addNodes(g.labNodes).addEdges(g.labEdges)

  /** Remove a node from this graph */
  def removeNode(v: N): Graph[N,A,B] =
    removeNodes(Seq(v))

  /** Remove multiple nodes from this graph */
  def removeNodes(vs: Seq[N]): Graph[N,A,B] =
    if (vs.isEmpty) this else decomp(vs.head).rest.removeNodes(vs.tail)

  /** Remove an edge from this graph */
  def removeEdge(e: Edge[N]): Graph[N,A,B] = decomp(e.from) match {
    case Decomp(None, _) => this
    case Decomp(Some(Context(p, v, l, s)), gp) =>
      gp & Context(p, v, l, s.filter { case (_, n) => n != e.to })
  }

  /** Remove an edge from this graph only if the label matches */
  def removeLEdge(e: LEdge[N,B]): Graph[N,A,B] = decomp(e.from) match {
    case Decomp(None, _) => this
    case Decomp(Some(Context(p, v, l, s)), gp) =>
      gp & Context(p, v, l, s.filter { case (x, n) => (x != e.label) || (n != e.to) })
  }

  /** Remove multiple edges from this graph */
  def removeEdges(es: Seq[Edge[N]]): Graph[N,A,B] =
    es.foldLeft(this)(_ removeEdge _)

  /** A list of all the nodes in the graph and their labels */
  def labNodes: Vector[LNode[N,A]] =
    rep.toVector map { case (node, GrContext(_, label, _)) => LNode(node, label) }

  /** A list of all the nodes in the graph */
  def nodes: Vector[N] = labNodes.map(_.vertex)

  /**
   * Decompose this graph into the context for an arbitrarily chosen node
   * and the rest of the graph.
   */
  def decompAny: Decomp[N,A,B] =
    if (isEmpty)
      Decomp(None, this)
    else
      decomp(rep.head._1)

  /** The number of nodes in this graph */
  def countNodes: Int = rep.size

  /** A list of all the edges in the graph and their labels */
  def labEdges: Vector[LEdge[N,B]] = for {
    (node, GrContext(_, _, s)) <- rep.toVector
    (next, labels) <- s.toVector
    label <- labels
  } yield LEdge(node, next, label)

  /** A list of all the edges in the graph */
  def edges: Vector[Edge[N]] = labEdges.map { case LEdge(v, w, _) => Edge(v, w) }

  /** Fold a function over the graph */
  def fold[C](u: C)(f: (Context[N,A,B], C) => C): C = {
    val Decomp(c, g) = decompAny
    c.map(x => f(x, g.fold(u)(f))) getOrElse u
  }

  /** Get all the contexts in the graph, as a vector */
  def contexts: Vector[Context[N,A,B]] =
    fold(Vector[Context[N,A,B]]())(_ +: _)

  /** Map a function over the graph */
  def gmap[C,D](f: Context[N,A,B] => Context[N,C,D]): Graph[N,C,D] =
    Graph(rep.map { case (k, v) => k -> f(v.toContext(k)).toGrContext })

  /** Map a function over the node labels in the grap */
  def nmap[C](f: A => C): Graph[N,C,B] =
    Graph(rep.mapValues { case GrContext(ps, a, ss) => GrContext(ps, f(a), ss) })

  /** Map a function over the edge labels in the graph */
  def emap[C](f: B => C): Graph[N,A,C] =
    Graph(rep.mapValues {
      case GrContext(ps, a, ss) =>
        GrContext(ps mapValues (_ map f), a, ss mapValues (_ map f))
    })

  /** Map over the unique node identifiers in the graph */
  def vmap[M](f: N => M): Graph[M,A,B] =
    Graph(rep.map {
      case (k, GrContext(ps, a, ss)) => f(k) -> GrContext(ps mapKeys f, a, ss mapKeys f)
    })

  /** Returns true if the given node is in the graph, otherwise false */
  def contains(v: N): Boolean = decomp(v) match {
    case Decomp(Some(_), _) => true
    case _ => false
  }

  /**
   * Find the context for the given node. Causes an error if the node is not
   * present in the graph.
   */
  def context(v: N): Context[N,A,B] =
    decomp(v).ctx.getOrElse(sys.error(s"Node $v is not present in the graph"))

  /** All the inbound links of the given node, including self-edges */
  def ins(v: N): Adj[N,B] =
    context(v).ins

  /** All the outbound links of the given node, including self-edges */
  def outs(v: N): Adj[N,B] =
    context(v).outs

  /** Find the label for a node */
  def label(v: N): Option[A] =
    decomp(v).ctx.map(_.label)

  /** Find the neighbors of a node */
  def neighbors(v: N): Vector[N] = {
    val Context(p, _, _, s) = context(v)
    (p ++ s).map(_._2)
  }

  /** Find all nodes that have a link from the given node */
  def successors(v: N): Vector[N] =
    outs(v).map(_._2)

  /** Find all nodes that have a link to the given node */
  def predecessors(v: N): Vector[N] =
    ins(v).map(_._2)

  /** Find all outbound edges for the given node */
  def outEdges(v: N): Vector[LEdge[N,B]] =
    outs(v).map { case (l, w) => LEdge(v, w, l) }

  /** Find all inbound edges for the given node */
  def inEdges(v: N): Vector[LEdge[N,B]] =
    ins(v).map { case (l, w) => LEdge(v, w, l) }

  /** The number of outbound edges from the given node */
  def outDegree(v: N): Int =
    outs(v).length

  /** The number of inbound edges from the given node */
  def inDegree(v: N): Int =
    ins(v).length

  /** The number of connections to and from the given node */
  def degree(v: N): Int = {
    val Context(p, _, _, s) = context(v)
    p.length + s.length
  }

  /** Find an edge between two nodes */
  def findEdge(e: Edge[N]): Option[LEdge[N,B]] =
    labEdges.find(c => c.from == e.from && c.to == e.to)

  /** Replace an edge with a new one */
  def updateEdge(e: LEdge[N,B]): Graph[N,A,B] =
    removeEdge(Edge(e.from, e.to)).addEdge(e)

  /** Update multiple edges */
  def updateEdges(es: Seq[LEdge[N,B]]): Graph[N,A,B] =
    es.foldLeft(this)(_ updateEdge _)

  /** Replace a node with a new one */
  def updateNode(n: LNode[N,A]): Graph[N,A,B] =
    decomp(n.vertex) match {
      case Decomp(Some(Context(p, v, l, s)), rest) =>
        rest & Context(p, n.vertex, n.label, s)
      case _ => this
    }

  /** Update multiple nodes */
  def updateNodes(ns: Seq[LNode[N,A]]): Graph[N,A,B] =
    ns.foldLeft(this)(_ updateNode _)

  /** Reverse the direction of all edges */
  def reverse: Graph[N,A,B] = gmap {
    case Context(p, v, l, s) => Context(s, v, l, p)
  }

  /**
   * Generalized depth-first search.
   */
  final def xdfsWith[C](vs: Seq[N],
                        d: Context[N,A,B] => Seq[N],
                        f: Context[N,A,B] => C): Vector[C] =
    if (vs.isEmpty || isEmpty) Vector()
    else decomp(vs.head) match {
      case Decomp(Some(c), g) => f(c) +: g.xdfsWith(d(c) ++ vs.tail, d, f)
      case Decomp(None, g) => g.xdfsWith(vs.tail, d, f)
    }

  /**
   * Forward depth-first search.
   */
  def dfsWith[C](vs: Seq[N], f: Context[N,A,B] => C): Seq[C] =
    xdfsWith(vs, _.successors, f)

  /** Forward depth-first search. */
  def dfs(vs: Seq[N]): Seq[N] = dfsWith(vs, _.vertex)

  /** Undirected depth-first search */
  def udfs(vs: Seq[N]): Seq[N] = xdfsWith(vs, _.neighbors, _.vertex)

  /** Reverse depth-first search. Follows predecessors. */
  def rdfs(vs: Seq[N]): Seq[N] = xdfsWith(vs, _.predecessors, _.vertex)

  /** Finds the transitive closure of this graph. */
  def tclose: Graph[N,A,Unit] = {
    val ln = labNodes
    val newEdges = ln.flatMap {
      case LNode(u, _) => reachable(u).map(v => LEdge(u, v, ()))
    }
    empty.addNodes(ln).addEdges(newEdges)
  }

  /** Finds all the reachable nodes from a given node, using DFS */
  def reachable(v: N): Vector[N] = dff(Seq(v)).flatMap(_.flatten)

  /**
   * Depth-first forest. Follows successors of the given nodes. The result is
   * a vector of trees of nodes where each path in each tree is a path through the
   * graph.
   */
  def dff(vs: Seq[N]): Vector[Tree[N]] = dffWith(vs, _.vertex)

  /**
   * Depth-first forest. Follows successors of the given nodes. The result is
   * a vector of trees of results of passing the context of each node to the function `f`.
   */
  def dffWith[C](vs: Seq[N], f: Context[N,A,B] => C): Vector[Tree[C]] =
    xdffWith(vs, _.successors, f)

  /**
   * Generalized depth-first forest. Uses the function `d` to decide which nodes to
   * visit next.
   */
  def xdffWith[C](vs: Seq[N],
                  d: Context[N,A,B] => Seq[N],
                  f: Context[N,A,B] => C): Vector[Tree[C]] =
    xdfWith(vs, d, f)._1

  /**
   * Generalized depth-first forest. Uses the function `d` to decide which nodes to
   * visit next
   */
  def xdfWith[C](vs: Seq[N],
                 d: Context[N,A,B] => Seq[N],
                 f: Context[N,A,B] => C): (Vector[Tree[C]], Graph[N,A,B]) =
    if (vs.isEmpty || isEmpty) (Vector(), this)
    else decomp(vs.head) match {
      case Decomp(None, g) => g.xdfWith(vs.tail, d, f)
      case Decomp(Some(c), g) =>
        val (xs, g2) = g.xdfWith(d(c), d, f)
        val (ys, g3) = g.xdfWith(vs.tail, d, f)
        (Tree.node(f(c), xs.toStream) +: ys, g3)
    }

  import scala.collection.immutable.Queue

  /**
   * Breadth-first search (nodes ordered by distance)
   */
  def bfsnInternal[C](f: Context[N,A,B] => C, q: Queue[N]): Vector[C] =
    if (q.isEmpty || isEmpty)
      Vector()
    else {
      val (v, qp) = q.dequeue
      decomp(v) match {
        case Decomp(Some(c), g) =>
          f(c) +: g.bfsnInternal(f, qp enqueue c.successors)
        case Decomp(None, g) =>
          g.bfsnInternal(f, qp)
      }
    }

  /**
   * Breadth-first search from the given nodes. The result is a vector of
   * results of passing the context of each visited node to the function `f`.
   */
  def bfsnWith[C](f: Context[N,A,B] => C, vs: Seq[N]): Seq[C] =
    bfsnInternal(f, Queue(vs:_*))

  /**
   * Breadth-first search from the given nodes. The result is the successors of
   * `vs`, with immediate successors first.
   */
  def bfsn(vs: Seq[N]): Seq[N] =
    bfsnWith(_.vertex, vs)

  /**
   * Breadth-first search from the given node. The result is a vector of results
   * of passing the context of each visited to the function `f`.
   */
  def bfsWith[C](f: Context[N,A,B] => C, v: N): Seq[C] =
    bfsnInternal(f, Queue(v))

  /**
   * Breadth-first search from the given node. The result is ordered by distance
   * from the node `v`.
   */
  def bfs(v: N): Seq[N] =
    bfsWith(_.vertex, v)

  /**
   * Breadth-first search giving the distance of each node from the search nodes.
   */
  def leveln(vs: Seq[(N,Int)]): Seq[(N,Int)] =
    if (vs.isEmpty) Seq()
    else if (isEmpty) Seq()
    else {
      val (v,j) = vs.head
      decomp(v) match {
        case Decomp(Some(c), g) =>
          (v, j) +: g.leveln(vs.tail ++ c.successors.map(s => (s, (j+1))))
        case Decomp(None, g) =>
          g.leveln(vs.tail)
      }
    }

  /**
   * Breadth-first search giving the distance of each node from the node `v`.
   */
  def level(v: N): Seq[(N,Int)] =
    leveln(Seq((v, 0)))

  /** Breadth-first edges */
  def bfenInternal(q: Queue[Edge[N]]): Vector[Edge[N]] =
    if (q.isEmpty || isEmpty) Vector()
    else {
      val (Edge(u,v), qp) = q.dequeue
      decomp(v) match {
        case Decomp(Some(c), g) =>
          Edge(u,v) +: g.bfenInternal(qp.enqueue(c.outEdges.map(_.edge)))
        case Decomp(None, g) =>
          g.bfenInternal(qp)
      }
    }

  /**
   * Breadth-first search remembering predecessor information.
   * Gives transient edges starting from the targets of the given edges,
   * in breadth-first order.
   */
  def bfen(es: Seq[Edge[N]]): Seq[Edge[N]] =
    bfenInternal(Queue(es:_*))

  /**
   * Breadth-first search remembering predecessor information.
   * Gives transient edges in breadth-first order, starting from the given node.
   */
  def bfe(v: N): Seq[Edge[N]] =
    bfen(Seq(Edge(v,v)))

  /**
   * Utility function for breadth-first search tree.
   */
  def bf(q: Queue[Path[N]]): RTree[N] =
    if (q.isEmpty || isEmpty) Stream()
    else {
      val (p, qp) = q.dequeue
      if (p.isEmpty) Stream(p)
      else decomp(p.head) match {
        case Decomp(Some(c), g) => p #:: g.bf(qp.enqueue(c.successors.map(_ +: p)))
        case Decomp(None, g) => g bf qp
      }
    }

  /**
   * Breadth-first search tree. The result is a list of paths through the graph
   * from the given vertex, in breadth-first order.
   */
  def bft(v: N): RTree[N] =
    bf(Queue(Vector(v)))

  /** The shortest path from vertex `s` to vertex `t` */
  def esp(s: N, t: N): Option[Path[N]] =
    getPath(t, bft(s))

  /** Utility function for labeled breadth-first search tree */
  def lbf(q: Queue[LPath[N,B]]): LRTree[N,B] =
    if (q.isEmpty || isEmpty) Stream()
    else {
      val (p, qp) = q.dequeue
      if (p.isEmpty) Stream(p)
      else decomp(p.head._2) match {
        case Decomp(Some(c), g) => p #:: g.lbf(qp.enqueue(c.outs.map(_ +: p)))
        case Decomp(None, g) => g lbf qp
      }
    }

  /** Breadth-first search tree with labeled paths */
  def lbft(v: N): LRTree[N,B] = {
    val out = outEdges(v)
    if (out.isEmpty) Stream(Vector())
    else {
      val LEdge(vp, _, l) = out.head
      lbf(Queue(Vector(l -> vp)))
    }
  }

  /** Shortest path from vertex `s` to vertex `t`, with labels */
  def lesp(s: N, t: N): Option[LPath[N,B]] =
    getLPath(t, lbft(s))

  /**
   * Find starting and ending nodes. An ending node `n` in graph `g` has `f(g,n)`
   * containing no nodes other than `n`.
   */
  def endNode(f: (Graph[N,A,B], N) => Seq[N], n: N): Boolean = {
    val ns = f(this, n)
    ns.isEmpty || ns.toSet == Set(n)
  }

  /** Find all nodes that match the given ending criteria. */
  def endBy(f: (Graph[N,A,B], N) => Seq[N]): Seq[N] =
    nodes.filter(n => endNode(f, n))

  /** Find the roots of the graph. A root is a node which has no incoming edges. */
  def roots: Set[N] = endBy(_ predecessors _).toSet

  /** Find the leaves of the graph. A leaf is a node which as no outgoing edges. */
  def leaves: Set[N] = endBy(_ successors _).toSet

  override def toString: String =
    nodes.foldLeft("") { (s, n) => decomp(n) match {
      case Decomp(Some(Context(_, v, l, ss)), _) =>
        val sss = ss.mkString(",")
        val n = if (s.isEmpty) "" else "\n"
        s"$s$n$v:$l->[$sss]"
      case _ => sys.error("Unpossible!")
    }}

}

