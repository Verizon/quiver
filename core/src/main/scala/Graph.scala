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

case class Edge[N](from: N, to: N)

case class LEdge[N,A](from: N, to: N, label: A) {
  def map[B](f: A => B): LEdge[N,B] = LEdge(from, to, f(label))
  def edge: Edge[N] = Edge(from, to)
}

case class LNode[N,A](vertex: N, label: A) {
  def map[B](f: A => B): LNode[N,B] = LNode(vertex, f(label))
}

/**
 * An implementation of an inductive graph where nodes of type
 * `N `are labeled with `A`, and edges are labeled with `B`.
 *
 * @groupname basic Basic Graph Operations
 * @groupprio basic 10
 *
 * @groupname decomposition Graph Decomposition
 * @groupdesc decomposition Structural decomposition of a graph
 * @groupprio decomposition 20
 *
 * @groupname composition Graph Composition
 * @groupprio composition 30
 *
 * @groupname mutation Deletion and Modification
 * @groupprio mutation 40
 *
 * @groupname projection Projection
 * @groupprio projection 45
 * @groupdesc projection Functions for extracting global information about a graph
 *
 * @groupname inspection Graph Inspection
 * @groupprio inspection 50
 * @groupdesc inspection Functions for extracting information about individual nodes and edges in a graph
 *
 * @groupname foldmaps Folds and Maps
 * @groupprio foldmaps 60
 *
 * @groupname filters Subgraphs
 * @groupprio filters 65
 *
 * @groupname dfs Depth-First Search
 * @groupdesc dfs Algorithms for depth-first traversal.
 * @groupprio dfs 70
 *
 * @groupname bfs Breadth-First Search
 * @groupdesc bfs Algorithms for breadth-first traversal.
 * @groupprio bfs 80
 *
 * @groupname ends Roots and Leaves
 * @groupdesc ends Queries to find starting and ending nodes, roots, and leaves.
 * @groupprio ends 90
 *
 * @groupname classification Graph Classification
 * @groupdesc classification Classification functions on graphs
 * @groupprio classification 100
 */
case class Graph[N,A,B](rep: GraphRep[N,A,B]) {
  /**
   * Check if the graph is empty
   * @group projection
   */
  def isEmpty = rep.isEmpty

  /**
   * Reverse the direction of all edges
   * @group basic
   */
  def reverse: Graph[N,A,B] = gmap {
    case Context(p, v, l, s) => Context(s, v, l, p)
  }

  /**
   * Make the graph undirected, ensuring that every edge has an inverse.
   * This takes edge labels into account when considering whether two edges are
   * equal.
   *
   * @group basic
   */
  def undir: Graph[N,A,B] = gmap {
    case Context(p, v, l, s) =>
      val ps = (p ++ s).toSet.toVector
      Context(ps, v, l, ps)
  }

  /**
   * Erase all labels in the graph
   *
   * @group basic
   */
  def unlabel: Graph[N,Unit,Unit] = gmap {
    case Context(p, v, _, s) =>
      def ul[V,L](a: Adj[V,L]): Adj[V,Unit] =
        a.map { case (_, n) => ((), n) }
      Context(ul(p), v, (), ul(s))
  }

  /**
   * Returns a context focused on the given node, if present,
   * and the graph with that node removed.
   *
   * @group decomposition
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

  /**
   * Returns a context focused on two nodes designated "first" and "last",
   * if present, and the graph with those nodes removed.
   *
   * @group decomposition
   */
  def bidecomp(first: N, last: N): Option[BiDecomp[N,A,B]] = {
    val Decomp(c1, r1) = decomp(first)
    val Decomp(c2, _) = decomp(last)
    for {
      x <- c1
      y <- c2
    } yield BiDecomp(x, y, r1.decomp(y.vertex).rest)
  }

  /**
   * Decompose this graph into the context for an arbitrarily chosen node
   * and the rest of the graph.
   * @group decomposition
   */
  def decompAny: Decomp[N,A,B] =
    if (isEmpty)
      Decomp(None, this)
    else
      decomp(rep.head._1)

  /**
   * Embed the given context in the graph. If the context's vertex is already in
   * the graph, removes the old context from the graph first. This operation is
   * the deterministic inverse of `decomp` and obeys the following laws:
   *
   * `(g & c) decomp c.vertex == Decomp(Some(c), g)`
   * `(g decomp c.vertex).rest & c == (g & c)`
   *
   * @group composition
   */
  def embed(ctx: Context[N,A,B]): Graph[N,A,B] = {
    val Context(p, v, l, s) = ctx
    val r = decomp(v).rest.rep
    val g1 = r + (v -> GrContext(fromAdj(p), l, fromAdj(s)))
    val g2 = addSucc(g1, v, p)
    val g3 = addPred(g2, v, s)
    Graph(g3)
  }

  /**
   * Alias for `embed`
   * @group composition
   */
  def &(ctx: Context[N,A,B]): Graph[N,A,B] = embed(ctx)

  /**
   * Add a node to this graph. If this node already exists with a different label,
   * its label will be replaced with this new one.
   * @group composition
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
   * @group composition
   */
  def addEdge(e: LEdge[N,B]): Graph[N,A,B] =
    safeAddEdge(e,
      sys.error(s"Can't add edge $e since the source and target nodes don't both exist in the graph."))

  /**
   * Add an edge to this graph. If the source and target nodes don't exist in this graph,
   * return the given `failover` graph.
   * @group composition
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

  /**
   * Add multiple nodes to this graph
   * @group composition
   */
  def addNodes(vs: Seq[LNode[N,A]]): Graph[N,A,B] =
    vs.foldLeft(this)(_ addNode _)

  /**
   * Add multiple edges to this graph
   * @group composition
   */
  def addEdges(es: Seq[LEdge[N,B]]): Graph[N,A,B] =
    es.foldLeft(this)(_ addEdge _)

  /**
   * Add multiple edges to this graph, ignoring edges whose source and target nodes
   * don't already exist in the graph.
   * @group composition
   */
  def safeAddEdges(es: Seq[LEdge[N,B]]): Graph[N,A,B] =
    es.foldLeft(this)(_ safeAddEdge _)

  /**
   * Adds all the nodes and edges from one graph to another.
   * @group composition
   */
  def union(g: Graph[N,A,B]): Graph[N,A,B] =
    addNodes(g.labNodes).addEdges(g.labEdges)

  /**
   * Remove a node from this graph
   * @group mutation
   */
  def removeNode(v: N): Graph[N,A,B] =
    removeNodes(Seq(v))

  /**
   * Remove multiple nodes from this graph
   * @group mutation
   */
  def removeNodes(vs: Seq[N]): Graph[N,A,B] =
    if (vs.isEmpty) this else decomp(vs.head).rest.removeNodes(vs.tail)

  /**
   * Remove an edge from this graph
   * @group mutation
   */
  def removeEdge(e: Edge[N]): Graph[N,A,B] = decomp(e.from) match {
    case Decomp(None, _) => this
    case Decomp(Some(Context(p, v, l, s)), gp) =>
      gp & Context(p, v, l, s.filter { case (_, n) => n != e.to })
  }

  /**
   * Remove an edge from this graph only if the label matches
   * @group mutation
   */
  def removeLEdge(e: LEdge[N,B]): Graph[N,A,B] = decomp(e.from) match {
    case Decomp(None, _) => this
    case Decomp(Some(Context(p, v, l, s)), gp) =>
      gp & Context(p, v, l, s.filter { case (x, n) => (x != e.label) || (n != e.to) })
  }

  /**
   * Remove multiple edges from this graph
   * @group mutation
   */
  def removeEdges(es: Seq[Edge[N]]): Graph[N,A,B] =
    es.foldLeft(this)(_ removeEdge _)

  /**
   * Replace an edge with a new one
   * @group mutation
   */
  def updateEdge(e: LEdge[N,B]): Graph[N,A,B] =
    removeEdge(Edge(e.from, e.to)).addEdge(e)

  /**
   * Update multiple edges
   * @group mutation
   */
  def updateEdges(es: Seq[LEdge[N,B]]): Graph[N,A,B] =
    es.foldLeft(this)(_ updateEdge _)

  /**
   * Replace a node with a new one
   * @group mutation
   */
  def updateNode(n: LNode[N,A]): Graph[N,A,B] =
    decomp(n.vertex) match {
      case Decomp(Some(Context(p, v, l, s)), rest) =>
        rest & Context(p, n.vertex, n.label, s)
      case _ => this
    }

  /**
   * Update multiple nodes
   * @group mutation
   */
  def updateNodes(ns: Seq[LNode[N,A]]): Graph[N,A,B] =
    ns.foldLeft(this)(_ updateNode _)

  /**
   * A list of all the nodes in the graph and their labels
   * @group projection
   */
  def labNodes: Vector[LNode[N,A]] =
    rep.toVector map { case (node, GrContext(_, label, _)) => LNode(node, label) }

  /**
   * A list of all the nodes in the graph
   * @group projection
   */
  def nodes: Vector[N] = labNodes.map(_.vertex)

  /**
   * A list of all the edges in the graph and their labels
   * @group projection
   */
  def labEdges: Vector[LEdge[N,B]] = for {
    (node, GrContext(_, _, s)) <- rep.toVector
    (next, labels) <- s.toVector
    label <- labels
  } yield LEdge(node, next, label)

  /**
   * A list of all the edges in the graph
   * @group projection
   */
  def edges: Vector[Edge[N]] = labEdges.map { case LEdge(v, w, _) => Edge(v, w) }

  /**
   * Get all the contexts in the graph, as a vector. Note that the resulting contexts may
   * overlap, in the sense that successive contexts in the result will contain vertices
   * from previous contexts as well.
   * @group projection
   */
  def contexts: Vector[Context[N,A,B]] =
    rep.map { case (v, c) => c.toContext(v) }.toVector

  /**
   * Project out (non-overlapping) contexts for which the given property is true.
   * Note that the contexts will not overlap, in the sense that successive contexts in
   * the result will not contain the vertices at the focus of previous contexts.
   * @group projection
   */
  def select(p: Context[N,A,B] => Boolean): Vector[Context[N,A,B]] =
    fold(Vector[Context[N,A,B]]())((c, cs) => if (p(c)) c +: cs else cs)

  /**
   * Get all the contexts for which the given property is true. Note that the resulting
   * contexts may overlap, in the sense that successive contexts in the result may contain
   * vertices from previous contexts.
   * @group projection
   */
  def selectAll(p: Context[N,A,B] => Boolean): Vector[Context[N,A,B]] =
    rep.flatMap { case (v, c) => Some(c.toContext(v)) filter p }.toVector

  /**
   * The number of nodes in this graph
   * @group projection
   */
  def countNodes: Int = rep.size


  /**
   * Fold a function over the graph. Note that each successive context received by the
   * function will not contain vertices at the focus of previously received contexts.
   * The first context will be an arbitrarily chosen context of this graph, but the
   * next context will be arbitrarily chosen from a graph with the first context removed,
   * and so on.
   * @group foldmaps
   */
  def fold[C](u: C)(f: (Context[N,A,B], C) => C): C = {
    val Decomp(c, g) = decompAny
    c.map(x => f(x, g.fold(u)(f))) getOrElse u
  }

  /**
   * Fold a function over all the contexts in the graph. Each context received by the
   * function will be an arbitrarily chosen context of this graph. Contrary to `fold`,
   * this visits every node of the graph and gives you all incoming and outgoing adjacencies
   * for each node.
   * @group foldmaps
   */
  def foldAll[C](u: C)(f: (Context[N,A,B], C) => C): C =
    rep.foldLeft(u) { case (acc, (v, ctx)) => f(ctx toContext v, acc) }

  /**
   * Map a function over the graph
   * @group foldmaps
   */
  def gmap[C,D](f: Context[N,A,B] => Context[N,C,D]): Graph[N,C,D] =
    Graph(rep.map { case (k, v) => k -> f(v.toContext(k)).toGrContext })

  /**
   * Map a function over the node labels in the graph
   * @group foldmaps
   */
  def nmap[C](f: A => C): Graph[N,C,B] =
    Graph(rep.mapValues { case GrContext(ps, a, ss) => GrContext(ps, f(a), ss) })

  /**
   * Map a function over the edge labels in the graph
   * @group foldmaps
   */
  def emap[C](f: B => C): Graph[N,A,C] =
    Graph(rep.mapValues {
      case GrContext(ps, a, ss) =>
        GrContext(ps mapValues (_ map f), a, ss mapValues (_ map f))
    })

  /**
   * Map over the unique node identifiers in the graph
   * @group foldmaps
   */
  def vmap[M](f: N => M): Graph[M,A,B] =
    Graph(rep.map {
      case (k, GrContext(ps, a, ss)) => f(k) -> GrContext(ps mapKeys f, a, ss mapKeys f)
    })

  /**
   * The subgraph containing only edges that match the property
   * @group filters
   */
  def efilter(f: LEdge[N,B] => Boolean): Graph[N,A,B] =
    fold(quiver.empty[N,A,B]) {
      case (Context(p, v, l, s), g) =>
        val pp = p.filter { case (b, u) => f(LEdge(u,v,b)) }
        val ss = s.filter { case (b, w) => f(LEdge(v,w,b)) }
        Context(pp, v, l, ss) & g
    }

  /**
   * The subgraph containing only edges whose labels match the property
   * @group filters
   */
  def elfilter(f: B => Boolean): Graph[N,A,B] =
    efilter { case LEdge(_,_,b) => f(b) }

  /**
   * Build a graph out of a partial map of the contexts in this graph.
   * @group filters
   */
  def filterMap[C,D](f: Context[N,A,B] => Option[Context[N,C,D]]): Graph[N,C,D] =
    fold(quiver.empty[N,C,D]) { (c, g) => f(c).fold(g)(g & _) }

  /**
   * The subgraph containing only labelled nodes that match the property
   * @group filters
   */
  def labnfilter(p: LNode[N,A] => Boolean): Graph[N,A,B] =
    removeNodes(labNodes.filter(!p(_)).map(_.vertex))

  /**
   * The subgraph containing only nodes that match the property
   * @group filters
   */
  def nfilter(p: N => Boolean): Graph[N,A,B] =
    labnfilter(n => p(n.vertex))

  /**
   * The subgraph containing only nodes whose labels match the property
   * @group filters
   */
  def labfilter(p: A => Boolean): Graph[N,A,B] =
    labnfilter(n => p(n.label))

  /**
   * The subgraph containing only the given nodes
   * @group filters
   */
  def subgraph(vs: Seq[N]): Graph[N,A,B] = {
    val s = vs.toSet
    nfilter(s contains _)
  }

  /**
   * Returns true if the given node is in the graph, otherwise false
   * @group inspection
   */
  def contains(v: N): Boolean = decomp(v) match {
    case Decomp(Some(_), _) => true
    case _ => false
  }

  /**
   * Find the context for the given node. Causes an error if the node is not
   * present in the graph.
   * @group inspection
   */
  def context(v: N): Context[N,A,B] =
    decomp(v).ctx.getOrElse(sys.error(s"Node $v is not present in the graph"))

  /**
   * All the inbound links of the given node, including self-edges
   * @group inspection
   */
  def ins(v: N): Adj[N,B] =
    context(v).ins

  /**
   * All the outbound links of the given node, including self-edges
   * @group inspection
   */
  def outs(v: N): Adj[N,B] =
    context(v).outs

  /**
   * Find the label for a node
   * @group inspection
   */
  def label(v: N): Option[A] =
    decomp(v).ctx.map(_.label)

  /**
   * Find the neighbors of a node
   * @group inspection
   */
  def neighbors(v: N): Vector[N] = {
    val Context(p, _, _, s) = context(v)
    (p ++ s).map(_._2)
  }

  /**
   * Find all nodes that have a link from the given node
   * @group inspection
   */
  def successors(v: N): Vector[N] =
    outs(v).map(_._2)

  /**
   * Find all nodes that have a link to the given node
   * @group inspection
   */
  def predecessors(v: N): Vector[N] =
    ins(v).map(_._2)

  /**
   * Find all outbound edges for the given node
   * @group inspection
   */
  def outEdges(v: N): Vector[LEdge[N,B]] =
    outs(v).map { case (l, w) => LEdge(v, w, l) }

  /**
   * Find all inbound edges for the given node
   * @group inspection
   */
  def inEdges(v: N): Vector[LEdge[N,B]] =
    ins(v).map { case (l, w) => LEdge(v, w, l) }

  /**
   * The number of outbound edges from the given node
   * @group inspection
   */
  def outDegree(v: N): Int =
    outs(v).length

  /**
   * The number of inbound edges from the given node
   * @group inspection
   */
  def inDegree(v: N): Int =
    ins(v).length

  /**
   * The number of connections to and from the given node
   * @group inspection
   */
  def degree(v: N): Int = {
    val Context(p, _, _, s) = context(v)
    p.length + s.length
  }

  /**
   * Find an edge between two nodes
   * @group inspection
   */
  def findEdge(e: Edge[N]): Option[LEdge[N,B]] =
    labEdges.find(c => c.from == e.from && c.to == e.to)

  /**
   * Generalized depth-first search.
   * @group dfs
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
   * @group dfs
   */
  def dfsWith[C](vs: Seq[N], f: Context[N,A,B] => C): Seq[C] =
    xdfsWith(vs, _.successors, f)

  /**
   * Forward depth-first search.
   * @group dfs
   */
  def dfs(vs: Seq[N]): Seq[N] = dfsWith(vs, _.vertex)

  /**
   * Undirected depth-first search
   * @group dfs
   */
  def udfs(vs: Seq[N]): Seq[N] = xdfsWith(vs, _.neighbors, _.vertex)

  /**
   * Reverse depth-first search. Follows predecessors.
   * @group dfs
   */
  def rdfs(vs: Seq[N]): Seq[N] = xdfsWith(vs, _.predecessors, _.vertex)

  /**
   * Finds the transitive closure of this graph.
   * @group dfs
   */
  def tclose: Graph[N,A,Unit] = {
    val ln = labNodes
    val newEdges = ln.flatMap {
      case LNode(u, _) => reachable(u).map(v => LEdge(u, v, ()))
    }
    empty.addNodes(ln).addEdges(newEdges)
  }

  /**
   * Finds all the reachable nodes from a given node, using DFS
   * @group dfs
   */
  def reachable(v: N): Vector[N] = dff(Seq(v)).flatMap(_.flatten)

  /**
   * Depth-first forest. Follows successors of the given nodes. The result is
   * a vector of trees of nodes where each path in each tree is a path through the
   * graph.
   * @group dfs
   */
  def dff(vs: Seq[N]): Vector[Tree[N]] = dffWith(vs, _.vertex)

  /**
   * Depth-first forest. Follows successors of the given nodes. The result is
   * a vector of trees of results of passing the context of each node to the function `f`.
   * @group dfs
   */
  def dffWith[C](vs: Seq[N], f: Context[N,A,B] => C): Vector[Tree[C]] =
    xdffWith(vs, _.successors, f)

  /**
   * Generalized depth-first forest. Uses the function `d` to decide which nodes to
   * visit next.
   * @group dfs
   */
  def xdffWith[C](vs: Seq[N],
                  d: Context[N,A,B] => Seq[N],
                  f: Context[N,A,B] => C): Vector[Tree[C]] =
    xdfWith(vs, d, f)._1

  /**
   * Generalized depth-first forest. Uses the function `d` to decide which nodes to
   * visit next
   * @group dfs
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
   * Utility function for breadth-first search (nodes ordered by distance)
   * @group bfs
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
   * @group bfs
   */
  def bfsnWith[C](f: Context[N,A,B] => C, vs: Seq[N]): Seq[C] =
    bfsnInternal(f, Queue(vs:_*))

  /**
   * Breadth-first search from the given nodes. The result is the successors of
   * `vs`, with immediate successors first.
   * @group bfs
   */
  def bfsn(vs: Seq[N]): Seq[N] =
    bfsnWith(_.vertex, vs)

  /**
   * Breadth-first search from the given node. The result is a vector of results
   * of passing the context of each visited to the function `f`.
   * @group bfs
   */
  def bfsWith[C](f: Context[N,A,B] => C, v: N): Seq[C] =
    bfsnInternal(f, Queue(v))

  /**
   * Breadth-first search from the given node. The result is ordered by distance
   * from the node `v`.
   * @group bfs
   */
  def bfs(v: N): Seq[N] =
    bfsWith(_.vertex, v)

  /**
   * Breadth-first search giving the distance of each node from the search nodes.
   * @group bfs
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
   * @group bfs
   */
  def level(v: N): Seq[(N,Int)] =
    leveln(Seq((v, 0)))

  /**
   * Breadth-first search remembering predecessor information.
   * Gives transient edges starting from the targets of the given edges,
   * in breadth-first order.
   * @group bfs
   */
  def bfen(es: Seq[Edge[N]]): Seq[Edge[N]] =
    bfenInternal(Queue(es:_*))

  /**
   * Utility function for breadth-first search, remembering predecessor information.
   * @group bfs
   */
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
   * Gives transient edges in breadth-first order, starting from the given node.
   * @group bfs
   */
  def bfe(v: N): Seq[Edge[N]] =
    bfen(Seq(Edge(v,v)))

  /**
   * Utility function for breadth-first search trees.
   * @group bfs
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
   * @group bfs
   */
  def bft(v: N): RTree[N] =
    bf(Queue(Vector(v)))

  /**
   * The shortest path from vertex `s` to vertex `t`
   * @group bfs
   */
  def esp(s: N, t: N): Option[Path[N]] =
    getPath(t, bft(s))

  /**
   * Utility function for labeled breadth-first search tree
   * @group bfs
   */
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

  /**
   * Breadth-first search tree with labeled paths
   * @group bfs
   */
  def lbft(v: N): LRTree[N,B] = {
    val out = outEdges(v)
    if (out.isEmpty) Stream(Vector())
    else {
      val LEdge(vp, _, l) = out.head
      lbf(Queue(Vector(l -> vp)))
    }
  }

  /**
   * Shortest path from vertex `s` to vertex `t`, with labels
   * @group bfs
   */
  def lesp(s: N, t: N): Option[LPath[N,B]] =
    getLPath(t, lbft(s))

  /**
   * Check if the given node is an end node according to the given criteria.
   * An ending node `n` in graph `g` has `f(g,n)` containing no nodes other than `n`.
   * @group ends
   */
  def endNode(f: (Graph[N,A,B], N) => Seq[N], n: N): Boolean = {
    val ns = f(this, n)
    ns.isEmpty || ns.toSet == Set(n)
  }

  /**
   * Find all nodes that match the given ending criteria.
   * @group ends
   */
  def endBy(f: (Graph[N,A,B], N) => Seq[N]): Seq[N] =
    nodes.filter(n => endNode(f, n))

  /**
   * Find the roots of the graph. A root is a node which has no incoming edges.
   * @group ends
   */
  def roots: Set[N] = endBy(_ predecessors _).toSet

  /**
   * Check if the given node is a root of this graph
   * @group ends
   */
  def isRoot(n: N): Boolean =
    endNode(_ predecessors _, n)

  /**
   * Find the leaves of the graph. A leaf is a node which as no outgoing edges.
   * @group ends
   */
  def leaves: Set[N] = endBy(_ successors _).toSet

  /**
   * Check if the given node is a leaf of this graph
   * @group ends
   */
  def isLeaf(n: N): Boolean =
    endNode(_ successors _, n)

  /**
   * Check if this graph has any loops, which connect a node to itself.
   * @group classification
   */
  def hasLoop: Boolean =
    !select(c => c.successors contains c.vertex).isEmpty

  /**
   * Check if this graph has multiple edges connecting any two nodes
   * @group classification
   */
  def hasMulti: Boolean =
    !select { c =>
      val succs = c.successors
      succs.toSet.size != succs.size
    }.isEmpty

  /**
   * Check whether this graph is simple. A simple graph has no loops and no multi-edges.
   * @group classification
   */
  def isSimple: Boolean = !hasMulti && !hasLoop

  override def toString: String =
    nodes.foldLeft("") { (s, n) => decomp(n) match {
      case Decomp(Some(Context(_, v, l, ss)), _) =>
        val sss = ss.mkString(",")
        val n = if (s.isEmpty) "" else "\n"
        s"$s$n$v:$l->[$sss]"
      case _ => sys.error("Unpossible!")
    }}

}

