package com.intel.graphs

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
 * An implementation of an inductive graph using `Map`.
 * Nodes are labeled with `A`, and edges are labeled with `B`.
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
   * Merge the given context into the graph. The context consists of a vertex, its label,
   * its successors, and its predecessors.
   */
  def &(ctx: Context[N,A,B]): Graph[N,A,B] = {
    val Context(p, v, l, s) = ctx
    val g1 = rep + (v -> GrContext(fromAdj(p), l, fromAdj(s)))
    val g2 = addSucc(g1, v, p)
    val g3 = addPred(g2, v, s)
    Graph(g3)
  }

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
        GrContext(ps, lp, ss.insertWith(w, Vector(l))(_ ++ _))
      }
      def addPredP(p: GrContext[N,A,B]) = {
        val GrContext(ps, lp, ss) = p
        GrContext(ps.insertWith(v, Vector(l))(_ ++ _), lp, ss)
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
  def decompAny: GDecomp[N,A,B] = labNodes match {
    case Vector() => sys.error("Cannot decompose an empty graph")
    case vs =>
      val Decomp(Some(c), g) = decomp(vs.head.vertex)
      GDecomp(c, g)
  }

  /** The number of nodes in this graph */
  def countNodes: Int = rep.size

  /** A list of all the edges in the graph and their labels */
  def labEdges: Vector[LEdge[N,B]] = for {
    (node, GrContext(_, _, s)) <- rep.toVector
    (next, labels) <- s.toVector
    label <- labels
  } yield LEdge(node, next, label)

  /** A list of all the edges in the graph */
  def edges = labEdges.map { case LEdge(v, w, _) => Edge(v, w) }

  /** Fold a function over the graph */
  def fold[C](u: C)(f: (Context[N,A,B], C) => C): C = {
    if (isEmpty) u else {
      val GDecomp(c, g) = decompAny
      f(c, g.fold(u)(f))
    }
  }

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

  override def toString: String =
    nodes.foldLeft("") { (s, n) => decomp(n) match {
      case Decomp(Some(Context(_, v, l, ss)), _) =>
        val sss = ss.mkString(",")
        s"$v:$l->[$sss]\n$s"
      case _ => sys.error("Unpossible!")
    }}
}

