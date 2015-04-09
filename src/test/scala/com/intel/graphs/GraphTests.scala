package com.intel.graphs

import org.scalacheck._
import org.scalacheck.Prop._
import scalaz._
import Scalaz._


object GraphTests extends Properties("Graph") {
  type N = Byte
  import GraphGen._

  property("Adding a node to a graph yields a graph that contains that node") =
    forAll { (g: Graph[N,Int,Int], n: LNode[N,Int]) =>
      (g addNode n) contains n.vertex
    }

  property("Adding a node and removing it again yields a graph that doesn't contain that node") =
    forAll { (g: Graph[N,Int,Int], n: LNode[N,Int]) =>
      !g.addNode(n).removeNode(n.vertex).contains(n.vertex)
    }

  property("Adding an edge should yield a map that contains that edge") =
    forAll { (g: Graph[N,Int,Int], e: LEdge[N,Int]) =>
      g.addNode(LNode(e.from, 0)).addNode(LNode(e.to, 0)).addEdge(e).findEdge(e.edge).isDefined
    }

  property("Adding an edge and removing it again yields a graph that doesn't contain that edge") =
    forAll { (g: Graph[N,Int,Int], e: LEdge[N,Int]) =>
      g.addNode(LNode(e.from, 0)).
      addNode(LNode(e.to, 0)).
      addEdge(e).
      removeLEdge(e).
      findEdge(e.edge) != Some(e)
    }

  property("A graph constructed from nodes and edges should contain those nodes and edges") =
    forAll { (ns: List[N], es: List[LEdge[N,Int]], i: Int) =>
      implicit val NO = Order[LNode[N,Int]].toScalaOrdering
      implicit val EO = Order[LEdge[N,Int]].toScalaOrdering
      val nns = ns.toSet.toList.map((b: N) => LNode(b, i)).sorted
      val ees = es.toSet.toList.filter((x: LEdge[N,Int]) =>
                  ns.contains(x.from) && ns.contains(x.to)
                ).sorted
      val g = safeMkGraph(nns, ees)
      nns == g.labNodes.sorted && ees == g.labEdges.sorted
    }

  property("A graph constructed from another graph's nodes and edges should be the same graph") =
    forAll { (ns: List[LNode[N,Int]], es: List[LEdge[N,Int]]) =>
      val g = safeMkGraph(ns, es)
      val nns = g.labNodes
      val ees = g.labEdges
      g == safeMkGraph(nns, ees)
    }

  property("Adding an edge between two existing nodes should succeed") =
    forAll { (n1: LNode[N,String], n2: LNode[N,String], el: String) =>
      !empty[N, String, String].
      addNode(n1).
      addNode(n2).
      addEdge(LEdge(n1.vertex, n2.vertex, el)).isEmpty
    }

  property("DFS should find all descendants of a node") =
    forAll { (ns: List[LNode[N,Int]], l: Int) =>
      val g = mkGraph(ns,
                if (ns.isEmpty) List()
                else ns.zip(ns.tail).map {
                  case (a, b) => LEdge(a.vertex, b.vertex, l)
                })
      ns.isEmpty || g.dfs(Seq(ns.head.vertex)).toSet == ns.map(_.vertex).toSet
    }

  property("RDFS should find all ancestors of a node") =
    forAll { (ns: List[LNode[N,Int]], l: Int) =>
      val g = mkGraph(ns,
                if (ns.isEmpty) List()
                else ns.zip(ns.tail).map {
                  case (a, b) => LEdge(a.vertex, b.vertex, l)
                })
      ns.isEmpty || g.rdfs(Seq(ns.last.vertex)).toSet == ns.map(_.vertex).toSet
    }

  property("The union of two graphs should contain all the edges and nodes of both graphs") =
    forAll { (g1: Graph[N,Int,Int], g2: Graph[N,Int,Int]) =>
      val u = g1 union g2
      g1.edges.forall(u.edges contains _)
      g1.nodes.forall(u.nodes contains _)
    }

  property("Graphs with union form a monoid") =
    forAll { (g1: Graph[N,Int,Int], g2: Graph[N,Int,Int], g3: Graph[N,Int,Int]) =>
      graphMonoid.monoidLaw.associative(g1, g2, g3)
      graphMonoid.monoidLaw.leftIdentity(g1)
      graphMonoid.monoidLaw.rightIdentity(g1)
    }

  property("Union is commutative (ignoring labels)") =
    forAll { (g1: Graph[N,Int,Int], g2: Graph[N,Int,Int]) =>
      val u1 = g1 union g2
      val u2 = g2 union g1
      implicit val N = Order[Edge[N]].toScalaOrdering
      u1.edges.sorted == u2.edges.sorted && u1.nodes.sorted == u2.nodes.sorted
    }
}
