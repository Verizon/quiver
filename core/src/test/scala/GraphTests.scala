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

import org.scalacheck._
import org.scalacheck.Prop._
import scalaz._
import Scalaz._
import scalaz.scalacheck.ScalazProperties._

object GraphTests extends Properties("Graph") {
  type N = Byte
  import GraphGen._

  def connect(ns: List[LNode[N,Int]], l: Int): Graph[N,Int,Int] =
    mkGraph(ns, if (ns.isEmpty) List()
                else ns.zip(ns.tail).map {
                  case (a, b) => LEdge(a.vertex, b.vertex, l)
                })

  property("embed and decomp are inverses of each other") =
    forAll { (g: Graph[N,Int,Int], c: Context[N,Int,Int]) =>
      (((g & c) decomp c.vertex) match {
        case Decomp(Some(c2), g2) => (g2 & c2) == (g & c)
        case _ => false
      }) &&
      (((g decomp c.vertex).rest & c) == (g & c))
    }

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
      val g = connect(ns, l)
      ns.isEmpty || g.dfs(Seq(ns.head.vertex)).toSet == ns.map(_.vertex).toSet
    }

  property("RDFS should find all ancestors of a node") =
    forAll { (ns: List[LNode[N,Int]], l: Int) =>
      val g = connect(ns, l)
      ns.isEmpty || g.rdfs(Seq(ns.last.vertex)).toSet == ns.map(_.vertex).toSet
    }

  property("BFS should find all descendants of a node") =
    forAll { (ns: List[LNode[N,Int]], l: Int) =>
      val g = connect(ns, l)
      val bfs = if (ns.isEmpty) Set() else g.bfs(ns.head.vertex).toSet
      ns.isEmpty || g.bfs(ns.head.vertex).toSet == ns.map(_.vertex).toSet
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

  property("Folding on contexts equals folding on nodes and edges") =
    forAll { (g: Graph[N,Int,Int]) =>
      (g.labNodes.map(_.label).sum, g.labEdges.map(_.label).sum) == g.fold((0,0)) {
        case (Context(ins, v, a, outs), (accn, acce)) =>
          (a + accn, (ins ++ outs).map(_._1).sum + acce)
      }
    }

  property("The roots of a graph have no incoming edges from other nodes") = forAll {
    (ns: List[LNode[N,Int]], es: List[LEdge[N,Int]], i: Int) =>
      val g = safeMkGraph(ns, es)
      val vs = ns.map(_.vertex).toSet
      val roots = vs.filterNot(n => es.find { e =>
        e.to === n && vs.contains(e.from) && (e.from /== n)
      }.isDefined)
      s"Found ${g.roots} expected ${roots}" |: g.roots === roots
    }

  property("The leaves of a graph have no outgoing edges to other nodes") = forAll {
    (ns: List[LNode[N,Int]], es: List[LEdge[N,Int]], i: Int) =>
      val g = safeMkGraph(ns, es)
      val vs = ns.map(_.vertex).toSet
      val leaves = vs.filterNot(n => es.find { e =>
        e.from === n && vs.contains(e.to) && (e.to /== n)
      }.isDefined)
      s"Found ${g.leaves} expected ${leaves}" |: g.leaves === leaves
    }

  property("The shortest path should be the cheapest path with constant cost") = forAll {
    (tpg: (Graph[N,Int,Int], LNode[N,Int], LNode[N,Int])) =>
      val graph = tpg._1
      val from = tpg._2.vertex
      val to = tpg._3.vertex
      val sPath = graph.lesp(from, to)
      val cPath = graph.cheapestPath[Int](from, to, (f: LNode[N,Int], l: Int, t: LNode[N,Int]) => 1)
      s"Paths different: cheapest = $cPath, shortest = $sPath" |: sPath === cPath
    }
  import GDecomp._

  property("GDecomp is a lawful comonad") = comonad.laws[({type λ[α] = GDecomp[Int,α,Int]})#λ]

  property("The shortest path through a graph should be the shortest path between all subpaths") = forAll {
    (tpg: (Graph[N,Int,Int], LNode[N, Int], LNode[N, Int])) =>
    val start = tpg._2
    val end = tpg._3
    val graph = tpg._1
    (for {
      path <- graph.esp(start.vertex, end.vertex).toVector
      subpath <- (1 until path.size).flatMap(path.sliding).toVector
      shortest = (subpath.headOption |@| subpath.lastOption).apply(graph.esp)
    } yield shortest.nonEmpty && shortest.get.nonEmpty && shortest.get.get == subpath).forall(identity)
  }

  property("The shortest labelled path through a graph should be labelled by the labelled shortest labelled path between all subpaths") = forAll {
    (tpg: (Graph[N,Int,Int], LNode[N, Int], LNode[N, Int])) =>
    val start = tpg._2
    val end = tpg._3
    val graph = tpg._1
    (for {
      path <- graph.lesp(start.vertex, end.vertex).toVector
      subpath <- (0 until path._2.size).map(n => (path._1, path._2.dropRight(n))).toVector
      shortest = graph.esp(subpath._1, subpath._2.lastOption.fold(subpath._1)(_._1))
    } yield shortest.nonEmpty && shortest.get.length == (subpath._2.length + 1)).forall(identity)
  }

  property("The shortest labelled path through a graph without the labels should be the shortest path exactly") = forAll {
    (tpg: (Graph[N,Int,Int], LNode[N, Int], LNode[N, Int])) =>
    val start = tpg._2
    val end = tpg._3
    val graph = tpg._1
    val lpath = graph.lesp(start.vertex, end.vertex)
    val path = graph.esp(start.vertex, end.vertex)
    s"Labelled path is $lpath which should be $path if the labels are dropped" |: lpath.map{ case (s, p) => s +: p.map(_._1) } === path
  }
}
