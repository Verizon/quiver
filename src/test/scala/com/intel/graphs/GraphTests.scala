package com.intel.graphs

import org.scalacheck._
import org.scalacheck.Prop._
import scalaz.{Node => _, _}

object GraphTests extends Properties("Graph") {
  type Node = Byte
  val gg = new GraphGen[Node]
  import gg._

  property("Adding a node to a graph yields a graph that contains that node") =
    forAll { (g: Graph[Int, Int], n: LNode[Int]) =>
      (g addNode n) contains n.vertex
    }

  property("Adding a node and removing it again yields a graph that doesn't contain that node") =
    forAll { (g: Graph[Int, Int], n: LNode[Int]) =>
      !g.addNode(n).removeNode(n.vertex).contains(n.vertex)
    }

  property("Adding an edge should yield a map that contains that edge") =
    forAll { (g: Graph[Int, Int], e: LEdge[Int]) =>
      g.addNode(LNode(e.from, 0)).addNode(LNode(e.to, 0)).addEdge(e).findEdge(e.edge).isDefined
    }

  property("Adding an edge and removing it again yields a graph that doesn't contain that edge") =
    forAll { (g: Graph[Int, Int], e: LEdge[Int]) =>
      g.addNode(LNode(e.from, 0)).
      addNode(LNode(e.to, 0)).
      addEdge(e).
      removeLEdge(e).
      findEdge(e.edge).isEmpty
    }

  property("A graph constructed from nodes and edges should contain those nodes and edges") =
    forAll { (ns: List[Node], es: List[LEdge[Int]], i: Int) =>
      implicit val NO = Order[LNode[Int]].toScalaOrdering
      implicit val EO = Order[LEdge[Int]].toScalaOrdering
      val nns = ns.toSet.toList.map((b: Node) => LNode(b, i)).sorted
      val ees = es.toSet.toList.filter((x: LEdge[Int]) =>
                  ns.contains(x.from) && ns.contains(x.to)
                ).map((x: LEdge[Int]) =>
                  LEdge(x.from, x.to, i)
                ).sorted
      val g = mkGraph(nns, ees)
      nns == g.labNodes.sorted && ees == g.labEdges.sorted
    }
}
