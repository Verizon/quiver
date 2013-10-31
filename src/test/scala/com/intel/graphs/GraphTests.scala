package com.intel.graphs

import org.scalacheck._
import org.scalacheck.Prop._

object GraphTests extends Properties("Graph") {
  val gg = new GraphGen[Byte]
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


}
