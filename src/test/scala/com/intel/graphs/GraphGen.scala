package com.intel.graphs

import org.scalacheck._
import org.scalacheck.Arbitrary._

class GraphGen[Node: Arbitrary] extends Graphs[Node] {

  def graphGen[A: Arbitrary, B: Arbitrary]: Gen[Graph[A,B]] = for {
    vs <- Gen.listOf(genNode[A])
    es <- Gen.listOf(genEdge[B])
  } yield mkGraph(vs, es)

  def genNode[A: Arbitrary]: Gen[LNode[A]] = for {
    a <- arbitrary[A]
    v <- arbitrary[Node]
  } yield LNode(v, a)


  def genEdge[A: Arbitrary]: Gen[LEdge[A]] = for {
    x <- arbitrary[Node]
    y <- arbitrary[Node]
    a <- arbitrary[A]
  } yield LEdge(x, y, a)

  implicit def arbitraryEdge[A: Arbitrary] = Arbitrary(genEdge[A])
  implicit def arbitraryNode[A: Arbitrary] = Arbitrary(genNode[A])
  implicit def arbitraryGraph[A: Arbitrary, B: Arbitrary] = Arbitrary(graphGen[A,B])

}
