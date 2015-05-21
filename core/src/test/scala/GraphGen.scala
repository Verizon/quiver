package oncue.quiver

import org.scalacheck._
import org.scalacheck.Arbitrary._

object GraphGen {

  def graphGen[N: Arbitrary, A: Arbitrary, B: Arbitrary]: Gen[Graph[N,A,B]] = for {
    vs <- Gen.listOf(genNode[N,A])
    es <- Gen.listOf(genEdge[N,B])
  } yield safeMkGraph(vs, es)

  def genNode[N: Arbitrary, A: Arbitrary]: Gen[LNode[N,A]] = for {
    a <- arbitrary[A]
    v <- arbitrary[N]
  } yield LNode(v, a)


  def genEdge[N: Arbitrary, A: Arbitrary]: Gen[LEdge[N,A]] = for {
    x <- arbitrary[N]
    y <- arbitrary[N]
    a <- arbitrary[A]
  } yield LEdge(x, y, a)

  implicit def arbitraryEdge[A: Arbitrary, N: Arbitrary] = Arbitrary(genEdge[N,A])
  implicit def arbitraryNode[A: Arbitrary, N: Arbitrary] = Arbitrary(genNode[N,A])
  implicit def arbitraryGraph[A: Arbitrary, B: Arbitrary, N: Arbitrary] =
    Arbitrary(graphGen[N,A,B])

}
