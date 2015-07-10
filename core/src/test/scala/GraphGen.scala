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
