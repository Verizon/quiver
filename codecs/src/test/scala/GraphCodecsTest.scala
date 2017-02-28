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
import scodec.codecs
import scodec.Attempt
import scalaz.\/

object GraphCodecsTest extends Properties("codecs"){
  import GraphGen.{arbitraryNode,arbitraryEdge,arbitraryGraph}

  implicit val uint8 = codecs.int32

  def roundTrip[A](typeName: String)(implicit ca: scodec.Codec[A], aa: Arbitrary[A]): Unit = {
    val _ = property(s"binary encoding round trip - $typeName") = {
      forAll { a: A =>
        val result = for {
          encoded <- ca.encode(a)
          decoded <- ca.decode(encoded)
        } yield decoded.value

        result == Attempt.Successful(a)
      }
    }
  }

  roundTrip[LNode[Int,Int]]("nodes")(GraphCodecs.lnode, arbitraryNode)
  roundTrip[LEdge[Int,Int]]("edges")(GraphCodecs.ledge, arbitraryEdge)
  roundTrip[Graph[Int,Int,Int]]("graph")(GraphCodecs.graph, arbitraryGraph)
}
