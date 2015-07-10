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

import scodec._
import scodec.bits._
import shapeless._
import scalaz.\/

object GraphCodecs {

  def ledge[N: Codec, A: Codec]: Codec[LEdge[N,A]] =
    (implicitly[Codec[N]] ::
     implicitly[Codec[N]] ::
     implicitly[Codec[A]]).as[LEdge[N,A]]

  def lnode[N: Codec, A: Codec]: Codec[LNode[N,A]] =
    (implicitly[Codec[N]] ::
     implicitly[Codec[A]]).as[LNode[N,A]]

  // needed because sometimes the codecs are greedy which
  // makes the whole graph not decode correctly.
  private def indexedSeq[A : Codec]: Codec[IndexedSeq[A]] =
    codecs.variableSizeBytes(
      codecs.int32,
      codecs.vector(implicitly[Codec[A]]).xmap(a => a, _.toVector))

  def graph[N: Codec, A: Codec, B: Codec]: Codec[Graph[N,A,B]] =
    (indexedSeq(lnode[N,A]) ~ indexedSeq(ledge[N,B])).xmap(
      q => safeMkGraph(q._1,q._2),
      g => (g.labNodes, g.labEdges)
    )
}
