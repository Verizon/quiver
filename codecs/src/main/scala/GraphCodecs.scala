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
