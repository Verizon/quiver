package quiver

import scodec._
import scodec.bits._
import shapeless._

object GraphCodecs {

  def ledge[N : Codec, A : Codec]: Codec[LEdge[N,A]] =
    (implicitly[Codec[N]] ::
     implicitly[Codec[N]] ::
     implicitly[Codec[A]]).as[LEdge[N,A]]

  def lnode[N : Codec, A : Codec]: Codec[LNode[N,A]] =
    (implicitly[Codec[N]] ::
     implicitly[Codec[A]]).as[LNode[N,A]]
}
