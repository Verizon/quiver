package quiver

import scodec._
import scodec.bits._

trait Codecs {
  implicit val uint8 = codecs.uint8
  implicit val utf8: Codec[String] =
    codecs.variableSizeBytes(codecs.int32, codecs.utf8)
  implicit def tuple2[A:Codec,B: Codec]: Codec[(A,B)] =
    Codec[A] ~ Codec[B]
}

import shapeless._

trait GraphCodecs extends Codecs { self =>
  def ledge[N : Codec, A : Codec]: Codec[LEdge[N,A]] =
    (implicitly[Codec[N]] ::
     implicitly[Codec[N]] ::
     implicitly[Codec[A]]).as[LEdge[N,A]]

  def lnode[N : Codec, A : Codec]: Codec[LNode[N,A]] =
    (implicitly[Codec[N]] ::
     implicitly[Codec[A]]).as[LNode[N,A]]
}
