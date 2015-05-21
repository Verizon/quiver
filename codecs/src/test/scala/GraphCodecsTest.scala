package quiver

import org.scalacheck._
import org.scalacheck.Prop._
import scodec.codecs
import scalaz.\/

object GraphCodecsTest extends Properties("codecs"){
  import GraphGen.{arbitraryNode,arbitraryEdge}

  implicit val uint8 = codecs.int32

  def roundTrip[A](typeName: String)(implicit ca: scodec.Codec[A], aa: Arbitrary[A]): Unit = {
    property(s"binary encoding round trip - $typeName") = {
      forAll { a: A =>
        val result = for {
          encoded <- ca.encode(a)
          decoded <- ca.decode(encoded)
        } yield decoded._2
        result == (\/.right(a))
      }
    }
  }

  roundTrip[LNode[Int,Int]]("nodes")(GraphCodecs.lnode, arbitraryNode)
  roundTrip[LEdge[Int,Int]]("edges")(GraphCodecs.ledge, arbitraryEdge)
}
