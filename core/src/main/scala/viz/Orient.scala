package oncue.quiver
package viz

sealed trait Orient {
  override def toString = this match {
    case Portrait => "\trotate = \"0\"\n"
    case Landscape => "\trotate = \"90\"\n"
  }
}
case object Portrait extends Orient
case object Landscape extends Orient

