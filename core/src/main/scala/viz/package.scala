package oncue.quiver

package object viz {

  /** Formats a graph for use with graphviz */
  def graphviz[N,A,B](g: Graph[N,A,B],
                      title: String = "fgl",
                      pageSize: (Double, Double) = (8.5, 11.0),
                      gridSize: (Int, Int) = (1, 1),
                      orient: Orient = Landscape): String = {
    def sn(node: LNode[N,A]) = node match {
      case LNode(n, a) =>
        val sa = sl(a)
        if (sa == "") "" else s"\t$n$sa\n"
    }
    def se(edge: LEdge[N,B]) = edge match {
      case LEdge(n1, n2, b) => s"\t$n1 -> $n2${sl(b)}\n"
    }
    val n = g.labNodes
    val e = g.labEdges
    val ns = (n flatMap sn).mkString
    val es = (e flatMap se).mkString
    def sz(w: Double, h: Double) = if (orient == Portrait) s"$w,$h" else s"$h,$w"
    val ps = s"${pageSize._1},${pageSize._2}"
    val (pw, ph) = if (orient == Portrait) gridSize else (gridSize._2, gridSize._1)
    val gs = sz(pageSize._1 * pw, pageSize._2 * ph)

    s"digraph $title {\n" +
      "\tmargin = \"0\"\n" +
      s"""\tpage = "${ps}"\n""" +
      s"""\tsize = "${gs}"\n""" +
      orient +
      "\tratio = \"fill\"\n" +
      ns +
      es +
    "}"
  }

  private def sq(s: String) =
    if (s.size == 1) s
    else if (s.startsWith("\""))
      s.substring(1, s.size - (if (s.endsWith("\"")) 1 else 0))
    else if (s.startsWith("'"))
      s.substring(1, s.size - (if (s.endsWith("'")) 1 else 0))
    else s

  private def sl[A](a: A) = {
    val l = sq(a.toString)
    if (l != "()") s""" [label = "${l}"]""" else ""
  }
}
