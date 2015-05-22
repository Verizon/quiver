package quiver

/**
 * The decomposition of a graph into a detached context focused on one node
 * and the rest of the graph.
 */
case class Decomp[N,A,B](ctx: Option[Context[N,A,B]], rest: Graph[N,A,B]) {
  def addSucc(node: LNode[N,A], edge: B): Decomp[N,A,B] =
    ctx.map(x => GDecomp(x, rest).addSucc(node, edge).toDecomp).getOrElse(this)
  def addPred(node: LNode[N,A], edge: B): Decomp[N,A,B] =
    ctx.map(x => GDecomp(x, rest).addPred(node, edge).toDecomp).getOrElse(this)
  def toGraph: Graph[N,A,B] = ctx.foldLeft(rest)(_ & _)
}

/** The same as `Decomp`, only more sure of itself */
case class GDecomp[N,A,B](ctx: Context[N,A,B], rest: Graph[N,A,B]) {
  def addSucc(node: LNode[N,A], edge: B): GDecomp[N,A,B] =
    GDecomp(Context(Vector(edge -> ctx.vertex), node.vertex, node.label, Vector()), rest & ctx)
  def addPred(node: LNode[N,A], edge: B): GDecomp[N,A,B] =
    GDecomp(Context(Vector(), node.vertex, node.label, Vector(edge -> ctx.vertex)), rest & ctx)
  def toDecomp: Decomp[N,A,B] = Decomp(Some(ctx), rest)
  def toGraph: Graph[N,A,B] = rest & ctx
}


/**
 * The decomposition of a graph into two detached
 * contexts focused on distinguished "first" and "last" nodes.
 */
case class BiDecomp[N,A,B](first: Context[N,A,B], last: Context[N,A,B], rest: Graph[N,A,B]) {
  def toGraph: Graph[N,A,B] = rest & first & last

  /** Appends a successor to the last node in this graph and makes that the last node. */
  def addSucc(node: LNode[N,A], edge: B): BiDecomp[N,A,B] =
    BiDecomp(first, Context(Vector(edge -> last.vertex), node.vertex, node.label, Vector()), rest & last)

  /** Prepends a predecessor to the first node in this graph and makes that the first node. */
  def addPred(node: LNode[N,A], edge: B): BiDecomp[N,A,B] =
    BiDecomp(Context(Vector(edge -> first.vertex), node.vertex, node.label, Vector()), last, rest & first)

  /**
   * Appends one decomposition to another. The first node of this graph will be the first node of the result.
   * The last node of the given graph will be the last node of the result. The given edge will be added
   * from the last node of this graph to the first node of the given graph.
   */
  def append(b: BiDecomp[N,A,B], edge: B): BiDecomp[N,A,B] =
    BiDecomp(first, b.last, rest union b.rest & last & b.first addEdge LEdge(last.vertex, b.first.vertex, edge))
}
