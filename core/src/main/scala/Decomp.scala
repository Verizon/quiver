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

import scalaz.Comonad

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
  def toGDecomp: Option[GDecomp[N,A,B]] = ctx.map(c => GDecomp(c, rest))
}

/** The same as `Decomp`, only more sure of itself */
case class GDecomp[N,A,B](ctx: Context[N,A,B], rest: Graph[N,A,B]) {
  def addSucc(node: LNode[N,A], edge: B): GDecomp[N,A,B] =
    GDecomp(Context(Vector(edge -> ctx.vertex), node.vertex, node.label, Vector()), rest & ctx)
  def addPred(node: LNode[N,A], edge: B): GDecomp[N,A,B] =
    GDecomp(Context(Vector(), node.vertex, node.label, Vector(edge -> ctx.vertex)), rest & ctx)
  def toDecomp: Decomp[N,A,B] = Decomp(Some(ctx), rest)
  def toGraph: Graph[N,A,B] = rest & ctx

  /** Get the label of the node under focus */
  def label: A = ctx.label

  /** Map over the labels in this decomposition */
  def map[C](f: A => C): GDecomp[N,C,B] = extend(x => f(x.label))

  /**
   * Recursively decompose the graph, passing each decomposition to the given function, storing the results as labels.
   * The resulting decomposition has the exact same structure as this one, just redecorated with new labels.
   */
  def extend[C](f: GDecomp[N,A,B] => C): GDecomp[N,C,B] =
    GDecomp(ctx.copy(label = f(this)),
            rest.decompAny.toGDecomp.map(_.extend(f).toGraph).getOrElse(empty))
}

object GDecomp {
  implicit def gDecompComonad[N,B]: Comonad[({type λ[α] = GDecomp[N,α,B]})#λ] =
    new Comonad[({type λ[α] = GDecomp[N,α,B]})#λ] {
      def copoint[A](as: GDecomp[N,A,B]) = as.label
      def map[A,C](as: GDecomp[N,A,B])(f: A => C) = as map f
      def cobind[A,C](as: GDecomp[N,A,B])(f: GDecomp[N,A,B] => C) =
        as extend f
    }
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
