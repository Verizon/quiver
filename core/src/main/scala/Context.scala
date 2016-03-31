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

/** The view of a graph focused on the context surrounding a particular node. */
case class Context[N,A,B](inAdj: Adj[N,B], vertex: N, label: A, outAdj: Adj[N,B]) {

  /** Adjacency list for all the incoming edges plus identity arrows to self */
  def ins: Adj[N,B] = inAdj ++ outAdj.filter(_._2 == vertex)

  /** Adjacency list for all the outgoing edges plus identity arrows from self */
  def outs: Adj[N,B] = outAdj ++ inAdj.filter(_._2 == vertex)

  /** Labeled edges into the focused node, including identity arrows to self */
  def inEdges: Vector[LEdge[N,B]] =
    ins.map { case (b, n) => LEdge(n, vertex, b) }

  /** Labeled edges from the focused node, including identity arrows from self */
  def outEdges: Vector[LEdge[N,B]] =
    outs.map { case (b, n) => LEdge(vertex, n, b) }

  /** All the targets of outgoing edges */
  def successors: Vector[N] = outs.map(_._2)

  /** All the sources of incoming edges */
  def predecessors: Vector[N] = ins.map(_._2)

  /** All neighbors of the node */
  def neighbors: Vector[N] = (inAdj ++ outAdj).map(_._2)

  def toGrContext: GrContext[N,A,B] = GrContext(fromAdj(inAdj), label, fromAdj(outAdj))

  /** Insert a successor after the focused node */
  def addSucc(n: N, edge: B): Context[N,A,B] =
    Context(inAdj, vertex, label, outAdj :+ (edge -> n))

  /** Insert a predecessor after the focused node */
  def addPred(n: N, edge: B): Context[N,A,B] =
    Context((edge -> n) +: inAdj, vertex, label, outAdj)

  /** Embed this context in the given graph */
  def embed(g: Graph[N,A,B]): Graph[N,A,B] =
    g embed this

  /** Alias for `embed` */
  def &(g: Graph[N,A,B]): Graph[N,A,B] =
    embed(g)
}

/** The label, predecessors, and successors of a given node */
case class GrContext[N,A,B](inAdj: Map[N, Set[B]],
                            label: A,
                            outAdj: Map[N, Set[B]]) {
  def toContext(v: N): Context[N,A,B] = Context(toAdj(inAdj), v, label, toAdj(outAdj))
}

