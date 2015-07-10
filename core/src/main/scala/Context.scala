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
case class Context[N,A,B](inEdges: Adj[N,B], vertex: N, label: A, outEdges: Adj[N,B]) {

  /** All the incoming edges plus identity arrows to self */
  def ins: Adj[N,B] = inEdges ++ outEdges.filter(_._2 == vertex)

  /** All the outgoing edges plus identity arrows from self */
  def outs: Adj[N,B] = outEdges ++ inEdges.filter(_._2 == vertex)

  /** All the targets of outgoing edges */
  def successors: Vector[N] = outs.map(_._2)

  /** All the sources of incoming edges */
  def predecessors: Vector[N] = ins.map(_._2)

  /** All neighbors of the node */
  def neighbors: Vector[N] = (inEdges ++ outEdges).map(_._2)

  def toGrContext: GrContext[N,A,B] = GrContext(fromAdj(inEdges), label, fromAdj(outEdges))

  /** Insert a successor after the focused node */
  def addSucc(n: N, edge: B) = Context(inEdges, vertex, label, outEdges :+ (edge -> n))

  /** Insert a predecessor after the focused node */
  def addPred(n: N, edge: B) = Context((edge -> n) +: inEdges, vertex, label, outEdges)
}

/** The label, predecessors, and successors of a given node */
case class GrContext[N,A,B](inEdges: Map[N, Set[B]],
                            label: A,
                            outEdges: Map[N, Set[B]]) {
  def toContext(v: N): Context[N,A,B] = Context(toAdj(inEdges), v, label, toAdj(outEdges))
}

