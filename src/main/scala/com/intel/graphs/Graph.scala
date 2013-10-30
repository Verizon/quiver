import scala.collection.immutable.IntMap

import scalaz.std.map._

//object Graph {
//  type AssocMap[A] = Map[A, Vector[A]]
//
//  def fromMap[K,V](m: Map[V, Seq[V]], key: V => K): Graph[K,V] =
//    fromEdges(m.toSeq.flatMap { case (k, vs) => vs.map(v => Edge(k, v)) }, key)
//
//  def fromEdges[K,V](edges: Seq[Edge[V]], key: V => K): Graph[K,V] =
//    empty(key).addEdges(edges)
//
//  def empty[K,V](key: V => K): Graph[K,V] = new Graph[K,V](Map(), Map(), key)
//}
//
//case class Edge[A](from: A, to: A)
//
//import Graph._
//
///**
// * A directed graph with labeled vertices of type `V`
// * which are uniquely identified by a key of type `K`
// */
//class Graph[K,V](assocMap: AssocMap[K],
//                 contents: Map[K,V],
//                 key: V => K) {
//
//  def addEdge(edge: Edge[V]): Graph[K,V] = {
//    val k1 = key(edge.from)
//    val k2 = key(edge.to)
//    val c = contents + (k1 -> edge.from) + (k2 -> edge.to)
//    val a = assocMap + (k1 -> (assocMap(k1) :+ k2))
//    new Graph(a, c, key)
//  }
//
//  def addEdges(s: Seq[Edge[V]]): Graph[K,V] =
//    s.foldLeft(this)(_ addEdge _)
//
//  def removeEdge(edge: Edge[K]): Graph[K,V] =
//    new Graph(alter(assocMap, edge.from)(_.map(_.filter(_ != edge.to))), contents, key)
//
//  def removeNode(node: V): Graph[K,V] = removeKey(key(node))
//
//  def removeKey(k: K): Graph[K,V] =
//    new Graph((assocMap - k).mapValues(_.filter(_ != k)), contents - k, key)
//
//  /** All vertices of this graph */
//  def vertices: Vector[K] = assocMap.keys.toVector
//
//  /** All the labels of this graph */
//  def labels: Vector[V] = contents.values.toVector
//
//  /** An association map representation of this graph */
//  def toMap: AssocMap[V] =
//    assocMap.map { case (k, ks) => contents(k) -> ks.map(contents) }
//
//  /** All edges of this graph */
//  def edges: Vector[Edge[V]] = for {
//    v <- vertices
//    w <- assocMap(v)
//  } yield Edge(contents(v), contents(w))
//
//  private def mapT(f: (K, Vector[K]) => Vector[K]): AssocMap[K] =
//    vertices.map(v => (v -> f(v, assocMap(v)))).toMap
//
//  /** The graph obtained by reversing all the edges */
//  def transpose: Graph[K,V] = Graph.fromEdges(reverseEdges, key)
//
//  private def reverseEdges: Vector[Edge[V]] =
//    edges map { case Edge(f, t) => Edge(t, f) }
//
//  /** A table of the count of edges from each node. */
//  def outDegree: Map[K,Int] = assocMap.mapValues(_.length)
//
//  /** A table of the count of edges into each node. */
//  def inDegree: Map[K,Int] =
//    transpose.outDegree
//}
//
