---
layout: default
title:  "Home"
section: "home"
---

## Overview

*Quiver* is a Scala library that provides support for modeling [multi-graphs](https://en.wikipedia.org/wiki/Multigraph) which is a network of _nodes_ connected by (possibly multiple) _directed edges_ between nodes. *Quiver* is useful for modelling state diagrams, network topologies, vector graphic scenes, calculating shortest-path traversals, and many other common domain problems which can be modelled as graphs.

*Quiver* is based on Martin Erwig's _[Functional Graph Library](http://web.engr.oregonstate.edu/~erwig/fgl/haskell/)_.

## Getting Started

To begin using *Quiver*, add the following dependency in your SBT build (check for the latest release by [looking on bintray](https://bintray.com/oncue/releases/quiver/view)):

```scala
libraryDependencies += "oncue.quiver" %% "core" % "x.x.x"
```

Then import the following package in your source file:

```scala
scala> import quiver._
import quiver._
```

<a name="construction"></a>

### Constructing Graphs

*Quiver* has two basic functions to build up graphs: the expression `empty` denotes the empty graph, and the expression `g & c` extends an existing graph `g` with a _context_ `c` of type `Context[N,A,B]`.

A `Context[N,A,B]` is a node `v:N` together with two lists of _adjacencies_: one for predecessors and one for successors of `v` represented by the type `Adj[N,B]`. An adjacency is a node `a:N` together with a label of type `B` of the edge from `v` to `a` (or vice versa). It's called a _context_ because it is a view of the graph from the context of a specific node. The context consists of the node itself, its two adjacency lists, and its label.

With these two functions, we can construct any graph. Let's look at a few very simple graphs of type `Graph[Int, Char, Unit]`. That is, graphs with integer nodes labeled by characters and unlabeled edges (they are labeled with the empty `()` value of the trivial type `Unit`).

The empty graph:

```scala
scala> import quiver._
import quiver._

scala> val nil = empty[Int,Char,Unit]
nil: quiver.Graph[Int,Char,Unit] = 
```

A graph with one node identified as `1`, labeled `a`:

```scala
scala> import quiver._
import quiver._

scala> val a = nil & Context(Vector(), 1, 'a', Vector())
a: quiver.Graph[Int,Char,Unit] = 1:a->[]
```

A graph with one node having an edge to itself:

```scala
scala> import quiver._
import quiver._

scala> val loop = nil & Context(Vector(), 1, 'a', Vector(() -> 1))
loop: quiver.Graph[Int,Char,Unit] = 1:a->[((),1)]
```

The `toString` method is defined to print the adjacency list representation. This means that a graph is shown as a list of labeled nodes, each followed by the list of labeled outgoing edges. 

Here is a graph with two nodes, `a` and `b`, and one edge `a -> b`:

```scala
scala> import quiver._
import quiver._

scala> val e = a & Context(Vector(() -> 1), 2, 'b', Vector())
e: quiver.Graph[Int,Char,Unit] = 
1:a->[((),2)]
2:b->[]
```

A graph with a cycle of two nodes `a <-> b`:

```scala
scala> import quiver._
import quiver._

scala> val ab = a & Context(Vector(() -> 1), 2, 'b', Vector(() -> 1))
ab: quiver.Graph[Int,Char,Unit] = 
1:a->[((),2)]
2:b->[((),1)]
```

The convention in quiver is that two edges like this with the same label going in opposite directions are equivalent to an _undirected_ edge.

There are several additional methods defined on `Graph` that make graph construction more convenient. The `addNode` and `addNodes` methods can be used to insert one or more labeled nodes into the graph. The methods `addEdge` and `addEdges` can be used to extend the graph with one or more labeled edges. Similarly, `removeNode`, `removeNodes`, `removeEdge`, and `removeEdges` do what you might expect.

A very useful function for building graphs is `mkGraph`, which just takes a list of nodes and a list of edges and constructs a graph. It is defined in the package `oncue.quiver`.

<a name="extraction"></a>

### Extracting Graph Information ###

Several methods on `Graph` let us extract global information about the graph. For example, `isEmpty` will tell us if the graph is empty. We can count the number of nodes in the graph with `countNodes`, and we can get the list of nodes with `nodes`, or the list of nodes and their labels with `labNodes`. Similarly, the methods `edges` and `labEdges` get the lists of plain and labeled edges.

We can also get information about individual nodes: for a graph that contains a node `v`, we can determine `v`'s successors by calling `successors(v)`, predecessors with `predecessors(v)` and both by calling `neighbors(v)`. The outgoing and incoming edges of a node can be accessed with `inEdges` and `outEdges`, respectively, and the number of inbound, outbound, or total connections to a node can be determined with `inDegree`, `outDegree`, and `degree`.

The above methods are all also defined on `Context`, which is particularly useful when looking at the decomposition of a graph.

<a name="decomposistion"></a>

### Graph Decomposition ###

The fundamental operation for decomposing a graph is given by the method `decomp`. When we say `g decomp v`, the result is a `Decomp(c,t)`, a _decomposition_ of a graph `g` into `c:Option[Context]`, the context of node `v` (if it exists, otherwise `None`), and the remainder of the graph, `t`, which does not contain the node `v`. We can regard `decomp` as the inverse of `&` in that `(g & c).decomp == Decomp(Some(c), g)`.

Let's look at some examples:

```scala
scala> a decomp 1
res0: quiver.Decomp[Int,Char,Unit] = Decomp(Some(Context(Vector(),1,a,Vector())),)

scala> loop decomp 1
res1: quiver.Decomp[Int,Char,Unit] = Decomp(Some(Context(Vector(),1,a,Vector(((),1)))),)

scala> ab decomp 1
res2: quiver.Decomp[Int,Char,Unit] = Decomp(Some(Context(Vector(((),2)),1,a,Vector(((),2)))),2:b->[])

scala> e decomp 1
res3: quiver.Decomp[Int,Char,Unit] = Decomp(Some(Context(Vector(),1,a,Vector(((),2)))),2:b->[])

scala> a decomp 2
res4: quiver.Decomp[Int,Char,Unit] = Decomp(None,1:a->[])
```

The `decompAny` method furthermore decomposes the graph on an arbitrary node. This operation is _unsafe_ in the same way as taking the `head` of a `List`. It's an error to decompose the empty graph, but `decompAny` will always pick the first node it finds if the graph is nonempty.

### General-Purpose Operations ###

The method `gmap` applies a function to all contexts of the graph. It takes a function of type `Context[N,A,B] => Context[N,C,D]` and produces a new graph with all the contexts transformed by the function.

Two specialized versions of this function, `nmap` and `emap`, allow you to apply a function over all the node and edge labels, respectively.

The `reverse` method swaps the direction of all edges in the graph.

A general fold operation on graph is given by the `fold` method. It successively decomposes all contexts from a graph and combines them in a right-associative way with a binary function of type `(Context[N,A,B], C) => C`, into a single value of some type `C`. It is very similar to the well-known `foldRight` function on lists, but an important difference is that the contexts are decomposed from the graph in an arbitrary order.

<a name="traversal"></a>

### Graph Traversal ###

Depth-first search is one of the most basic and important graph algorithms. A depth-first traversal of a graph essentially means to visit each node in the graph once by visiting successors before siblings.

The `dfs` method on `Graph` yields a `Seq[N]` which is the sequence of all the nodes in the graph, in the order visited. It takes a `Seq[N]` as its argument which is the list of nodes from which to start the search.

The `dff` method computes a depth-first spanning tree, which keeps the edges that have been traversed to reach all the nodes. It actually returns a _list_ of trees because the graph might not be connected. A list of trees is also called a _forest_, hence the name `dff` or "depth-first forest".

The data type `Tree` and functions for computing pre- and postorder lists of nodes are defined by the `scalaz.Tree` data type which is part of the [`Scalaz`](http://github.com/scalaz/scalaz) library.

Many different functions are provided by `Graph` for depth-first search. For example, `rdfs` is a depth-first search that follows predecessors rather than successors. And a very general depth-first traversal is provided by `xdfsWith`. It takes a function `d` of type `Context[N,A,B] => Seq[N]` to determine for each context visited which nodes to visit next, and a function `Context[N,A,B] => C` to compute a value for that context. The method then returns a `Vector[C]` of those computed values.

Functions for breadth-first search, shortest paths, voronoi diagrams, minimum spanning trees, and finding independent node sets are not yet provided by the library but could easily be added or written by the user.

