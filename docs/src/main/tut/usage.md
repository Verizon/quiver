## Graphs ##

To begin using the library, add the following as a dependency in your SBT build:

```scala
libraryDependencies += "oncue" %% "quiver" % "3.0"
```

Then import the following package in your source file:

```tut
import oncue.quiver._
```

### Constructing Graphs ###

We have two basic functions to build up graphs: the expression `empty` denotes the empty graph, and the expression `g & c` extends an existing graph `g` with a _context_ `c` of type `Context[N,A,B]`.

A `Context[N,A,B]` is a node `v:N` together with two lists of _adjacencies_: one for predecessors and one for successors of `v` represented by the type Adj[N,B]. An adjacency is a node `a:N` together with the label of type `B` of the edge from `v` to `a` (or vice versa). It's called a _context_ because it is a view of the graph from the context of a specific node. The context consists of the node itself, its two adjacency lists, and its label.

With these two functions, we can construct any graph. Let's look at a few very simple graphs of type `Graph[Int, Char, Unit]`. That is, graphs with integer nodes labeled by characters and unlabeled edges (they are labeled with the empty `()` value of the trivial type `Unit`).

The empty graph:

```tut
val nil = empty[Int,Char,Unit]
```
A graph with one node identified as `1`, labeled `a`:

```tut
val a = nil & Context(Vector(), 1, 'a', Vector())
```

A graph with one node having an edge to itself:

```tut
val loop = nil & Context(Vector(), 1, 'a', Vector(() -> 1))
```

A graph with one edge `a -> b`:

```tut
val e = a & Context(Vector(() -> 1), 2, 'b', Vector())
```

A graph with a cycle of two nodes `a <-> b`:

```tut
val ab = a & Context(Vector(() -> 1), 2, 'b', Vector(() -> 1))
```

To have a textual representation of graphs, the `toString` method is defined to print the adjacency list representation. This means that a graph is shown as a list of labeled nodes, each followed by the list of labeled outgoing edges. 

There are several additional methods defined on `Graph` that make graph construction more convenient. The `addNode` and `addNodes` methods can be used to insert one or more labeled nodes into the graph. The methods `addEdge` and `addEdges` can be used to extend the graph with one or more labeled edges. Similarly, `removeNode`, `removeNodes`, `removeEdge`, and `removeEdges` do what you might expect.

A very useful function for building graphs is `mkGraph`, which just takes a list of nodes and a list of edges and constructs a graph. It is defined in the package `oncue.quiver`.

### Extracting Graph Information ###

Several methods on `Graph` let us extract global information about the graph. For example, `isEmpty` will tell us if the graph is empty. We can count the number of nodes in the graph with `countNodes`, and we can get the list of nodes with `nodes`, or the list of nodes and their labels with `labNodes`. Similarly, the methods `edges` and `labEdges` get the lists of plain and labeled edges.

We can also get information about individual nodes: for a graph that contains a node `v`, we can determine `v`'s successors by calling `successors(v)`, predecessors with `predecessors(v)` and both by calling `neighbors(v)`. The outgoing and incoming edges of a node can be accessed with `inEdges` and `outEdges`, respectively, and the number of inbound, outbound, or total connections to a node can be determined with `inDegree`, `outDegree`, and `degree`.

The above methods are all also defined on `Context`, which is particularly useful when looking at the decomposition of a graph.

### Graph Decomposition ###

The fundamental operation for decomposing a graph is given by the method `decomp`. When we say `g decomp v`, the result is a `Decomp(c,t)`, a _decomposition_ of a graph `g` into `c:Option[Context]`, the context of node `v` (if it exists, otherwise `None`), and the remainder of the graph, `t`, which does not contain the node `v`. We can regard `decomp` as the inverse of `&` in that `(g & c).decomp == Decomp(Some(c), g)`.

Let's look at some examples:

```tut
a decomp 1
loop decomp 1
ab decomp 1
e decomp 1
a decomp 2
```

The `decompAny` method furthermore decomposes the graph on an arbitrary node. It's an error to decompose the empty graph, but `decompAny` will always pick the first node it finds if the graph is nonempty.

### General-Purpose Operations ###

The method `gmap` applies a function to all contexts of the graph. It takes a function of type `Context[N,A,B] => Context[N,C,D]` and produces a new graph with all the contexts transformed by the function.

Two specialized versions of this function, `nmap` and `emap`, allow you to apply a function over all the node and edge labels, respectively.

The `reverse` method swaps the direction of all edges in the graph.

A general fold operation on graph is given by the `fold` method. It successively decomposes all contexts from a graph and combines them in a right-associative way with a binary function of type `(Context[N,A,B], C) => C`, into a single value of some type `C`. It is very similar to the well-known `foldRight` function on lists, but an important difference is that the contexts are decomposed from the graph in an arbitrary order.

### Graph Traversal ###

Depth-first search is one of the most basic and important graph algorithms. A depth-first traversal of a graph essentially means to visit each node in the graph once by visiting successors before siblings.

The `dfs` method on `Graph` yields a `Seq[N]` which is the sequence of all the nodes in the graph, in the order visited. It takes a `Seq[N]` as its argument which is the list of nodes from which to start the search.

The `dff` method computes a depth-first spanning tree, which keeps the edges that have been traversed to reach all the nodes. It actually returns a _list_ of trees because the graph might not be connected. A list of trees is also called a _forest_, hence the name `dff` or "depth-first forest".

The data type `Tree` and functions for computing pre- and postorder list of nodes are defined by the `scalaz.Tree` data type which is part of the [`Scalaz`](http://github.com/scalaz/scalaz) library.

Many different functions are provided by `Graph` for depth-first search. For example, `rdfs` is a depth-first search that follows predecessors rather than successors. And a very general depth-first traversal is provided by `xdfsWith`. It takes a function `d` of type `Context[N,A,B] => Seq[N]` to determine for each context visited which nodes to visit next, and a function `Context[N,A,B] => C` to compute a value for that context. The method then returns a `Vector[C]` of those computed values.

Functions for breadth-first search, shortest paths, voronoi diagrams, minimum spanning trees, and finding independent node sets are not yet provided by the library but could easily be added or written by the user.

