package concsim.base

import scala.collection.mutable.{Map as MMap}
import scala.collection.parallel._

import collection.parallel.CollectionConverters.IterableIsParallelizable

class Graph[A](
    var nodes: Iterable[A],
    var edges: Map[A, Set[A]]
) {
  def this(nodes: Iterable[A]) = this(nodes, nodes.map((_, Set.empty)).toMap)

  def withEdges(newEdges: (A, A)*): Graph[A] = withNodesAndEdges(Iterable.empty, newEdges)
  def withNodes(newNodes: A*): Graph[A] = withNodesAndEdges(newNodes, Iterable.empty)
  def withNodesAndEdges(newNodes: Iterable[A], newEdges: Iterable[(A, A)]): Graph[A] = {
    val newNodeList = (nodes ++ newNodes)
    val newEdgesGrouped = newEdges.groupBy(_._1).map((k, v) => (k, v.map(_._2)))
    val newEdgeList: Map[A, Set[A]] = newNodeList.map(n => (n, edges.getOrElse(n, Set.empty) ++ newEdgesGrouped.getOrElse(n, Iterator.empty))).toMap

    nodes = newNodeList
    edges = newEdgeList

    // possibly new nodes and edges, compute the reachability matrix again when needed
    invalidateReachability

    this
  }

  var reachableMatrix: Option[Map[A, Map[A, Boolean]]] = None
  private def invalidateReachability = (reachableMatrix = None)
  private def computeReachability = {
    // simple Floyd-Warshall
    // TODO: make simpler
    val snodes = nodes
    val reach = nodes.map(n => (n, MMap(nodes.map(m => (m, false)).toSeq: _*))).toMap

    // account for edges
    for (i <- snodes)
      for (j <- edges(i))
        reach(i)(j) = true

    // closure
    for (k <- snodes)
      for (i <- nodes)
        for (j <- snodes)
          if (!reach(i)(j) && (reach(i)(k) && reach(k)(j)))
            reach(i)(j) = true

    reachableMatrix = Some(
      nodes.map(n => (n, nodes.map(m => (m, reach(n)(m))).toMap)).toMap
    )
  }

  def reachable(n: A, m: A): Boolean =
    if (n == m) false
    else {
      if (reachableMatrix.isEmpty) computeReachability
      reachableMatrix.get.apply(n)(m)
    }

  /**
   * Merge two Graphs, taking union of the nodes and edges
   *
   * @param other
   * @return
   */
  def merge(other: Graph[A]): Graph[A] = {
    // safe merge with duplicates concatenated
    // TODO: this is stupid, make this efficient
    val newEdgeList = edges ++ other.edges.map { case (k, v) => k -> (v ++ edges.getOrElse(k, Set.empty)) }

    Graph(nodes ++ other.nodes, newEdgeList)
  }

  def transitiveClosure: Graph[A] = {
    val newEdgeList: Map[A, Set[A]] = nodes
      .map(n =>
        (n ->
          nodes.filter(reachable(n, _)).toSet)
      )
      .toMap

    Graph(nodes, newEdgeList)
  }

  def hasCycle: Boolean = nodes.exists(n => nodes.exists(m => reachable(n, m) && reachable(m, n)))

  def topologicalSort: Seq[A] = {
    require(!hasCycle)

    val permaMark = MMap(nodes.map(_ -> false).toSeq: _*)
    val tempMark = MMap(nodes.map(_ -> false).toSeq: _*)

    var sorted = Seq[A]()

    // sort n and its children
    def sort_(n: A): Unit =
      if (!permaMark(n)) {
        if (!tempMark(n)) {
          tempMark(n) = true

          edges.getOrElse(n, Set()).foreach(sort_(_))

          tempMark(n) = false
          permaMark(n) = true
          sorted = n +: sorted
        }
      }

    nodes.foreach(sort_(_))

    sorted
  }

  def copy: Graph[A] = Graph(nodes, edges)

  override def toString(): String =
    // pain
    edges.map { case (k, v) => v.toSeq.map(x => s"$k -> $x") }.fold(Seq.empty)(_ ++ _).toString()
}
