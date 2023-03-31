package base

import scala.collection.mutable.{Map as MMap}
import scala.collection.parallel._

import collection.parallel.CollectionConverters.IterableIsParallelizable

class ParGraph[A](
    var nodes: ParIterable[A],
    var edges: ParMap[A, ParIterable[A]]
) {
  require(nodes.forall(edges.get(_).isDefined))

  def this(nodes: ParIterable[A]) = this(nodes, nodes.map((_, ParIterable.empty)).toMap)
  def this(nodes: Iterable[A]) = this(nodes.par)

  def withEdges(newEdges: (A, A)*): ParGraph[A] = withNodesAndEdges(ParIterable.empty, newEdges.par)
  def withNodes(newNodes: A*): ParGraph[A] = withNodesAndEdges(newNodes.par, ParIterable.empty)
  def withNodesAndEdges(newNodes: ParIterable[A], newEdges: ParIterable[(A, A)]): ParGraph[A] = {
    val newNodeList = (nodes ++ newNodes).toSet.par
    val newEdgesGrouped = newEdges.par.groupBy(_._1).map((k, v) => (k, v.map(_._2)))
    val newEdgeList: ParMap[A, ParIterable[A]] = newNodeList
      .map(n =>
        (
          n,
          newEdgesGrouped.get(n) match {
            case None => ParSet.empty
            case Some(s) => (edges(n) ++ s)
          }
        )
      )
      .toMap

    nodes = newNodeList
    edges = newEdgeList

    invalidateReachability

    this
  }

  var reachableMatrix: Option[ParMap[A, ParMap[A, Boolean]]] = None
  private def invalidateReachability = (reachableMatrix = None)
  private def computeReachability = {
    // simple Floyd-Warshall
    // with all these stupid conversions it might be slower than the sequential one smh
    // TODO: make simpler
    val snodes = nodes.seq
    val reach = nodes.map(n => (n, MMap(nodes.map(m => (m, false)).seq.toSeq: _*))).toMap

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
  def merge(other: ParGraph[A]): ParGraph[A] = {
    // safe merge with duplicates concatenated
    // TODO: this is stupid, make this efficient
    val newEdgeList = edges ++ other.edges.map { case (k, v) => k -> (v ++ edges.getOrElse(k, ParIterable.empty)).toSet.par }

    ParGraph(nodes ++ other.nodes, newEdgeList)
  }

  def transitiveClosure: ParGraph[A] = {
    val newEdgeList: ParMap[A, ParIterable[A]] = nodes
      .map(n =>
        (n ->
          nodes.filter(reachable(n, _)))
      )
      .toMap

    ParGraph(nodes, newEdgeList)
  }

  def hasCycle: Boolean = nodes.exists(n => nodes.exists(m => reachable(n, m) && reachable(m, n)))

  override def toString(): String =
    // pain
    edges.map { case (k, v) => v.map(x => s"$k -> $x").toSet }.fold(ParSet.empty)(_ ++ _).seq.toString()
}
