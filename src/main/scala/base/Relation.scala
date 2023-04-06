package concsim.base

import scala.collection.mutable.Set
import scala.collection.parallel._

import collection.parallel.CollectionConverters.IterableIsParallelizable

class Relation[A](
    val objects: Set[A],
    val graph: Graph[A]
) {

  def this(objects: Set[A]) = this(objects, Graph(objects))
  def this() = this(Set.empty)
  def this(objects: A*) = this(Set(objects: _*))

  def copy: Relation[A] = Relation(objects, graph.copy)

  def addObjects(newObjects: A*): this.type = {
    for (o <- newObjects) objects += o
    this
  }

  def addEdges(newEdges: (A, A)*): this.type = {
    addObjects(newEdges.flatten { (a, b) => Seq(a, b) }: _*)
    graph.withNodesAndEdges(objects, newEdges)
    this
  }

  /**
   * Returns a new Relation with these edges added
   */
  def withEdges(newEdges: (A, A)*): Relation[A] = {
    val r = this.copy
    r.addEdges(newEdges: _*)
    r
  }

  def relates(n: A, m: A): Boolean = graph.edges.get(n).fold(false)(_.contains(m))
  def reachable(n: A, m: A): Boolean = objects.contains(n) && objects.contains(m) && graph.reachable(n, m)

  def hasCycle = graph.hasCycle
  def isAcyclic = !hasCycle
  def linearized = graph.topologicalSort

  def union(other: Relation[A]): Relation[A] = Relation(objects ++ other.objects, graph.merge(other.graph))
  def transitiveClosure: Relation[A] = Relation(objects, graph.transitiveClosure)

  // infix definitions for convenience
  infix def U(other: Relation[A]): Relation[A] = this.union(other)
  def `+` : Relation[A] = transitiveClosure

  extension (s: Seq[Relation[A]]) {
    def union: Relation[A] = s.par.reduce(_.union(_))
  }

  override def toString(): String = graph.toString
}
