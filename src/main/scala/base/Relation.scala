package concsim.base

import scala.collection.mutable.Set
import scala.collection.parallel._
import collection.parallel.CollectionConverters.IterableIsParallelizable

class Relation[A]
    (
    val objects: Set[A],
    val graph: Graph[A]
    ) {

    def this(objects: Set[A]) = this(objects, Graph(objects))
    def this() = this(Set.empty)

    def addObjects(newObjects: A*): this.type = {
        for (o <- newObjects) if (!objects.contains(o)) objects += o
        this
    }

    def addEdges(newEdges: (A, A)*): this.type = {
        addObjects(newEdges.map(_._1): _*)
        graph.withNodesAndEdges(objects, newEdges)
        this
    }

    def hasCycle = graph.hasCycle
    def isAcyclic = !hasCycle

    def union (other: Relation[A]): Relation[A] = Relation(objects ++ other.objects, graph.merge(other.graph))
    def transitiveClosure: Relation[A] = Relation(objects, graph.transitiveClosure)

    // infix definitions for convenience
    infix def U (other: Relation[A]): Relation[A] = this.union(other)
    def `+`: Relation[A] = transitiveClosure

    extension (s: Seq[Relation[A]]) {
        def union: Relation[A] = s.par.reduce(_.union(_))
    }

    override def toString(): String = graph.toString
}
