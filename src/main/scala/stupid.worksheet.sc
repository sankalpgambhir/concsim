import concsim.program.Program

import concsim.base._
import scala.collection.mutable.Set
import scala.collection.parallel._

val x2 = Relation[Int](Set(1, 2, 3, 4))
x2.addEdges((1, 2), (2, 3))

val z = Relation(Set(1, 2, 3, 4))
z.addEdges((2, 4), (4, 1))
println(z.toString)

println(x2.toString)

val k = (x2 U z).+

k.graph.reachable(1, 4)
k.graph.reachable(4, 1)
k.graph.reachableMatrix

k.hasCycle

val k1 = Relation(1, 2, 3)
k1.addEdges(1 -> 2, 2 -> 3)
k1.linearized

val k2 = k1.withEdges(3 -> 2)
k2.addEdges(2 -> 1)

import concsim.program._

val x = Variable("x")
val y = Variable("y")

val p1 = Program(
    Seq(
        Seq(Read(x), Write(x, 2), Read(x, 0))
    )
)

val p2 = Program(
    Seq(
        Seq(Write(x, 1), Read(y, 0), Write(y, 1), Read(x, 1)),
        Seq(Write(x, 2), Read(y, 0), Write(y, 0), Read(x))
    )
)

val p3 = Program(
    Seq(
        Seq(Write(x, 1), Read(x, 2)),
        Seq(Write(x, 2), Read(x, 2))
    )
)

SequentialConsistency.hb(p1)
SequentialConsistency.hb(p2)
SequentialConsistency.hb(p3)

p1.validUnder(SequentialConsistency)
p2.validUnder(SequentialConsistency)
p3.validUnder(SequentialConsistency)
