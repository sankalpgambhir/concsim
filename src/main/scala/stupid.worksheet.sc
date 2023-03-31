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

val y = 6

import concsim.program._

val x = Variable("x")
val p = Program(
    Seq(
        Seq(Read(x), Write(x, 2), Read(x, 0))
    )
)

p.valid(SequentialConsistency)

