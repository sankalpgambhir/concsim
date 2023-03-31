

import concsim.base._
import scala.collection.mutable.Set
import scala.collection.parallel._

@main def hello: Unit = {
  val x = Relation[Int](Set(1, 2, 3))
  x.addEdges((1, 2), (2, 3))

  val z = Graph(ParSet(1, 2, 3))
  println(z.toString)

  println(x.toString)

  val y = 6
}

def msg = "I was compiled by Scala 3. :)"
