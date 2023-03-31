import concsim.base._
import concsim.program._

import scala.collection.mutable.Set
import scala.collection.parallel._

@main def hello: Unit = {
  val x = Variable("x")
  val p = Program(
    Seq(
      Seq(Read(x), Write(x, 2), Read(x, 0))
    )
  )

  println(p.validUnder(SequentialConsistency))
}

def msg = "I was compiled by Scala 3. :)"
