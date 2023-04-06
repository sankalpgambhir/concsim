import concsim.base._
import concsim.program._

import java.util.concurrent.locks.ReadWriteLock
import scala.collection.mutable.Set
import scala.collection.parallel._

@main def main: Unit = {
  val x = Variable("x")
  val y = Variable("y")

  // simple ordering test
  val p = Program(
    Seq(
      Seq(Write(x, 3)),
      Seq(Write(x, 1), ReadWrite(x, 3, 4)),
      Seq(Write(x, 2), Read(x, 3))
    )
  )
  println(p)
  println(p.validUnder(SequentialConsistency))

  println("-----------------------------")
  // println("With lock")
  val r1 = Variable("r1") // register for thread 1
  val r2 = Variable("r2") // register for thread 2
  val l = Variable("l") // a lock variable

  val pLocal = Program(
    Seq(
      Seq(Lock(l), ReadWrite(x, None, r1, None), ReadWrite(r1, None, x, None, _ + 1), Unlock(l), Read(x, 2)),
      Seq(Lock(l), ReadWrite(x, None, r2, None), ReadWrite(r2, None, x, None, _ + 1), Unlock(l), Read(x, 1))
    )
  )

  println("-----------------------------")
  println(pLocal)
  println("Expect: Valid")
  println(pLocal.validUnder(SequentialConsistency))

  val pLocalError = Program(
    Seq(
      Seq(Lock(l), ReadWrite(x, None, r1, None), ReadWrite(r1, None, x, None, _ + 1), Unlock(l), Read(x, 1)),
      Seq(Lock(l), ReadWrite(x, None, r2, None), ReadWrite(r2, None, x, None, _ + 1), Unlock(l), Read(x, 1))
    )
  )

  println("-----------------------------")
  println(pLocalError)
  println("Expect: Invalid")
  println(pLocalError.validUnder(SequentialConsistency))

  val pLocalNoLock = Program(
    Seq(
      Seq(ReadWrite(x, None, r1, None), ReadWrite(r1, None, x, None, _ + 1), Read(x, 1)),
      Seq(ReadWrite(x, None, r2, None), ReadWrite(r2, None, x, None, _ + 1), Read(x, 1))
    )
  )

  println("-----------------------------")
  println(pLocalNoLock)
  println("Expect: Valid")
  println(pLocalNoLock.validUnder(SequentialConsistency))

  val unlockWithoutLock = Program(
    Seq(
      Seq(Unlock(l))
    )
  )

  println("-----------------------------")
  println(unlockWithoutLock)
  println("Expect: Invalid")
  println(unlockWithoutLock.validUnder(SequentialConsistency))

}
