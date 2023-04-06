package concsim.program

import escritoire.Heading
import escritoire.Tabulation

class Program(val events: Seq[Seq[Instruction]]) {

  val variables: Set[Variable] = events.reduce(_ ++ _).map(_.v).toSet
  val initialisations: Seq[Instruction] = variables.map(Write(_, 0)).toSeq

  def validUnder(model: MemoryModel) = model.valid(this)

  override def toString(): String = {
    val eventStringsUneven = events.map(_.map(_.toString()))
    val maxLen = events.map(_.length).max

    val eventStrings: Seq[Seq[String]] = eventStringsUneven.map(_.padTo(maxLen, ""))

    val t = Tabulation(
      events.zipWithIndex.map((t, i) => Heading(s"th$i", getter = ((r: Seq[String]) => r(i)))): _*
    ).tabulate(1000, eventStrings.transpose)

    "Program\n" ++ t.reduce(_ ++ "\n" ++ _)
  }
}
