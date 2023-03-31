package concsim.program

class Program(val events: Seq[Seq[Instruction]]) {

  val variables: Set[Variable] = events.reduce(_ ++ _).map(_.v).toSet
  val initialisations: Seq[Instruction] = variables.map(Write(_, 0)).toSeq

  def validUnder(model: MemoryModel) = model.valid(this)
}
