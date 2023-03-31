package concsim.program

sealed abstract class Instruction(val v: Variable) {
  //
}

class Read(override val v: Variable, val r: Option[Int] = None) extends Instruction(v) {
  def this(v: Variable, r: Int) = this(v, Some(r))
  def this(v: Variable) = this(v, None)

  override def toString(): String = s"(rd: $v : ${r.getOrElse(None)})"
}

class Write(override val v: Variable, val w: Int) extends Instruction(v) {
  override def toString(): String = s"(wr: $v : $w)"
}

// TODO: See about RMWs
// case class RMW(val v: Variable, val r: ) extends Instruction {

// }
