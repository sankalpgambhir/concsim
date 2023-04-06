package concsim.program

sealed trait Instruction(val v: Variable)

sealed trait ReadInstruction(val vr: Variable, val r: Option[Int]) extends Instruction
sealed trait WriteInstruction(val vw: Variable, val w: Option[Int]) extends Instruction

class Read(vr: Variable, r: Option[Int]) extends ReadInstruction(vr, r) with Instruction(vr) {
  def this(v: Variable, r: Int) = this(v, Some(r))
  def this(v: Variable) = this(v, None)

  override def toString(): String = s"(rd: $v : ${r.getOrElse("?")})"
}

class Write(vw: Variable, val wConcrete: Int) extends WriteInstruction(vw, Some(wConcrete)) with Instruction(vw) {
  override def toString(): String = s"(wr: $v : ${w.get})"
}

class ReadWrite(vr: Variable, r: Option[Int], vw: Variable, w: Option[Int], val compute: Int => Int) extends ReadInstruction(vr, r) with WriteInstruction(vw, w) with Instruction(vw) {
  def this(vr: Variable, r: Option[Int], vw: Variable, w: Option[Int]) = this(vr, r, vw, w, x => x)
  def this(vr: Variable, r: Int, vw: Variable, w: Int) = this(vr, Some(r), vw, Some(w))
  def this(v: Variable, r: Int, w: Int) = this(v, Some(r), v, Some(w))
  def this(v: Variable, w: Int) = this(v, None, v, Some(w))

  override def toString(): String = s"(rw: (rd: $vr : ${r.getOrElse("?")}) : (wr: $vw : ${w.getOrElse(s"?($vr)")}))"
}

class Lock(val vl: Variable) extends ReadWrite(vl, 0, 1) {
  override def toString(): String = s"(lk: $v)"
}
class Unlock(val vl: Variable) extends ReadWrite(vl, 1, 0) {
  override def toString(): String = s"(unlk: $v)"
}
