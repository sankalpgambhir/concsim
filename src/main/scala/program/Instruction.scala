package concsim.program

sealed trait Instruction(val v: Variable) 

sealed trait ReadInstruction(override val v: Variable, val r: Option[Int]) extends Instruction
sealed trait WriteInstruction(override val v: Variable, val w: Int) extends Instruction

class Read(val vr: Variable, override val r: Option[Int] = None) extends ReadInstruction(vr, r) with Instruction(vr) {
  def this(v: Variable, r: Int) = this(v, Some(r))
  def this(v: Variable) = this(v, None)

  override def toString(): String = s"(rd: $v : ${r.getOrElse("?")})"
}

class Write(val vw: Variable, override val w: Int) extends WriteInstruction(vw, w) with Instruction(vw) {
  override def toString(): String = s"(wr: $v : $w)"
}

// TODO: See about RMWs
// RMWs can be cross variable
// TODO: fix this
class RMW(val vrw: Variable, override val r: Option[Int], override val w: Int) extends ReadInstruction(vrw, r) with WriteInstruction(vrw, w) with Instruction(vrw) {
  def this(v: Variable, r: Int, w: Int) = this(v, Some(r), w)
  def this(v: Variable, w: Int) = this(v, None, w)

  override def toString(): String = s"(rmw: $vrw : ${r.getOrElse("?")} : $w)"
}

class Lock(val vl: Variable) extends RMW(vl, 0, 1) {
  override def toString(): String = s"(lk: $v)"
}
class Unlock(val vl: Variable) extends RMW(vl, 1, 0) {
  override def toString(): String = s"(unlk: $v)"
}
