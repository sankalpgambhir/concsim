package concsim.program

sealed abstract class Instruction (val v: Variable) {
    //
}

class Read(override val v: Variable, val r: Option[Int] = None) extends Instruction(v) {
    def this(v: Variable, r: Int) = this(v, Some(r))
    def this(v: Variable) = this(v, None)
}

class Write(override val v: Variable, val w: Int) extends Instruction(v) {

}

// TODO: See about RMWs
// case class RMW(val v: Variable, val r: ) extends Instruction {

// }
