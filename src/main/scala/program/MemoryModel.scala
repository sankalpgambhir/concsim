package concsim.program

import concsim.base.Relation

type Order = Relation[Instruction]

sealed trait MemoryModel {

  sealed trait ProgramJudgement
  case class Valid(witness: Option[Order]) extends ProgramJudgement
  case object Invalid extends ProgramJudgement

  /**
   * Program Order (po)
   */
  def po(p: Program): Order = {
    // actual program order
    val edgeList = p.events.map(l => (l zip l.tail)).reduce(_ ++ _)

    // initialisation events come before the first of every thread
    val initList = p.initialisations.map(init => p.events.collect(_.head).map((init, _))).reduce(_ ++ _)

    Relation[Instruction]().addEdges((edgeList ++ initList): _*)
  }

  /**
   * Reads-From (rf)
   * @return stream of possible `rf` orders
   */
  def rf(p: Program): LazyList[Order]

  /**
   * Happens-Before (hb)
   * Memory models may implement custom relations to compute this
   * @return stream of possible `hb` orders
   */
  def hb(p: Program): LazyList[Order]

  final def valid(p: Program): this.ProgramJudgement =
    val ord = hb(p)
    ord match
      case LazyList() => Invalid
      case _ =>
        ord.find(_.isAcyclic) match
          case None => Invalid
          case Some(witness) => Valid(Some(witness))
}

case object SequentialConsistency extends MemoryModel {

  def moRfWithWorklist(p: Program)(worklist: Seq[Read], currentOrder: Order = Relation(), currentHB: Order = po(p)): LazyList[Order] = {
    lazy val rd = worklist.head
    lazy val possibleRFsByOrder: Iterable[Write] =
      currentHB.objects.collect {
        case wr: Write
            if (
              wr.v == rd.v &&
                (!currentHB.reachable(rd, wr)) &&
                !currentHB.objects.exists {
                  case wr2: Write => wr.v == wr2.v && currentHB.reachable(wr, wr2) && currentHB.reachable(wr2, rd)
                  case _ => false
                }
            ) =>
          wr
      }
    lazy val possibleRFs = possibleRFsByOrder.filter(rd.r.isEmpty || rd.r.get == _.w)
    def newEdges(wr: Instruction): Seq[(Instruction, Instruction)] = Seq((wr -> rd)) ++ possibleRFsByOrder.filterNot(_ == wr).map(_ -> wr)

    if (worklist.isEmpty) currentOrder #:: LazyList.empty
    else if (possibleRFs.isEmpty)
      // invalid branch
      LazyList.empty
    else possibleRFs.map(wr => moRfWithWorklist(p)(worklist.tail, currentOrder.withEdges(newEdges(wr): _*), currentHB.withEdges(newEdges(wr): _*))).reduce(_ #::: _)
  }

  /**
   * Performs a (not so) stupid search to assign an rf order to the program.
   * Also optimized to insert a partial mo into the rf order. 
   *
   * Really stupidly checks all possible permutations to find errors. If your
   * behaviour is really invalid, this is quite annoying.
   *
   * With the mo-optimization instead of a separate complete computation, I'm
   * also not sure if it is complete modulo order of reads. But I think it
   * produces a partial-mo that is "total" for this RF, i.e. any total extension
   * of it should not affect cyclicity. I do not currently have a proof of this.
   *
   * @param p the program
   * @return an rf order (U mo)
   */
  override def rf(p: Program): LazyList[Order] = {
    val reads = p.events.reduce(_ ++ _).collect { case r: Read => r }

    reads.permutations.map(moRfWithWorklist(p)(_)).reduce(_ #::: _)
  }

  override def hb(p: Program): LazyList[Order] = rf(p).map(po(p) U _)
}
