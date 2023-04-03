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
    * Dumb rf generation with minimal pruning ((po U rf)+)
    *
    * @param p the program
    * @param worklist the list of currently unassigned reads
    * @param currentOrder currently accumulated rf order
    * @param currentHB currently accumulated hb order (assumed (po U rf)+ by default) 
    * @return
    */
  def rfWithWorklist(p: Program)(worklist: Seq[Read], currentOrder: Order = Relation(), currentHB: Order = po(p)): LazyList[Order] = {
    lazy val rd = worklist.head
    lazy val possibleRFs: Iterable[Write] =
      currentHB.objects.collect {
        case wr: Write
            if (
              wr.v == rd.v &&
              rd.r.isEmpty || rd.r.get == wr.w &&
              (!currentHB.reachable(rd, wr)) // is this right in general? Since it assumes (po U rf)+ reachability for selecting rewrites
              // just remove it if it seems wrong, it is technically just pruning; can change to just po reachability by removing currentHB[.withEdges...] below
            ) =>
          wr
      }

    if (worklist.isEmpty) currentOrder #:: LazyList.empty
    else if (possibleRFs.isEmpty)
      // invalid branch
      LazyList.empty
    else possibleRFs.map(wr => rfWithWorklist(p)(worklist.tail, currentOrder.withEdges(wr -> rd), currentHB.withEdges(wr -> rd))).reduce(_ #::: _)
  }

  /**
   * Reads-From (rf)
   *
   * Currently uses [[rfWithWorklist]] to perform a straightforward search for
   * generation. The assumption of an order of reads (using `Seq[Read]`) would
   * make this incomplete. We solve this by... checking all permutations. Since
   * it is evaluated lazily, it is not so bad if there is actually an error. It
   * is pretty bad if you actually have `Invalid` behaviour though.
   *
   * Can be improved quite a lot by implementing better rf-equivalence checking
   * and equivalence-driven generation for Mazurkiewicz traces.
   *
   * e.g. overkill: Optimal Stateless Model Checking for Reads-From Equivalence under
   * Sequential Consistency [[https://dl.acm.org/doi/pdf/10.1145/3360576]]
   *
   * @return stream of possible `rf` orders
   */
  def rf(p: Program): LazyList[Order] = {
    val reads = p.events.reduce(_ ++ _).collect { case r: Read => r }

    reads.permutations.map(rfWithWorklist(p)(_)).reduce(_ #::: _)
  }

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

  /**
    * Internal function to obtain rf (with partial mo) with some pruning.
    * Modified version of [[rfWithWorklist]]. See [[rf]] and [[rfWithWorklist]]
    * for details.
    */
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
