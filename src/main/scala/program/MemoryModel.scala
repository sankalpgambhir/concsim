package concsim.program

import scala.collection.mutable.{Map as MMap}
import concsim.base.Relation
import escritoire.{Tabulation, Heading}

type Order = Relation[Instruction]

sealed trait MemoryModel {

  sealed trait ProgramJudgement
  case class Valid(witness: Order, program: Program) extends ProgramJudgement {
    // this currently has a strong linearization requirement
    // and infers read values using the topo sort provided
    // rushing this to have an SC system working
    // TODO: make this generic
    override def toString(): String = {
      val linearizedWithoutVals = witness.linearized

      val linearizedWithInits = {
        var lin = Seq[(Instruction, Instruction)]()
        val vals = MMap(program.variables.map(_ -> 0).toSeq: _*)

        for (i <- linearizedWithoutVals)
          i match {
            case rd: Read => lin = lin :+ (i, Read(rd.vr, rd.r.getOrElse(vals(rd.vr))))
            case _: Lock => lin = lin :+ (i, i) // if you don't have a special case, it gets rewritten to an RMW!
            case _: Unlock => lin = lin :+ (i, i)
            case rm: ReadWrite => {
              val curr = vals(rm.vr)
              val writeVal = if (rm.w.isDefined) then rm.w.get else rm.compute(curr)
              vals(rm.vw) = writeVal
              lin = lin :+ (i, ReadWrite(rm.vr, Some(rm.r.getOrElse(curr)), rm.vw, Some(writeVal)))
            }
            case wr: Write => vals(wr.vw) = wr.w.get; lin = lin :+ (i, i)
          }

        lin
      }

      val linearized = linearizedWithInits.drop(program.variables.size)

      val t = Tabulation(
        program.events.zipWithIndex.map((t, i) => 
          Heading(s"th$i", getter = ((r: (Instruction, Instruction)) => if (t.exists(_ == r._1)) then r._2.toString() else ""))
        ): _*
      ).tabulate(1000, linearized)

      val trace = t.reduce(_ ++ "\n" ++ _)
      s"Valid: Trace found\n$trace"
    }

  }
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
  def rfWithWorklist(p: Program)(worklist: Seq[ReadInstruction], currentOrder: Order = Relation(), currentHB: Order = po(p)): LazyList[Order] = {
    lazy val rd = worklist.head
    lazy val possibleRFs: Iterable[WriteInstruction] =
      currentHB.objects.collect { i =>
        i match {
          case wr: WriteInstruction
              if (
                wr.vw == rd.vr &&
                rd.r.isEmpty || wr.w.isEmpty || rd.r.get == wr.w.get &&
                (!currentHB.reachable(rd, wr)) // is this right in general? Since it assumes (po U rf)+ reachability for selecting rewrites
                // just remove it if it seems wrong, it is technically just pruning; can change to just po reachability by removing currentHB[.withEdges...] below
              ) => wr
        }
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
    val reads = p.events.reduce(_ ++ _).collect { i => i match { case r: ReadInstruction => r } }

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
          case Some(witness) => Valid(witness, p)
}

case object SequentialConsistency extends MemoryModel {

  /**
    * After all reads and rmws have been assigned sources, check that ambiguously
    * chosen writes actually compute to the required value.
    *
    * @param rford the rf order
    * @return whether rf is well formed wrt RMW assignments
    */
  def validRMWReads(rford: Order): Boolean = {
    if (rford.isAcyclic) {
      val reads = rford.objects.collect{case r: ReadInstruction => r}
      val readVals = MMap[ReadInstruction, Int]()

      def getReadVal_(r: ReadInstruction): Option[Int] = {
        // well constructed rf guarantees this exists
        val wr = rford.objects.find(rford.relates(_, r)).get

        // pain
        wr match {
          case w: Write => 
            if (r.r.isEmpty || r.r.get == w.w.get)
              readVals(r) = w.w.get
              w.w
            else None
          case w: ReadWrite =>
            // if it is an RMW
            // if the value is available, match it
            // else, compute and match it
            if (w.w.isDefined)
              if (r.r.isEmpty || r.r.get == w.w.get)
                readVals(r) = w.w.get
                w.w
              else None
            else if (w.r.isDefined)
              val newW = w.compute(w.r.get)
              if (r.r.isEmpty || r.r.get == newW)
                readVals(r) = newW
                Some(newW)
              else None
            else
              // get r and then compute
              val inner = getReadVal(w)
              if (inner.isDefined)
                val newW = w.compute(inner.get)
                if (r.r.isEmpty || r.r.get == newW)
                  readVals(r) = newW
                  Some(newW)
                else None
              else None
          case _ => None
        }
      }

      def getReadVal(r: ReadInstruction): Option[Int] = if readVals.contains(r) then Some(readVals(r)) else getReadVal_(r)

      reads.map(getReadVal).forall(_.isDefined)
    }
    else false
  }

  /**
    * Internal function to obtain rf (with partial mo) with some pruning.
    * Modified version of [[rfWithWorklist]]. See [[rf]] and [[rfWithWorklist]]
    * for details.
    */
  def moRfWithWorklist(p: Program)(worklist: Seq[ReadInstruction], currentRF: Order = Relation(), currentMO: Order = Relation(), currentHB: Order = po(p)): LazyList[(Order, Order)] = {
    lazy val rd = worklist.head
    lazy val possibleRFsByOrder: Iterable[WriteInstruction] =
      currentHB.objects.collect {
        case wr: (WriteInstruction)
            if (
              wr.vw == rd.vr &&
                (!currentHB.reachable(rd, wr)) &&
                !currentHB.objects.exists {
                  case wr2: Write => wr.vw == wr2.vw && currentHB.reachable(wr, wr2) && currentHB.reachable(wr2, rd)
                  case _ => false
                }
            ) =>
          wr
      }
    lazy val possibleRFs = possibleRFsByOrder.filter(wr => rd.r.isEmpty || wr.w.isEmpty || rd.r.get == wr.w.get)
    def newEdges(wr: Instruction): Seq[(Instruction, Instruction)] = possibleRFsByOrder.filter(currentHB.reachable(_, rd)).filterNot(_ == wr).map(_ -> wr).toSeq

    if (worklist.isEmpty) (currentRF, currentMO) #:: LazyList.empty
    else if (possibleRFs.isEmpty)
      // invalid branch
      LazyList.empty
    else possibleRFs.map(wr => moRfWithWorklist(p)(worklist.tail, currentRF.withEdges(wr -> rd), currentMO.withEdges(newEdges(wr): _*), currentHB.withEdges(newEdges(wr): _*))).reduce(_ #::: _)
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
    val reads = p.events.reduce(_ ++ _).collect { case r: ReadInstruction => r }

    // TODO: remove take?
    reads.permutations.take(1).map(moRfWithWorklist(p)(_)).reduce(_ #::: _).collect{case (r, m) if validRMWReads(r) => r U m}
  }

  override def hb(p: Program): LazyList[Order] = rf(p).map(po(p) U _)
}
