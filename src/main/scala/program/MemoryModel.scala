package concsim.program

import concsim.base.Relation
import collection.parallel.CollectionConverters.IterableIsParallelizable

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
        val edgeList = p.events.map(l => (l zip l.tail)).reduce(_++_)

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

    def rfWithWorklist(p: Program)(worklist: Seq[Read], currentOrder: Order = Relation(), currentHB: Order = po(p)): LazyList[Order] = {
        lazy val rd = worklist.head
        lazy val possibleRFs: Seq[Write] = 
            currentHB.objects.collect{
                case wr: Write
                    if (
                        wr.v == rd.v &&
                        (rd.r.isEmpty || rd.r.get == wr.w) && 
                        (!currentHB.reachable(rd, wr)) && 
                        !currentHB.objects.exists{
                            case wr2: Write => wr.v == wr2.v && currentHB.reachable(wr, wr2) && currentHB.reachable(wr2, rd) 
                            case _ => false
                        }
                    ) => wr
            }.toSeq
        
        if (worklist.isEmpty) currentOrder #:: LazyList.empty
        else if (possibleRFs.isEmpty)
            // invalid branch
            LazyList.empty
        else possibleRFs.map(wr => rfWithWorklist(p)(worklist.tail, currentOrder.withEdges((wr, rd)), currentHB.withEdges((wr, rd)))).reduce(_ #::: _)
    }

    /**
      * Performs a stupid search to assign an rf order to the program
      * TODO: Not complete because it assigns an order to the reads
      *
      * @param p the program
      * @return
      */
    override def rf(p: Program): LazyList[Order] = {
        val reads = p.events.reduce(_ ++ _).collect{ case r: Read => r }

        rfWithWorklist(p)(reads)
    }

    override def hb(p: Program): LazyList[Order] = rf(p).map(po(p) U _)
}
