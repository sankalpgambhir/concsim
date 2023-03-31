package concsim.program

import concsim.base.Relation
import collection.parallel.CollectionConverters.IterableIsParallelizable

type Order = Relation[Instruction]

sealed trait MemoryModel {

    /**
      * Program Order (po)
      */
    def po(p: Program): Order = {
        // actual program order
        val edgeList = p.events.map(l => (l zip l.tail).toSet).reduce(_++_).toSeq

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

    final def valid(p: Program): Boolean = hb(p).forall(_.isAcyclic)
}

case object SequentialConsistency extends MemoryModel {

    def rfWithWorklist(p: Program)(worklist: Seq[Read], currentOrder: Order = Relation(), currentHB: Order = po(p)): LazyList[Order] = {
        lazy val rd = worklist.head
        def possibleRFs: Seq[Write] =
            currentHB.objects.collect{case w: Write if (w.v == rd.v && (rd.r.isEmpty || rd.r.get == w.w)) => w}.toSeq
        
        if (worklist.isEmpty) currentOrder #:: LazyList.empty
        else possibleRFs.map(wr => rfWithWorklist(p)(worklist.tail, currentOrder.addEdges((wr, rd)), currentHB.addEdges((wr, rd)))).reduce(_ #::: _)
    }

    /**
      * Performs a stupid search to assign an rf order to the program
      * TODO: Not complete because it assigns an order to the reads
      *
      * @param p
      * @return
      */
    override def rf(p: Program): LazyList[Order] = {
        val unassignedReads = p.events.reduce(_ ++ _).collect{ case r: Read => r }

        rfWithWorklist(p)(unassignedReads)
    }

    override def hb(p: Program): LazyList[Order] = rf(p).map(po(p) U _)
}
