package de.tu_dresden.inf.lat
package axiomatization

import scala.jdk.CollectionConverters.*

object GraphSimulator {

  /**
   * This method computes the largest simulation from the source graph to the target graph.  More specifically, by
   * "simulation" we mean a forwardly-directed simulation, which describe the semantics of the description logic 𝓔𝓛 and
   * of its extension with simulation quantifiers or, respectively, greatest fixed-point semantics.  It is assumed that
   * the nodes in both graphs are consecutive numbers starting with 0.
   *
   * @param source the source graph
   * @param target the target graph
   * @param logger a logger that is used to process status messages during the computation
   * @return the largest simulation from the source to the target
   */
  @Deprecated(forRemoval = true)
  def computeMaximalSimulation[L, R](source: BitGraph[L, R], target: BitGraph[L, R])(using logger: Logger): ConcurrentArrayBitBiMap = {

    import de.tu_dresden.inf.lat.parallel.ParallelComputations.*

    logger.reset()
    logger.println("Computing pre-simulation...")
    val start1 = System.currentTimeMillis()
    val simulation = ConcurrentArrayBitBiMap(source.nodes().size, target.nodes().size)
    target.nodes().foreachPar(y => {
      logger.tick()
      val yLabels = target.labels(y)
      val yProperties = target.successorRelations(y)
      for (x <- source.nodes()) {
        if ((source.labels(x) subsetOf yLabels) && (source.successorRelations(x) subsetOf yProperties)) {
          simulation.add(x, y)
        }
      }
    })
    logger.reset()
    println("Phase 1: " + Util.formatTime(System.currentTimeMillis() - start1))

    logger.println("Computing initial mapping R(x,r)...")
    val start2 = System.currentTimeMillis()
    val R = java.util.concurrent.ConcurrentHashMap[Int, ConcurrentBitMap[R]]().asScala

    def addToR(x: Int, r: R, y: Int): Unit = {
      R.getOrElseUpdate(x, ConcurrentBitMap[R](target.nodes().size - 1)).add(r, y)
    }

    val predecessors = new scala.Array[collection.mutable.HashMap[R, collection.mutable.BitSet]](target.nodes().size)
    target.nodes().foreachPar(x => {
      predecessors(x) = collection.mutable.HashMap[R, collection.mutable.BitSet]()
      for ((r, y) <- target.predecessors(x)) {
        predecessors(x).getOrElseUpdate(r, collection.mutable.BitSet()).addOne(y)
      }
    })

    source.nodes().foreachPar(x => {
      logger.tick()
      R(x) = ConcurrentBitMap[R](target.nodes().size - 1)
      for (y <- simulation.row(x).viewAsImmutableBitSet) {
        for (r <- predecessors(y).keySet) {
          R(x).rowMap.getOrElseUpdate(r, concurrent.ConcurrentBoundedBitSet2(target.nodes().toBitMask))
            .viewAsMutableBitSet &~= predecessors(y)(r)
        }
      }
    })
    logger.reset()
    println("Phase 2: " + Util.formatTime(System.currentTimeMillis() - start2))

    logger.println("Computing simulation...")
    val start3 = System.currentTimeMillis()
    val nodesThatHaveACommonPredecessorWith = new scala.Array[collection.BitSet](source.nodes().size)
    source.nodes().foreachPar(node => {
      val predecessors = collection.mutable.BitSet()
      source.predecessors(node).foreach({ (_, pred) => predecessors.add(pred) })
      nodesThatHaveACommonPredecessorWith(node) =
        predecessors.flatMap(pred => source.successorRelations(pred).flatMap(rel => source.successorsForRelation(pred, rel)))
    })
    //logger.println("Common predecessor relation: " + Util.formatTime(System.currentTimeMillis() - start3))
    //    var continueMultiThreaded = true
    //    while (continueMultiThreaded && R.keySet.nonEmpty) {
    while (R.keySet.nonEmpty) {

      val futures = java.util.concurrent.ConcurrentLinkedQueue[scala.concurrent.Future[_]]()
      //val remainingNodes = collection.mutable.HashSet.from(R.keySet.iterator.take(4 * Runtime.getRuntime.availableProcessors()))
      //      val startA = System.currentTimeMillis()
      val remainingNodes = collection.mutable.HashSet.from(R.keySet)
      //      logger.println("HashSet.from: " + Util.formatTime(System.currentTimeMillis() - startA))
      //      val startB = System.currentTimeMillis()
      //      val _remainingNodes = {
      //        val len = ((graph.nodes().size - 1) >> 6) + 1
      //        val bitMask = new scala.Array[Long](len)
      //        (0 until len).foreachPar(idx => {
      //          val min = idx << 6
      //          val max = (idx + 1) << 6
      //          var mask = 0L
      //          (min to max).foreach(elem => {
      //            if (R.keySet contains elem) {
      //              mask |= 1L << elem
      //            }
      //          })
      //          bitMask(idx) = mask
      //        })
      //        collection.mutable.BitSet.fromBitMask(bitMask)
      //      }
      //      logger.println("BitSet.fromBitMask: " + Util.formatTime(System.currentTimeMillis() - startB))
      //val remainingNodes = collection.mutable.BitSet.fromSpecific(R.keySet)
      //      var parallelism = 0

      while (remainingNodes.nonEmpty) {
        val x = remainingNodes.head
        remainingNodes -= x
        remainingNodes --= nodesThatHaveACommonPredecessorWith(x)
        //remainingNodes &~= nodesThatHaveACommonPredecessorWith(x)
        //        parallelism += 1
        futures.add(scala.concurrent.Future {
          val Rx = R.remove(x).get
          Rx.rows().foreach({ r =>
            logger.tick()
            // TODO: Use Java's Fork/Join Framework for additional speed-up
            source.predecessorsForRelation(x, r).foreach { xx =>
              val simRow = simulation.row(xx)
              val simRowView = simRow.viewAsImmutableBitSet
              (Rx.rowImmutable(r) intersect simRowView).foreach { yy =>
                simRow.remove(yy)
                simulation.col(yy).remove(xx)
                target.predecessors(yy).foreach { (rr, yyy) =>
                  if ((simRowView intersect target.successorsForRelation(yyy, rr)).isEmpty) {
                    addToR(xx, rr, yyy)
                  }
                }
              }
            }
          })
        })
      }

      futures.asScala.awaitEach()

      //      continueMultiThreaded = parallelism > 2

    }

    //    logger.println()
    //    logger.println("Now continuing single-threaded...")
    //    while (R.keySet.nonEmpty) {
    //      val x = R.keySet.head
    //      // TODO: Define a method for the following code as it is a duplicate of the above code in the Future.
    //      val Rx = R.remove(x).get
    //      Rx.rows().foreach({ r =>
    //        logger.tick()
    //        for (xx <- graph.predecessorsForRelation(x, r)) {
    //          val simRow = simulation.row(xx)
    //          val simRowView = simRow.viewAsImmutableBitSet
    //          for (yy <- Rx.rowImmutable(r) intersect simRowView) {
    //            simRow.remove(yy)
    //            simulation.col(yy).remove(xx)
    //            for ((rr, yyy) <- graph.predecessors(yy)) {
    //              val successors = graph.successorsForRelation(yyy, rr)
    //              if ((simRowView intersect successors).isEmpty) {
    //                addToR(xx, rr, yyy)
    //              }
    //            }
    //          }
    //        }
    //      })
    //    }

    logger.reset()
    println("Phase 3: " + Util.formatTime(System.currentTimeMillis() - start3))

    simulation

  }

  /**
   * This method computes the largest simulation from the source graph to the target graph.  More specifically, by
   * "simulation" we mean a forwardly-directed simulation, which describe the semantics of the description logic 𝓔𝓛 and
   * of its extension with simulation quantifiers or, respectively, greatest fixed-point semantics.  It is assumed that
   * the nodes in both graphs are consecutive numbers starting with 0.
   *
   * @param source the source graph
   * @param target the target graph
   * @param logger a logger that is used to process status messages during the computation
   * @return the largest simulation from the source to the target
   */
  @Deprecated(forRemoval = true)
  def computeMaximalSimulation2[L, R](source: BitGraph[L, R], target: BitGraph[L, R])(using logger: Logger): ConcurrentArrayBitBiMap = {

    import de.tu_dresden.inf.lat.parallel.NestedParallelComputations.*

    logger.reset()
    logger.println("Computing pre-simulation...")
    val start1 = System.currentTimeMillis()
    val simulation = ConcurrentArrayBitBiMap(source.nodes().size, target.nodes().size)
    target.nodes().foreachPar(y => {
      logger.tick()
      val yLabels = target.labels(y)
      val yProperties = target.successorRelations(y)
      for (x <- source.nodes()) {
        if ((source.labels(x) subsetOf yLabels) && (source.successorRelations(x) subsetOf yProperties)) {
          simulation.add(x, y)
        }
      }
    })
    logger.reset()
    println("Phase 1: " + Util.formatTime(System.currentTimeMillis() - start1))

    logger.println("Computing initial mapping R(x,r)...")
    val start2 = System.currentTimeMillis()
    val R = java.util.concurrent.ConcurrentHashMap[Int, ConcurrentBitMap[R]]().asScala

    def addToR(x: Int, r: R, y: Int): Unit = {
      R.getOrElseUpdate(x, ConcurrentBitMap[R](target.nodes().size - 1)).add(r, y)
    }

    val predecessors = new scala.Array[collection.mutable.HashMap[R, collection.mutable.BitSet]](target.nodes().size)
    target.nodes().foreachPar(x => {
      predecessors(x) = collection.mutable.HashMap[R, collection.mutable.BitSet]()
      for ((r, y) <- target.predecessors(x)) {
        predecessors(x).getOrElseUpdate(r, collection.mutable.BitSet()).addOne(y)
      }
    })

    source.nodes().foreachPar(x => {
      logger.tick()
      R(x) = ConcurrentBitMap[R](target.nodes().size - 1)
      for (y <- simulation.row(x).viewAsImmutableBitSet) {
        for (r <- predecessors(y).keySet) {
          R(x).rowMap.getOrElseUpdate(r, concurrent.ConcurrentBoundedBitSet2(target.nodes().toBitMask))
            .viewAsMutableBitSet &~= predecessors(y)(r)
        }
      }
    })
    logger.reset()
    println("Phase 2: " + Util.formatTime(System.currentTimeMillis() - start2))

    logger.println("Computing simulation...")
    val start3 = System.currentTimeMillis()
    val nodesThatHaveACommonPredecessorWith = new scala.Array[collection.BitSet](source.nodes().size)
    source.nodes().foreachPar(node => {
      val predecessors = collection.mutable.BitSet()
      source.predecessors(node).foreach({ (_, pred) => predecessors.add(pred) })
      nodesThatHaveACommonPredecessorWith(node) =
        predecessors.flatMap(pred => source.successorRelations(pred).flatMap(rel => source.successorsForRelation(pred, rel)))
    })

    while (R.keySet.nonEmpty) {

      val futures = java.util.concurrent.ConcurrentLinkedQueue[java.util.concurrent.RecursiveAction]()
      val remainingNodes = collection.mutable.HashSet.from(R.keySet)

      while (remainingNodes.nonEmpty) {
        val x = remainingNodes.head
        remainingNodes -= x
        remainingNodes --= nodesThatHaveACommonPredecessorWith(x)
        val future: java.util.concurrent.RecursiveAction = () => {
          val Rx = R.remove(x).get
          Rx.rows().foreach({ r =>
            logger.tick()
            source.predecessorsForRelation(x, r).foreach { xx =>
              val simRow = simulation.row(xx)
              val simRowView = simRow.viewAsImmutableBitSet
              (Rx.rowImmutable(r) intersect simRowView).foreach { yy =>
                simRow.remove(yy)
                simulation.col(yy).remove(xx)
                target.predecessors(yy).foreach { (rr, yyy) =>
                  if ((simRowView intersect target.successorsForRelation(yyy, rr)).isEmpty) {
                    addToR(xx, rr, yyy)
                  }
                }
              }
            }
          })
        }
        futures.add(future)
        FORK_JOIN_POOL.execute(future)
      }

      // futures.asScala.foreach(_.quietlyJoin())
      futures.forEach(_.quietlyJoin())

    }

    logger.reset()
    println("Phase 3: " + Util.formatTime(System.currentTimeMillis() - start3))

    simulation

  }

  /**
   * This method computes the largest simulation from the source graph to the target graph.  More specifically, by
   * "simulation" we mean a forwardly-directed simulation, which describe the semantics of the description logic 𝓔𝓛 and
   * of its extension with simulation quantifiers or, respectively, greatest fixed-point semantics.  It is assumed that
   * the nodes in both graphs are consecutive numbers starting with 0.
   *
   * @param source the source graph
   * @param target the target graph
   * @param logger a logger that is used to process status messages during the computation
   * @return the largest simulation from the source to the target
   */
  def computeMaximalSimulation3[L, R](source: BitGraph[L, R], target: BitGraph[L, R])(using logger: Logger): ConcurrentArrayBitBiMap = {
    import de.tu_dresden.inf.lat.parallel.NestedParallelComputations.*
    val nodesThatHaveACommonPredecessorWith = new scala.Array[collection.BitSet](source.nodes().size)
    source.nodes().foreachPar(node => {
      val predecessors = collection.mutable.BitSet()
      source.predecessors(node).foreach({ (_, pred) => predecessors.add(pred) })
      nodesThatHaveACommonPredecessorWith(node) =
        predecessors.flatMap(pred => source.successorRelations(pred).flatMap(rel => source.successorsForRelation(pred, rel)))
    })
    computeMaximalSimulation3[Int, L, R, collection.mutable.BitSet, BitGraph[L, R], ConcurrentArrayBitBiMap](
      source, target, nodesThatHaveACommonPredecessorWith,
      () => ConcurrentArrayBitBiMap(source.nodes().size, target.nodes().size),
      (simulation, xx, yy) => { simulation.col(yy).remove(xx) })
  }

  /**
   * This method computes the largest simulation from the source graph to the target graph.  More specifically, by
   * "simulation" we mean a forwardly-directed simulation, which describe the semantics of the description logic 𝓔𝓛 and
   * of its extension with simulation quantifiers or, respectively, greatest fixed-point semantics.  It is assumed that
   * the nodes in both graphs are consecutive numbers starting with 0.
   *
   * @param source the source graph
   * @param target the target graph
   * @param logger a logger that is used to process status messages during the computation
   * @return the largest simulation from the source to the target
   */
  def computeMaximalSimulation3[L, R](source: HashGraph[collection.BitSet, L, R], target: BitGraph[L, R])(using logger: Logger): ConcurrentBitSetToIntRelation = {
    import de.tu_dresden.inf.lat.parallel.NestedParallelComputations.*
    val nodesThatHaveACommonPredecessorWith = collection.mutable.HashMap[collection.BitSet, collection.mutable.HashSet[collection.BitSet]]()
    source.nodes().foreachPar(node => {
      val predecessors = collection.mutable.HashSet[collection.BitSet]()
      source.predecessors(node).foreach({ (_, pred) => predecessors.add(pred) })
      nodesThatHaveACommonPredecessorWith(node) =
        predecessors.flatMap(pred => source.successorRelations(pred).flatMap(rel => source.successorsForRelation(pred, rel)))
    })
    computeMaximalSimulation3[collection.BitSet, L, R, collection.mutable.HashSet[collection.BitSet], HashGraph[collection.BitSet, L, R], ConcurrentBitSetToIntRelation](
      source, target, nodesThatHaveACommonPredecessorWith,
      () => ConcurrentBitSetToIntRelation(target.nodes().max),
      (_, _, _) => {})
  }

  /**
   * This method computes the largest simulation from the source graph to the target graph.  More specifically, by
   * "simulation" we mean a forwardly-directed simulation, which describe the semantics of the description logic 𝓔𝓛 and
   * of its extension with simulation quantifiers or, respectively, greatest fixed-point semantics.  It is assumed that
   * the nodes in both graphs are consecutive numbers starting with 0.
   *
   * @param source the source graph
   * @param target the target graph
   * @param logger a logger that is used to process status messages during the computation
   * @return the largest simulation from the source to the target
   */
  def computeMaximalSimulation3[N,
                                L,
                                R,
                                SetN <: collection.mutable.Set[N],
                                G <: Graph[N, L, R, SetN],
                                S <: ConcurrentToIntRelation[N]]
  (source: G, target: BitGraph[L, R], nodesThatHaveACommonPredecessorWith: N => collection.Set[N], newSimulation: () => S, removeFromColumn: (S, N, Int) => Unit)(using logger: Logger): S = {

    import de.tu_dresden.inf.lat.parallel.NestedParallelComputations.*

    logger.reset()
    logger.println("Computing pre-simulation...")
    val start1 = System.currentTimeMillis()
    val simulation = newSimulation()
    target.nodes().foreachPar(y => {
      logger.tick()
      val yLabels = target.labels(y)
      val yProperties = target.successorRelations(y)
      for (x <- source.nodes()) {
        if ((source.labels(x) subsetOf yLabels) && (source.successorRelations(x) subsetOf yProperties)) {
          simulation.add(x, y)
        }
      }
    })
    logger.reset()
    logger.println("Phase 1: " + Util.formatTime(System.currentTimeMillis() - start1))

    logger.println("Computing initial mapping R(x,r)...")
    val start2 = System.currentTimeMillis()
    val R = java.util.concurrent.ConcurrentHashMap[N, ConcurrentBitMap[R]]().asScala

    def addToR(x: N, r: R, y: Int): Unit = {
      R.getOrElseUpdate(x, ConcurrentBitMap[R](target.nodes().size - 1)).add(r, y)
    }

    val predecessors = new scala.Array[collection.mutable.HashMap[R, collection.mutable.BitSet]](target.nodes().size)
    target.nodes().foreachPar(x => {
      predecessors(x) = collection.mutable.HashMap[R, collection.mutable.BitSet]()
      for ((r, y) <- target.predecessors(x)) {
        predecessors(x).getOrElseUpdate(r, collection.mutable.BitSet()).addOne(y)
      }
    })

    source.nodes().foreachPar(x => {
      logger.tick()
      R(x) = ConcurrentBitMap[R](target.nodes().size - 1)
      for (y <- simulation.row(x).viewAsImmutableBitSet) {
        for (r <- predecessors(y).keySet) {
          R(x).rowMap.getOrElseUpdate(r, concurrent.ConcurrentBoundedBitSet2(target.nodes().toBitMask))
            .viewAsMutableBitSet &~= predecessors(y)(r)
        }
      }
    })
    logger.reset()
    logger.println("Phase 2: " + Util.formatTime(System.currentTimeMillis() - start2))

    logger.println("Computing simulation...")
    val start3 = System.currentTimeMillis()

    while (R.keySet.nonEmpty) {

      val futures = java.util.concurrent.ConcurrentLinkedQueue[java.util.concurrent.RecursiveAction]()
      val remainingNodes = collection.mutable.HashSet.from(R.keySet)

      while (remainingNodes.nonEmpty) {
        val x = remainingNodes.head
        remainingNodes -= x
        remainingNodes --= nodesThatHaveACommonPredecessorWith(x)
        val future: java.util.concurrent.RecursiveAction = () => {
          val Rx = R.remove(x).get
          Rx.rows().foreach({ r =>
            logger.tick()
            source.predecessorsForRelation(x, r).foreach { xx =>
              val simRow = simulation.row(xx)
              val simRowView = simRow.viewAsImmutableBitSet
              (Rx.rowImmutable(r) intersect simRowView).foreach { yy =>
                simRow.remove(yy)
                removeFromColumn(simulation, xx, yy)
                target.predecessors(yy).foreach { (rr, yyy) =>
                  if ((simRowView intersect target.successorsForRelation(yyy, rr)).isEmpty) {
                    addToR(xx, rr, yyy)
                  }
                }
              }
            }
          })
        }
        futures.add(future)
        FORK_JOIN_POOL.execute(future)
      }

      futures.forEach(_.quietlyJoin())

    }

    logger.reset()
    logger.println("Phase 3: " + Util.formatTime(System.currentTimeMillis() - start3))

    simulation

  }

}
