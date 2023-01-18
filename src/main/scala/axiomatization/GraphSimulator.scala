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
  def computeMaximalSimulation[L, R](source: BitGraph[L, R], target: BitGraph[L, R])(using logger: Logger): ConcurrentArrayBitBiMap = {
    import de.tu_dresden.inf.lat.parallel.NestedParallelComputations.*
    val nodesThatHaveACommonPredecessorWith = new scala.Array[collection.BitSet](source.nodes().size)
    source.nodes().foreachPar(node => {
      val predecessors = collection.mutable.BitSet()
      source.predecessors(node).foreach({ (_, pred) => predecessors.add(pred) })
      nodesThatHaveACommonPredecessorWith(node) =
        predecessors.flatMap(pred => source.successorRelations(pred).flatMap(rel => source.successorsForRelation(pred, rel)))
    })
    computeMaximalSimulation[Int, L, R, collection.mutable.BitSet, BitGraph[L, R], ConcurrentArrayBitBiMap](
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
  def computeMaximalSimulation[L, R](source: HashGraph[collection.BitSet, L, R], target: BitGraph[L, R])(using logger: Logger): ConcurrentBitSetToIntRelation = {
    import de.tu_dresden.inf.lat.parallel.NestedParallelComputations.*
    val nodesThatHaveACommonPredecessorWith = java.util.concurrent.ConcurrentHashMap[collection.BitSet, collection.mutable.HashSet[collection.BitSet]]().asScala
    source.nodes().foreachPar(node => {
      val predecessors = collection.mutable.HashSet[collection.BitSet]()
      source.predecessors(node).foreach({ (_, pred) => predecessors.add(pred) })
      nodesThatHaveACommonPredecessorWith(node) =
        predecessors.flatMap(pred => source.successorRelations(pred).flatMap(rel => source.successorsForRelation(pred, rel)))
    })
    computeMaximalSimulation[collection.BitSet, L, R, collection.mutable.HashSet[collection.BitSet], HashGraph[collection.BitSet, L, R], ConcurrentBitSetToIntRelation](
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
  def computeMaximalSimulation[N,
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

    source.nodes().foreachPar(x => {
      logger.tick()
      R(x) = ConcurrentBitMap[R](target.nodes().size - 1)
      for (y <- simulation.row(x).viewAsImmutableBitSet) {
        for (r <- target.predecessorRelations(y)) {
          R(x).rowMap.getOrElseUpdate(r, concurrent.ConcurrentBoundedBitSet(target.nodes().toBitMask))
            .viewAsMutableBitSet &~= target.predecessorsForRelation(y, r)
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
