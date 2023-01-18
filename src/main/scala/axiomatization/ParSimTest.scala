package de.tu_dresden.inf.lat
package axiomatization

import org.semanticweb.owlapi.model.{OWLClass, OWLObjectProperty}
import de.tu_dresden.inf.lat.parallel.ParallelComputations
import de.tu_dresden.inf.lat.parallel.ParallelComputations.*
import de.tu_dresden.inf.lat.axiomatization.GraphSimulator._

import scala.jdk.CollectionConverters.*
import scala.annotation.tailrec

@Deprecated(forRemoval = true)
object ParSimTest {

  def main(args: Array[String]): Unit = {

    print("Verbose logging? ")
    val verbose = scala.io.StdIn.readBoolean()
    given logger: Logger = if verbose then ConsoleLogger() else NoLogger()

    print("Which ontology? ")
    val ont = scala.io.StdIn.readInt()
    val manager = org.semanticweb.owlapi.apibinding.OWLManager.createOWLOntologyManager()
    //val ontology = manager.loadOntologyFromOntologyDocument(java.io.File("ore2015_pool_sample_experiments/files/ore_ont_" + ont + "_reduced.owl"))
    val ontology = manager.loadOntologyFromOntologyDocument(java.io.File("ore2015_pool_sample/files/ore_ont_" + ont + ".owl"))
    val graph = Interpretation.fromOntology(ontology)
    println(graph.nodes().size + " objects")

    val start = System.currentTimeMillis()
    val ccs = getConnectedComponents(graph)
    println(ccs.size + " connected components")
    println("CCs in " + Util.formatTime(System.currentTimeMillis() - start))

//    val start1 = System.currentTimeMillis()
//    val seqSim = Interpretation.maximalSimulationOn(graph)
//    println(Util.formatTime(System.currentTimeMillis() - start1))

//    val start2 = System.currentTimeMillis()
//    val parSim2 = maximalSimulationOn2(graph)
//    println(Util.formatTime(System.currentTimeMillis() - start2))

//    val start3 = System.currentTimeMillis()
//    val parSim3 = maximalSimulationOn3(graph)
//    println(Util.formatTime(System.currentTimeMillis() - start3))

//    val start4 = System.currentTimeMillis()
//    val parSim4 = maximalSimulationOn4(graph)
//    println(Util.formatTime(System.currentTimeMillis() - start4))

//    val start5 = System.currentTimeMillis()
//    val parSim5 = maximalSimulationOn5(graph)
//    println(Util.formatTime(System.currentTimeMillis() - start5))

//    val start6 = System.currentTimeMillis()
//    val parSim6 = maximalSimulationOn6(graph)
//    println(Util.formatTime(System.currentTimeMillis() - start6))

//    val start7 = System.currentTimeMillis()
//    val parSim7 = maximalSimulationOn7(graph)
//    println(Util.formatTime(System.currentTimeMillis() - start7))

//    val start8 = System.currentTimeMillis()
//    val parSim8 = maximalSimulationOn8(graph)
//    println(Util.formatTime(System.currentTimeMillis() - start8))
//
//    val start9 = System.currentTimeMillis()
//    val parSim9 = maximalSimulationOn9(graph)
//    println(Util.formatTime(System.currentTimeMillis() - start9))

    val startS1 = System.currentTimeMillis()
    val parSimS1 = GraphSimulator.computeMaximalSimulation(graph, graph)
    println(Util.formatTime(System.currentTimeMillis() - startS1))

//    val startS2 = System.currentTimeMillis()
//    val parSimS2 = GraphSimulator.computeMaximalSimulation2(graph, graph)
//    println(Util.formatTime(System.currentTimeMillis() - startS2))

    val startS3 = System.currentTimeMillis()
    val parSimS3 = GraphSimulator.computeMaximalSimulation3(graph, graph)
    println(Util.formatTime(System.currentTimeMillis() - startS3))

//    val equal2 = graph.nodes().foldLeft(true)((res, node) => res & (seqSim.row(node) equals parSim2.rowImmutable(node)))
//    println(equal2)
//    val equal3 = graph.nodes().foldLeft(true)((res, node) => res & (seqSim.row(node) equals parSim3.rowImmutable(node)))
//    println(equal3)
//    val defects4 = graph.nodes().foldLeft(0)((res, node) => res + (if seqSim.row(node) equals parSim4.rowImmutable(node) then 0 else 1))
//    println("Defects(1,4): " + defects4)
//    val defects5 = graph.nodes().foldLeft(0)((res, node) => res + (if seqSim.row(node) equals parSim5.row(node).viewAsImmutableBitSet then 0 else 1))
//    println("Defects(1,5): " + defects5)
//    val defects6 = graph.nodes().foldLeft(0)((res, node) => res + (if seqSim.row(node) equals parSim6.row(node).viewAsImmutableBitSet then 0 else 1))
//    println("Defects(1,6): " + defects6)
//    val defects7 = graph.nodes().foldLeft(0)((res, node) => res + (if seqSim.row(node) equals parSim7.row(node).viewAsImmutableBitSet then 0 else 1))
//    println("Defects(1,7): " + defects7)
//    val defects8 = graph.nodes().foldLeft(0)((res, node) => res + (if seqSim.row(node) equals parSim8.row(node).viewAsImmutableBitSet then 0 else 1))
//    println("Defects(1,8): " + defects8)
//    val defects78 = graph.nodes().foldLeft(0)((res, node) => res + (if parSim7.row(node).viewAsImmutableBitSet equals parSim8.row(node).viewAsImmutableBitSet then 0 else 1))
//    println("Defects(7,8): " + defects78)
//    val defects89 = graph.nodes().foldLeft(0)((res, node) => res + (if parSim8.row(node).viewAsImmutableBitSet equals parSim9.row(node).viewAsImmutableBitSet then 0 else 1))
//    println("Defects(8,9): " + defects89)
//    val defects9S1 = graph.nodes().foldLeft(0)((res, node) => res + (if parSim9.row(node).viewAsImmutableBitSet equals parSimS1.row(node).viewAsImmutableBitSet then 0 else 1))
//    println("Defects(9,S1): " + defects9S1)
//      val defectsS1S2 = graph.nodes().foldLeft(0)((res, node) => res + (if parSimS1.row(node).viewAsImmutableBitSet equals parSimS2.row(node).viewAsImmutableBitSet then 0 else 1))
//      println("Defects(S1,S2): " + defectsS1S2)
      val defectsS1S3 = graph.nodes().foldLeft(0)((res, node) => res + (if parSimS1.row(node).viewAsImmutableBitSet equals parSimS3.row(node).viewAsImmutableBitSet then 0 else 1))
      println("Defects(S1,S3): " + defectsS1S3)

    ParallelComputations.shutdownThreadPool()

  }

  def getConnectedComponents(graph: BitGraph[OWLClass, OWLObjectProperty]): collection.Set[collection.BitSet] = {
    val connectedComponents = collection.mutable.HashSet[collection.BitSet]()
    val hasNotBeenVisited = graph.nodes().clone()
    while (hasNotBeenVisited.nonEmpty) {
      val connectedComponent = collection.mutable.BitSet()
      @tailrec def dfs(nodes: Iterable[Int]): Unit = {
        val next = collection.mutable.BitSet()
        for (node <- nodes) {
          if (hasNotBeenVisited(node) == true) {
            hasNotBeenVisited(node) = false
            connectedComponent.addOne(node)
            //graph.predecessors(node).foreach({ case (_, pred) => dfs(pred) })
            //graph.successorRelations(node).flatMap(graph.successorsForRelation(node, _)).foreach({ succ => dfs(succ) })
            next.addAll(graph.predecessors(node).map(_._2))
            next.addAll(graph.successorRelations(node).flatMap(graph.successorsForRelation(node, _)))
          }
        }
        if (next.nonEmpty) {
          dfs(next)
        }
      }
      dfs(Iterable.single(hasNotBeenVisited.head))
      connectedComponents.addOne(connectedComponent)
    }
    connectedComponents
  }

  @Deprecated
  def maximalSimulationOn2(graph: BitGraph[OWLClass, OWLObjectProperty])(using logger: Logger): ConcurrentBitBiMap = {

    logger.reset()
    logger.println("Computing pre-simulation...")
    val start1 = System.currentTimeMillis()
    val simulation = ConcurrentBitBiMap(graph.nodes().size, graph.nodes().size)
    graph.nodes().foreachPar(y => {
      //      logger.tick()
      val yLabels = graph.labels(y)
      val yProperties = graph.successorRelations(y)
      for (x <- graph.nodes() if !(x equals y)) {
        if ((graph.labels(x) subsetOf yLabels) && (graph.successorRelations(x) subsetOf yProperties)) {
          simulation.add(x, y)
        }
      }
    })
    println("Phase 1: " + Util.formatTime(System.currentTimeMillis() - start1))
    //    logger.reset()

    logger.println("Computing initial mapping R(x,r)...")
    val start2 = System.currentTimeMillis()
    val bitR = ConcurrentBitMap[(Int, OWLObjectProperty)](graph.nodes().size - 1)
    graph.nodes().foreachPar(x => {
      //      logger.tick()
      for (yy <- graph.nodes()) {
        for (property <- graph.successorRelations(yy)) {
          val successors = graph.successorsForRelation(yy, property)
          if (!successors.contains(x) && (simulation.rowImmutable(x) intersect successors).isEmpty) {
            bitR.add((x, property), yy)
          }
        }
      }
    })
    println("Phase 2: " + Util.formatTime(System.currentTimeMillis() - start2))
    //    logger.reset()

    logger.println("Computing simulation...")
    val start3 = System.currentTimeMillis()
    while (bitR.rows().nonEmpty) {
      //      logger.tick()
      val (x, r) = bitR.rows().head
      graph.predecessorsForRelation(x, r).foreachPar(xx => {
        val row = simulation.row(xx)
        if (row.isDefined) {
          (bitR.rowImmutable((x, r)) intersect row.get.viewAsImmutableBitSet()).foreach { yy =>
            //            simulation.remove(xx, yy)
            row.get.remove(yy)
            simulation.col(yy).get.remove(xx)
            graph.predecessors(yy).foreach { (rr, yyy) =>
              val successors = graph.successorsForRelation(yyy, rr)
              if (!successors.contains(xx) && (row.get.viewAsImmutableBitSet() intersect successors).isEmpty) {
                bitR.add((xx, rr), yyy)
              }
            }
          }
        }
      })
      bitR.clearRow((x, r))
    }
    println("Phase 3: " + Util.formatTime(System.currentTimeMillis() - start3))
    logger.reset()

    simulation

  }

  @Deprecated
  def maximalSimulationOn3(graph: BitGraph[OWLClass, OWLObjectProperty])(using logger: Logger): ConcurrentBitBiMap = {

    logger.reset()
    logger.println("Computing pre-simulation...")
    val start1 = System.currentTimeMillis()
    val simulation = ConcurrentBitBiMap(graph.nodes().size, graph.nodes().size)
    graph.nodes().foreachPar(y => {
      //      logger.tick()
      val yLabels = graph.labels(y)
      val yProperties = graph.successorRelations(y)
      for (x <- graph.nodes() if !(x equals y)) {
        if ((graph.labels(x) subsetOf yLabels) && (graph.successorRelations(x) subsetOf yProperties)) {
          simulation.add(x, y)
        }
      }
    })
    println("Phase 1: " + Util.formatTime(System.currentTimeMillis() - start1))
    //    logger.reset()

    logger.println("Computing initial mapping R(x,r)...")
    val start2 = System.currentTimeMillis()
    val bitR = ConcurrentBitMap[(Int, OWLObjectProperty)](graph.nodes().size - 1)
    graph.nodes().foreachPar(x => {
      //      logger.tick()
      for (yy <- graph.nodes()) {
        for (property <- graph.successorRelations(yy)) {
          val successors = graph.successorsForRelation(yy, property)
          if (!successors.contains(x) && (simulation.rowImmutable(x) intersect successors).isEmpty) {
            bitR.add((x, property), yy)
          }
        }
      }
    })
    println("Phase 2: " + Util.formatTime(System.currentTimeMillis() - start2))
    //    logger.reset()

    logger.println("Computing simulation...")
    val start3 = System.currentTimeMillis()
    while (bitR.rows().nonEmpty) {
      //      logger.tick()
      val (x, r) = bitR.rows().head
      graph.predecessorsForRelation(x, r).foreach(xx => {
        val row = simulation.row(xx)
        if (row.isDefined) {
          (bitR.rowImmutable((x, r)) intersect row.get.viewAsImmutableBitSet()).foreachPar(yy => {
            //            simulation.remove(xx, yy)
            row.get.remove(yy)
            simulation.col(yy).get.remove(xx)
            graph.predecessors(yy).foreach { (rr, yyy) =>
              val successors = graph.successorsForRelation(yyy, rr)
              if (!successors.contains(xx) && (row.get.viewAsImmutableBitSet() intersect successors).isEmpty) {
                bitR.add((xx, rr), yyy)
              }
            }
          })
        }
      })
      bitR.clearRow((x, r))
    }
    println("Phase 3: " + Util.formatTime(System.currentTimeMillis() - start3))
    logger.reset()

    simulation

  }

  @Deprecated
  def maximalSimulationOn4(graph: BitGraph[OWLClass, OWLObjectProperty])(using logger: Logger): ConcurrentBitBiMap = {

    logger.reset()
    logger.println("Computing pre-simulation...")
    val start1 = System.currentTimeMillis()
    val simulation = ConcurrentBitBiMap(graph.nodes().size, graph.nodes().size)
    graph.nodes().foreachPar(y => {
      logger.tick()
      val yLabels = graph.labels(y)
      val yProperties = graph.successorRelations(y)
      for (x <- graph.nodes() if !(x equals y)) {
        if ((graph.labels(x) subsetOf yLabels) && (graph.successorRelations(x) subsetOf yProperties)) {
          simulation.add(x, y)
        }
      }
    })
    logger.reset()
    println("Phase 1: " + Util.formatTime(System.currentTimeMillis() - start1))

    logger.println("Computing initial mapping R(x,r)...")
    val start2 = System.currentTimeMillis()
    val bitR = ConcurrentBitMap[(Int, OWLObjectProperty)](graph.nodes().size - 1)
    graph.nodes().foreachPar(x => {
      logger.tick()
      for (yy <- graph.nodes()) {
        for (property <- graph.successorRelations(yy)) {
          val successors = graph.successorsForRelation(yy, property)
          if (!successors.contains(x) && (simulation.rowImmutable(x) intersect successors).isEmpty) {
            bitR.add((x, property), yy)
          }
        }
      }
    })
    logger.reset()
    println("Phase 2: " + Util.formatTime(System.currentTimeMillis() - start2))

    logger.println("Computing simulation...")
    val start3 = System.currentTimeMillis()
    while (bitR.rows().nonEmpty) {

      //val notSimilar = java.util.concurrent.ConcurrentLinkedQueue[(Int, Int)]()
      val newBitR = java.util.concurrent.ConcurrentLinkedQueue[(Int, OWLObjectProperty, Int)]()

      val iterator = new scala.collection.Iterator[(Int, OWLObjectProperty)] {

        private val hasCommonPredecessorWithPreviousNode = collection.mutable.BitSet()
        private val it = bitR.rows().iterator.filterNot({ (x, _) => hasCommonPredecessorWithPreviousNode(x) })

        def hasNext: Boolean = it.hasNext

        def next(): (Int, OWLObjectProperty) = {
          val (x, r) = it.next()
          //graph.predecessors(x).foreach({ (_, pred) =>
          //  graph.successorRelations(pred).foreach({ rel =>
          //    forbiddenNodes.addAll(graph.successorsForRelation(pred, rel))
          //  })
          //})
          for ((_, predecessor) <- graph.predecessors(x); relation <- graph.successorRelations(predecessor)) {
            hasCommonPredecessorWithPreviousNode.addAll(graph.successorsForRelation(predecessor, relation))
          }
          (x, r)
        }
      }

      collection.mutable.HashSet.from(iterator) //.take(Runtime.getRuntime.availableProcessors()))
        .foreachPar({ (x, r) =>
          logger.tick()
          val bitRRowView = bitR.row((x, r)).get.viewAsImmutableBitSet //.copyToImmutableBitSet()
          bitR.clearRow((x, r))
          graph.predecessorsForRelation(x, r).foreach { xx =>
            val simRow = simulation.row(xx)
            if (simRow.isDefined) {
              val simRowView = simRow.get.viewAsImmutableBitSet
              (bitRRowView intersect simRowView).foreach { yy =>
                //simulation.remove(xx, yy)
                simRow.get.remove(yy)
                simulation.col(yy).get.remove(xx)
                //notSimilar.add((xx, yy))
                graph.predecessors(yy).foreach { (rr, yyy) =>
                  val successors = graph.successorsForRelation(yyy, rr)
                  if (!(successors contains xx)) {
                    val size = (simRowView intersect successors).size
                    if ((size == 0)) { // || ((size == 1) && (successors contains yy))) {
                      //bitR.add((xx, rr), yyy)
                      newBitR.add((xx, rr, yyy))
                    }
                  }
                }
              }
            }
          }
        })
      //notSimilar.forEach(simulation.remove)
      newBitR.forEach({ (xx, rr, yyy) => bitR.add((xx, rr), yyy) })

    }
    logger.reset()
    println("Phase 3: " + Util.formatTime(System.currentTimeMillis() - start3))

    simulation

  }

  @Deprecated
  def maximalSimulationOn5(graph: BitGraph[OWLClass, OWLObjectProperty])(using logger: Logger): ConcurrentArrayBitBiMap = {

    logger.reset()
    logger.println("Computing pre-simulation...")
    val start1 = System.currentTimeMillis()
    val simulation = ConcurrentArrayBitBiMap(graph.nodes().size, graph.nodes().size)
    graph.nodes().foreachPar(y => {
      logger.tick()
      val yLabels = graph.labels(y)
      val yProperties = graph.successorRelations(y)
      for (x <- graph.nodes() if !(x equals y)) {
        if ((graph.labels(x) subsetOf yLabels) && (graph.successorRelations(x) subsetOf yProperties)) {
          simulation.add(x, y)
        }
      }
    })
    logger.reset()
    println("Phase 1: " + Util.formatTime(System.currentTimeMillis() - start1))

    logger.println("Computing initial mapping R(x,r)...")
    val start2 = System.currentTimeMillis()
    val bitR = ConcurrentBitMap[(Int, OWLObjectProperty)](graph.nodes().size - 1)
    graph.nodes().foreachPar(x => {
      logger.tick()
      for (yy <- graph.nodes()) {
        for (property <- graph.successorRelations(yy)) {
          val successors = graph.successorsForRelation(yy, property)
          if (!successors.contains(x) && (simulation.row(x).viewAsImmutableBitSet() intersect successors).isEmpty) {
            bitR.add((x, property), yy)
          }
        }
      }
    })
    logger.reset()
    println("Phase 2: " + Util.formatTime(System.currentTimeMillis() - start2))

    logger.println("Computing simulation...")
    val start3 = System.currentTimeMillis()
    while (bitR.rows().nonEmpty) {

      if (bitR.rows().size < 10) {

        def processFast(x: Int, r: OWLObjectProperty): Unit = {
          logger.tick()
          val bitRRowView = bitR.row((x, r)).get.viewAsImmutableBitSet //.copyToImmutableBitSet()
          bitR.clearRow((x, r))
          graph.predecessorsForRelation(x, r).foreach { xx =>
            val simRow = simulation.row(xx)
            val simRowView = simRow.viewAsImmutableBitSet
            (bitRRowView intersect simRowView).foreach { yy =>
              //simulation.remove(xx, yy)
              simRow.remove(yy)
              simulation.col(yy).remove(xx)
              //notSimilar.add((xx, yy))
              graph.predecessors(yy).foreach { (rr, yyy) =>
                val successors = graph.successorsForRelation(yyy, rr)
                if (!(successors contains xx)) {
                  val size = (simRowView intersect successors).size
                  if ((size == 0)) { // || ((size == 1) && (successors contains yy))) {
                    bitR.add((xx, rr), yyy)
                    //newBitR.add((xx, rr, yyy))
                  }
                }
              }
            }
          }
        }

        val (x, r) = bitR.rows().head
        processFast(x, r)

      } else {

        //val notSimilar = java.util.concurrent.ConcurrentLinkedQueue[(Int, Int)]()
        val newBitR = java.util.concurrent.ConcurrentLinkedQueue[(Int, OWLObjectProperty, Int)]()

        def process(x: Int, r: OWLObjectProperty): Unit = {
          logger.tick()
          val bitRRowView = bitR.row((x, r)).get.viewAsImmutableBitSet //.copyToImmutableBitSet()
          bitR.clearRow((x, r))
          graph.predecessorsForRelation(x, r).foreach { xx =>
            val simRow = simulation.row(xx)
            val simRowView = simRow.viewAsImmutableBitSet
            (bitRRowView intersect simRowView).foreach { yy =>
              //simulation.remove(xx, yy)
              simRow.remove(yy)
              simulation.col(yy).remove(xx)
              //notSimilar.add((xx, yy))
              graph.predecessors(yy).foreach { (rr, yyy) =>
                val successors = graph.successorsForRelation(yyy, rr)
                if (!(successors contains xx)) {
                  val size = (simRowView intersect successors).size
                  if ((size == 0)) { // || ((size == 1) && (successors contains yy))) {
                    //bitR.add((xx, rr), yyy)
                    newBitR.add((xx, rr, yyy))
                  }
                }
              }
            }
          }
        }

        val iterator = new scala.collection.Iterator[(Int, OWLObjectProperty)] {

          private val hasCommonPredecessorWithPreviousNode = collection.mutable.BitSet()
          private val it = bitR.rows().iterator.filterNot({ (x, _) => hasCommonPredecessorWithPreviousNode(x) })

          def hasNext: Boolean = it.hasNext

          def next(): (Int, OWLObjectProperty) = {
            val (x, r) = it.next()
            //graph.predecessors(x).foreach({ (_, pred) =>
            //  graph.successorRelations(pred).foreach({ rel =>
            //    forbiddenNodes.addAll(graph.successorsForRelation(pred, rel))
            //  })
            //})
            for ((_, predecessor) <- graph.predecessors(x); relation <- graph.successorRelations(predecessor)) {
              hasCommonPredecessorWithPreviousNode.addAll(graph.successorsForRelation(predecessor, relation))
            }
            (x, r)
          }
        }

        collection.mutable.HashSet.from(iterator) //.take(Runtime.getRuntime.availableProcessors()))
          .foreachPar({ (x, r) => process(x, r) })

        //notSimilar.forEach(simulation.remove)
        newBitR.forEach({ (xx, rr, yyy) => bitR.add((xx, rr), yyy) })

      }

    }
    logger.reset()
    println("Phase 3: " + Util.formatTime(System.currentTimeMillis() - start3))

    simulation

  }

  @Deprecated
  def maximalSimulationOn6(graph: BitGraph[OWLClass, OWLObjectProperty])(using logger: Logger): ConcurrentArrayBitBiMap = {

    logger.reset()
    logger.println("Computing pre-simulation...")
    val start1 = System.currentTimeMillis()
    val simulation = ConcurrentArrayBitBiMap(graph.nodes().size, graph.nodes().size)
    graph.nodes().foreachPar(y => {
      logger.tick()
      val yLabels = graph.labels(y)
      val yProperties = graph.successorRelations(y)
      for (x <- graph.nodes() if !(x equals y)) {
        if ((graph.labels(x) subsetOf yLabels) && (graph.successorRelations(x) subsetOf yProperties)) {
          simulation.add(x, y)
        }
      }
    })
    logger.reset()
    println("Phase 1: " + Util.formatTime(System.currentTimeMillis() - start1))

    logger.println("Computing initial mapping R(x,r)...")
    val start2 = System.currentTimeMillis()
    //val bitR = BitMap[(Int, OWLObjectProperty)]()
    val bitR = ConcurrentBitMap[(Int, OWLObjectProperty)](graph.nodes().size - 1)
    graph.nodes().foreachPar(x => {
      logger.tick()
      for (yy <- graph.nodes()) {
        for (property <- graph.successorRelations(yy)) {
          val successors = graph.successorsForRelation(yy, property)
          if (!successors.contains(x) && (simulation.row(x).viewAsImmutableBitSet intersect successors).isEmpty) {
            //bitR.synchronized { bitR.add((x, property), yy) }
            bitR.add((x, property), yy)
          }
        }
      }
    })
    logger.reset()
    println("Phase 2: " + Util.formatTime(System.currentTimeMillis() - start2))

    logger.println("Computing simulation...")
    val start3 = System.currentTimeMillis()
    val nodesThatHaveACommonPredecessorWith = new java.util.concurrent.ConcurrentHashMap[Int, collection.BitSet]().asScala
    graph.nodes().foreachPar(node => {
      val predecessors = collection.mutable.BitSet()
      graph.predecessors(node).foreach({ (_, pred) => predecessors.add(pred) })
      nodesThatHaveACommonPredecessorWith(node) =
        predecessors.flatMap(pred => graph.successorRelations(pred).flatMap(rel => graph.successorsForRelation(pred, rel)))
    })
    var continueMultiThreaded = true
    while (continueMultiThreaded && bitR.rows().nonEmpty) {

      //val notSimilar = java.util.concurrent.ConcurrentLinkedQueue[(Int, Int)]()
      //val newBitR = java.util.concurrent.ConcurrentLinkedQueue[(Int, OWLObjectProperty, Int)]()

      val iterator = new scala.collection.Iterator[(Int, OWLObjectProperty)] {

        private val hasCommonPredecessorWithPreviousNode = collection.mutable.BitSet()
        private val it = bitR.rows().iterator.filterNot({ (x, _) => hasCommonPredecessorWithPreviousNode(x) })

        def hasNext: Boolean = it.hasNext

        def next(): (Int, OWLObjectProperty) = {
          val (x, r) = it.next()
          //graph.predecessors(x).foreach({ (_, pred) =>
          //  graph.successorRelations(pred).foreach({ rel =>
          //    forbiddenNodes.addAll(graph.successorsForRelation(pred, rel))
          //  })
          //})
          //for ((_, predecessor) <- graph.predecessors(x); relation <- graph.successorRelations(predecessor)) {
          //  hasCommonPredecessorWithPreviousNode.addAll(graph.successorsForRelation(predecessor, relation))
          //}
          hasCommonPredecessorWithPreviousNode.addAll(nodesThatHaveACommonPredecessorWith(x))
          (x, r)
        }
      }

      val next = collection.mutable.HashSet.from(iterator) //.take(Runtime.getRuntime.availableProcessors()))
      continueMultiThreaded = next.size > 2
      next.foreachPar({ (x, r) =>
        logger.tick()
        //val bitRRowView = bitR.rowImmutable((x, r))
        //bitR.clearRow((x, r))
        val bitRRow = bitR.clearRow((x, r)).get.viewAsImmutableBitSet
        //val bitRRowView = bitR.row((x, r))
        //bitR.synchronized { bitR.clearRow((x, r)) }
        //val bitRRow = bitR.synchronized(bitR.clearRow((x, r))).get
        graph.predecessorsForRelation(x, r).foreach { xx =>
          val simRow = simulation.row(xx)
          val simRowView = simRow.viewAsImmutableBitSet
          (bitRRow intersect simRowView).foreach { yy =>
            //simulation.remove(xx, yy)
            simRow.remove(yy)
            simulation.col(yy).remove(xx)
            //notSimilar.add((xx, yy))
            graph.predecessors(yy).foreach { (rr, yyy) =>
              val successors = graph.successorsForRelation(yyy, rr)
              if (!(successors contains xx) && (simRowView intersect successors).isEmpty) {
                bitR.add((xx, rr), yyy)
                //newBitR.add((xx, rr, yyy))
              }
            }
          }
        }
      })
      //notSimilar.forEach(simulation.remove)
      //newBitR.forEach({ (xx, rr, yyy) => bitR.add((xx, rr), yyy) })

    }

    logger.println()
    logger.println("Now continuing single-threaded...")
//    print("Copying the simulation...")
//    val simulationCopy = BitBiMap()
//    graph.nodes().foreach(node => {
//      val row = simulation.row(node)
//      if (row.nonEmpty)
//        simulationCopy.rowMap(node) = row
//      val col = simulation.col(node)
//      if (col.nonEmpty)
//        simulationCopy.colMap(node) = col
//    })
//    println(" done.")

    while (bitR.rows().nonEmpty) {
      logger.tick()
      val (x, r) = bitR.rows().head
      //val bitRRowView = bitR.row((x, r))
      //bitR.clearRow((x, r))
      //val bitRRow = bitR.clearRow((x, r)).get
      val bitRRow = bitR.clearRow((x, r)).get.viewAsImmutableBitSet
      for (xx <- graph.predecessorsForRelation(x, r)) {
        val simRow = simulation.row(xx)
        val simRowView = simRow.viewAsImmutableBitSet
        for (yy <- bitRRow intersect simRowView) {
        //for (yy <- bitR.rowImmutable((x, r)) intersect simRowView) {
          //simulation.remove(xx, yy)
          simRow.remove(yy)
          simulation.col(yy).remove(xx)
          //notSimilar.add((xx, yy))
          for ((rr, yyy) <- graph.predecessors(yy)) {
            val successors = graph.successorsForRelation(yyy, rr)
            if (!(successors contains xx) && (simRowView intersect successors).isEmpty) {
              bitR.add((xx, rr), yyy)
              //newBitR.add((xx, rr, yyy))
            }
          }
        }
      }
    }

    logger.reset()
    println("Phase 3: " + Util.formatTime(System.currentTimeMillis() - start3))

    simulation

  }

  @Deprecated
  def maximalSimulationOn7(graph: BitGraph[OWLClass, OWLObjectProperty])(using logger: Logger): ConcurrentArrayBitBiMap = {

    logger.reset()
    logger.println("Computing pre-simulation...")
    val start1 = System.currentTimeMillis()
    val simulation = ConcurrentArrayBitBiMap(graph.nodes().size, graph.nodes().size)
    graph.nodes().foreachPar(y => {
      logger.tick()
      val yLabels = graph.labels(y)
      val yProperties = graph.successorRelations(y)
      for (x <- graph.nodes() if !(x equals y)) {
        if ((graph.labels(x) subsetOf yLabels) && (graph.successorRelations(x) subsetOf yProperties)) {
          simulation.add(x, y)
        }
      }
    })
    logger.reset()
    println("Phase 1: " + Util.formatTime(System.currentTimeMillis() - start1))

    logger.println("Computing initial mapping R(x,r)...")
    val start2 = System.currentTimeMillis()
    val R = java.util.concurrent.ConcurrentHashMap[Int, ConcurrentBitMap[OWLObjectProperty]]().asScala

    def addToR(x: Int, r: OWLObjectProperty, y: Int): Unit = {
      R.getOrElseUpdate(x, ConcurrentBitMap[OWLObjectProperty](graph.nodes().size - 1)).add(r, y)
    }

    graph.nodes().foreachPar(x => {
      logger.tick()
      for (yy <- graph.nodes()) {
        for (property <- graph.successorRelations(yy)) {
          val successors = graph.successorsForRelation(yy, property)
          if (!successors.contains(x) && (simulation.row(x).viewAsImmutableBitSet intersect successors).isEmpty) {
            addToR(x, property, yy)
          }
        }
      }
    })
    logger.reset()
    println("Phase 2: " + Util.formatTime(System.currentTimeMillis() - start2))

    logger.println("Computing simulation...")
    val start3 = System.currentTimeMillis()
    val nodesThatHaveACommonPredecessorWith = new java.util.concurrent.ConcurrentHashMap[Int, collection.BitSet]().asScala
    graph.nodes().foreachPar(node => {
      val predecessors = collection.mutable.BitSet()
      graph.predecessors(node).foreach({ (_, pred) => predecessors.add(pred) })
      nodesThatHaveACommonPredecessorWith(node) =
        predecessors.flatMap(pred => graph.successorRelations(pred).flatMap(rel => graph.successorsForRelation(pred, rel)))
    })
    //logger.println("Common predecessor relation: " + Util.formatTime(System.currentTimeMillis() - start3))
    var continueMultiThreaded = true
    while (continueMultiThreaded && R.keySet.nonEmpty) {

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
      var parallelism = 0

      while (remainingNodes.nonEmpty) {
        val x = remainingNodes.head
        remainingNodes -= x
        remainingNodes --= nodesThatHaveACommonPredecessorWith(x)
        //remainingNodes &~= nodesThatHaveACommonPredecessorWith(x)
        parallelism += 1
        futures.add(scala.concurrent.Future {
          val Rx = R.remove(x).get
          Rx.rows().foreach({ r =>
            logger.tick()
            graph.predecessorsForRelation(x, r).foreach { xx =>
              val simRow = simulation.row(xx)
              val simRowView = simRow.viewAsImmutableBitSet
              (Rx.rowImmutable(r) intersect simRowView).foreach { yy =>
                simRow.remove(yy)
                simulation.col(yy).remove(xx)
                graph.predecessors(yy).foreach { (rr, yyy) =>
                  val successors = graph.successorsForRelation(yyy, rr)
                  if (!(successors contains xx) && (simRowView intersect successors).isEmpty) {
                    addToR(xx, rr, yyy)
                  }
                }
              }
            }
          })
        })
      }

      futures.asScala.awaitEach()

      continueMultiThreaded = parallelism > 2

    }

    logger.println()
    logger.println("Now continuing single-threaded...")

    while (R.keySet.nonEmpty) {
      val x = R.keySet.head
      val Rx = R.remove(x).get
      Rx.rows().foreach({ r =>
        logger.tick()
        for (xx <- graph.predecessorsForRelation(x, r)) {
          val simRow = simulation.row(xx)
          val simRowView = simRow.viewAsImmutableBitSet
          for (yy <- Rx.rowImmutable(r) intersect simRowView) {
            simRow.remove(yy)
            simulation.col(yy).remove(xx)
            for ((rr, yyy) <- graph.predecessors(yy)) {
              val successors = graph.successorsForRelation(yyy, rr)
              if (!(successors contains xx) && (simRowView intersect successors).isEmpty) {
                addToR(xx, rr, yyy)
              }
            }
          }
        }
      })
    }

    logger.reset()
    println("Phase 3: " + Util.formatTime(System.currentTimeMillis() - start3))

    simulation

  }

  def maximalSimulationOn8(graph: BitGraph[OWLClass, OWLObjectProperty])(using logger: Logger): ConcurrentArrayBitBiMap = {

    logger.reset()
    logger.println("Computing pre-simulation...")
    val start1 = System.currentTimeMillis()
    val simulation = ConcurrentArrayBitBiMap(graph.nodes().size, graph.nodes().size)
    graph.nodes().foreachPar(y => {
      logger.tick()
      val yLabels = graph.labels(y)
      val yProperties = graph.successorRelations(y)
      for (x <- graph.nodes() if !(x equals y)) {
        if ((graph.labels(x) subsetOf yLabels) && (graph.successorRelations(x) subsetOf yProperties)) {
          simulation.add(x, y)
        }
      }
    })
    logger.reset()
    println("Phase 1: " + Util.formatTime(System.currentTimeMillis() - start1))

    logger.println("Computing initial mapping R(x,r)...")
    val start2 = System.currentTimeMillis()
    val R = java.util.concurrent.ConcurrentHashMap[Int, ConcurrentBitMap[OWLObjectProperty]]().asScala

    def addToR(x: Int, r: OWLObjectProperty, y: Int): Unit = {
      R.getOrElseUpdate(x, ConcurrentBitMap[OWLObjectProperty](graph.nodes().size - 1)).add(r, y)
    }

    //val predecessors = collection.mutable.HashMap[Int, collection.mutable.HashMap[OWLObjectProperty, collection.mutable.BitSet]]()
    //for (x <- graph.nodes()) {
    val predecessors = new scala.Array[collection.mutable.HashMap[OWLObjectProperty, collection.mutable.BitSet]](graph.nodes().size)
    graph.nodes().foreachPar(x => {
      predecessors(x) = collection.mutable.HashMap[OWLObjectProperty, collection.mutable.BitSet]()
      for ((r, y) <- graph.predecessors(x)) {
        predecessors(x).getOrElseUpdate(r, collection.mutable.BitSet()).addOne(y)
      }
    })

    graph.nodes().foreachPar(x => {
      logger.tick()
      //      for (yy <- graph.nodes()) {
      //        for (property <- graph.successorRelations(yy)) {
      //          val successors = graph.successorsForRelation(yy, property)
      //          if (!successors.contains(x) && (simulation.row(x).viewAsImmutableBitSet intersect successors).isEmpty) {
      //            addToR(x, property, yy)
      //          }
      //        }
      //      }
      R(x) = ConcurrentBitMap[OWLObjectProperty](graph.nodes().size - 1)
      for (r <- predecessors(x).keySet) {
        R(x).rowMap.getOrElseUpdate(r, concurrent.ConcurrentBoundedBitSet2(graph.nodes().toBitMask))
          .viewAsMutableBitSet.subtractAll(predecessors(x)(r))
      }
      for (y <- simulation.row(x).viewAsImmutableBitSet) {
        for (r <- predecessors(y).keySet) {
          R(x).rowMap.getOrElseUpdate(r, concurrent.ConcurrentBoundedBitSet2(graph.nodes().toBitMask))
            .viewAsMutableBitSet.subtractAll(predecessors(y)(r))
        }
      }
    })
    logger.reset()
    println("Phase 2: " + Util.formatTime(System.currentTimeMillis() - start2))

    logger.println("Computing simulation...")
    val start3 = System.currentTimeMillis()
    val nodesThatHaveACommonPredecessorWith = new java.util.concurrent.ConcurrentHashMap[Int, collection.BitSet]().asScala
    graph.nodes().foreachPar(node => {
      val predecessors = collection.mutable.BitSet()
      graph.predecessors(node).foreach({ (_, pred) => predecessors.add(pred) })
      nodesThatHaveACommonPredecessorWith(node) =
        predecessors.flatMap(pred => graph.successorRelations(pred).flatMap(rel => graph.successorsForRelation(pred, rel)))
    })
    //logger.println("Common predecessor relation: " + Util.formatTime(System.currentTimeMillis() - start3))
    var continueMultiThreaded = true
    while (continueMultiThreaded && R.keySet.nonEmpty) {

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
      var parallelism = 0

      while (remainingNodes.nonEmpty) {
        val x = remainingNodes.head
        remainingNodes -= x
        remainingNodes --= nodesThatHaveACommonPredecessorWith(x)
        //remainingNodes &~= nodesThatHaveACommonPredecessorWith(x)
        parallelism += 1
        futures.add(scala.concurrent.Future {
          //          The code in Phase 2:
          //          R(x) = ConcurrentBitMap[OWLObjectProperty](graph.nodes().size - 1)
          //          for (r <- predecessors(x).keySet) {
          //            R(x).rowMap.getOrElseUpdate(r, concurrent.ConcurrentBoundedBitSet2(graph.nodes().toBitMask))
          //              .viewAsMutableBitSet.subtractAll(predecessors(x)(r))
          //          }
          //          for (y <- simulation.row(x).viewAsImmutableBitSet) {
          //            for (r <- predecessors(y).keySet) {
          //              R(x).rowMap.getOrElseUpdate(r, concurrent.ConcurrentBoundedBitSet2(graph.nodes().toBitMask))
          //                .viewAsMutableBitSet.subtractAll(predecessors(y)(r))
          //            }
          //          }
          val Rx = R.remove(x).get
          Rx.rows().foreach({ r =>
            logger.tick()
            // TODO: Use Java's Fork/Join Framework
            graph.predecessorsForRelation(x, r).foreach { xx =>
              val simRow = simulation.row(xx)
              val simRowView = simRow.viewAsImmutableBitSet
              (Rx.rowImmutable(r) intersect simRowView).foreach { yy =>
                simRow.remove(yy)
                simulation.col(yy).remove(xx)
                graph.predecessors(yy).foreach { (rr, yyy) =>
                  val successors = graph.successorsForRelation(yyy, rr)
                  if (!(successors contains xx) && (simRowView intersect successors).isEmpty) {
                    addToR(xx, rr, yyy)
                  }
                }
              }
            }
          })
        })
      }

      futures.asScala.awaitEach()

      continueMultiThreaded = parallelism > 2

    }

    logger.println()
    logger.println("Now continuing single-threaded...")

    while (R.keySet.nonEmpty) {
      val x = R.keySet.head
      val Rx = R.remove(x).get
      Rx.rows().foreach({ r =>
        logger.tick()
        for (xx <- graph.predecessorsForRelation(x, r)) {
          val simRow = simulation.row(xx)
          val simRowView = simRow.viewAsImmutableBitSet
          for (yy <- Rx.rowImmutable(r) intersect simRowView) {
            simRow.remove(yy)
            simulation.col(yy).remove(xx)
            for ((rr, yyy) <- graph.predecessors(yy)) {
              val successors = graph.successorsForRelation(yyy, rr)
              if (!(successors contains xx) && (simRowView intersect successors).isEmpty) {
                addToR(xx, rr, yyy)
              }
            }
          }
        }
      })
    }

    logger.reset()
    println("Phase 3: " + Util.formatTime(System.currentTimeMillis() - start3))

    simulation

  }

  /**
   * @param graph
   * @param logger
   * @return
   * It is assumed that the nodes in the graph are consecutive numbers starting with 0
   */
  def maximalSimulationOn9(graph: BitGraph[OWLClass, OWLObjectProperty])(using logger: Logger): ConcurrentArrayBitBiMap = {

    logger.reset()
    logger.println("Computing pre-simulation...")
    val start1 = System.currentTimeMillis()
    val simulation = ConcurrentArrayBitBiMap(graph.nodes().size, graph.nodes().size)
    graph.nodes().foreachPar(y => {
      logger.tick()
      val yLabels = graph.labels(y)
      val yProperties = graph.successorRelations(y)
      for (x <- graph.nodes()) {
        if ((x equals y) || ((graph.labels(x) subsetOf yLabels) && (graph.successorRelations(x) subsetOf yProperties))) {
          simulation.add(x, y)
        }
      }
    })
    logger.reset()
    println("Phase 1: " + Util.formatTime(System.currentTimeMillis() - start1))

    logger.println("Computing initial mapping R(x,r)...")
    val start2 = System.currentTimeMillis()
    val R = java.util.concurrent.ConcurrentHashMap[Int, ConcurrentBitMap[OWLObjectProperty]]().asScala

    def addToR(x: Int, r: OWLObjectProperty, y: Int): Unit = {
      R.getOrElseUpdate(x, ConcurrentBitMap[OWLObjectProperty](graph.nodes().size - 1)).add(r, y)
    }

    //val predecessors = collection.mutable.HashMap[Int, collection.mutable.HashMap[OWLObjectProperty, collection.mutable.BitSet]]()
    //for (x <- graph.nodes()) {
    val predecessors = new scala.Array[collection.mutable.HashMap[OWLObjectProperty, collection.mutable.BitSet]](graph.nodes().size)
    graph.nodes().foreachPar(x => {
      predecessors(x) = collection.mutable.HashMap[OWLObjectProperty, collection.mutable.BitSet]()
      for ((r, y) <- graph.predecessors(x)) {
        predecessors(x).getOrElseUpdate(r, collection.mutable.BitSet()).addOne(y)
      }
    })

    graph.nodes().foreachPar(x => {
      logger.tick()
      //      for (yy <- graph.nodes()) {
      //        for (property <- graph.successorRelations(yy)) {
      //          val successors = graph.successorsForRelation(yy, property)
      //          if (!successors.contains(x) && (simulation.row(x).viewAsImmutableBitSet intersect successors).isEmpty) {
      //            addToR(x, property, yy)
      //          }
      //        }
      //      }
      R(x) = ConcurrentBitMap[OWLObjectProperty](graph.nodes().size - 1)
//      for (r <- predecessors(x).keySet) {
//        R(x).rowMap.getOrElseUpdate(r, concurrent.ConcurrentBoundedBitSet2(graph.nodes().toBitMask))
//          .viewAsMutableBitSet.subtractAll(predecessors(x)(r))
//      }
      for (y <- simulation.row(x).viewAsImmutableBitSet) {
        for (r <- predecessors(y).keySet) {
          R(x).rowMap.getOrElseUpdate(r, concurrent.ConcurrentBoundedBitSet2(graph.nodes().toBitMask))
            .viewAsMutableBitSet.subtractAll(predecessors(y)(r))
        }
      }
    })
    logger.reset()
    println("Phase 2: " + Util.formatTime(System.currentTimeMillis() - start2))

    logger.println("Computing simulation...")
    val start3 = System.currentTimeMillis()
    val nodesThatHaveACommonPredecessorWith = new java.util.concurrent.ConcurrentHashMap[Int, collection.BitSet]().asScala
    graph.nodes().foreachPar(node => {
      val predecessors = collection.mutable.BitSet()
      graph.predecessors(node).foreach({ (_, pred) => predecessors.add(pred) })
      nodesThatHaveACommonPredecessorWith(node) =
        predecessors.flatMap(pred => graph.successorRelations(pred).flatMap(rel => graph.successorsForRelation(pred, rel)))
    })
    //logger.println("Common predecessor relation: " + Util.formatTime(System.currentTimeMillis() - start3))
    var continueMultiThreaded = true
    while (continueMultiThreaded && R.keySet.nonEmpty) {

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
      var parallelism = 0

      while (remainingNodes.nonEmpty) {
        val x = remainingNodes.head
        remainingNodes -= x
        remainingNodes --= nodesThatHaveACommonPredecessorWith(x)
        //remainingNodes &~= nodesThatHaveACommonPredecessorWith(x)
        parallelism += 1
        futures.add(scala.concurrent.Future {
          //          The code in Phase 2:
          //          R(x) = ConcurrentBitMap[OWLObjectProperty](graph.nodes().size - 1)
          //          for (y <- simulation.row(x).viewAsImmutableBitSet) {
          //            for (r <- predecessors(y).keySet) {
          //              R(x).rowMap.getOrElseUpdate(r, concurrent.ConcurrentBoundedBitSet2(graph.nodes().toBitMask))
          //                .viewAsMutableBitSet.subtractAll(predecessors(y)(r))
          //            }
          //          }
          val Rx = R.remove(x).get
          Rx.rows().foreach({ r =>
            logger.tick()
            // TODO: Use Java's Fork/Join Framework
            graph.predecessorsForRelation(x, r).foreach { xx =>
              val simRow = simulation.row(xx)
              val simRowView = simRow.viewAsImmutableBitSet
              (Rx.rowImmutable(r) intersect simRowView).foreach { yy =>
                simRow.remove(yy)
                simulation.col(yy).remove(xx)
                graph.predecessors(yy).foreach { (rr, yyy) =>
                  val successors = graph.successorsForRelation(yyy, rr)
                  if ((simRowView intersect successors).isEmpty) {
                    addToR(xx, rr, yyy)
                  }
                }
              }
            }
          })
        })
      }

      futures.asScala.awaitEach()

      continueMultiThreaded = parallelism > 2

    }

    logger.println()
    logger.println("Now continuing single-threaded...")

    while (R.keySet.nonEmpty) {
      val x = R.keySet.head
      // TODO: Define a method for the following code as it is a duplicate of the above code in the Future.
      val Rx = R.remove(x).get
      Rx.rows().foreach({ r =>
        logger.tick()
        for (xx <- graph.predecessorsForRelation(x, r)) {
          val simRow = simulation.row(xx)
          val simRowView = simRow.viewAsImmutableBitSet
          for (yy <- Rx.rowImmutable(r) intersect simRowView) {
            simRow.remove(yy)
            simulation.col(yy).remove(xx)
            for ((rr, yyy) <- graph.predecessors(yy)) {
              val successors = graph.successorsForRelation(yyy, rr)
              if ((simRowView intersect successors).isEmpty) {
                addToR(xx, rr, yyy)
              }
            }
          }
        }
      })
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
  def computeMaximalSimulation2[L, R](source: BitGraph[L, R], target: BitGraph[L, R])(using logger: Logger): ConcurrentArrayBitBiMap = {

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
//                target.predecessors(yy).foreach { (rr, yyy) =>
//                  if ((simRowView intersect target.successorsForRelation(yyy, rr)).isEmpty) {
//                    addToR(xx, rr, yyy)
//                  }
//                }
                val Rxx = R.getOrElseUpdate(xx, ConcurrentBitMap[R](target.nodes().size - 1))
                predecessors(yy).foreach({ (rr, yyys) =>
//                  newRxxrr =
                })
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

//  def maximalSimulationOn(graph: BitGraph[OWLClass, OWLObjectProperty])(using logger: Logger): ConcurrentBitBiMap = {
//
//    logger.println("Computing pre-simulation...")
//    val start1 = System.currentTimeMillis()
//    val simulation = ConcurrentBitBiMap(graph.nodes().size, graph.nodes().size)
//
////    implicit val ec = scala.concurrent.ExecutionContext.fromExecutor(java.util.concurrent.ThreadPoolExecutor(Runtime.getRuntime.availableProcessors(), Runtime.getRuntime.availableProcessors(), 5L, java.util.concurrent.TimeUnit.SECONDS, LinkedBlockingQueue[Runnable]()))
////    implicit val ec = scala.concurrent.ExecutionContext.fromExecutor(java.util.concurrent.Executors.newWorkStealingPool())
//
////    graph.nodes().unsorted.map(y => scala.concurrent.Future {
//////      logger.tick()
////      val yLabels = graph.labels(y)
////      val yProperties = graph.successorRelations(y)
////      for (x <- graph.nodes() if !(x equals y)) {
////        if ((graph.labels(x) subsetOf yLabels) && (graph.successorRelations(x) subsetOf yProperties)) {
////          simulation.add(x, y)
////        }
////      }
////    }).foreach(scala.concurrent.Await.ready(_, scala.concurrent.duration.Duration.Inf))
//
//    import de.tu_dresden.inf.lat.parallel.ParallelComputation._
//
//    graph.nodes().unsorted.map(y => scala.concurrent.Future {
//      //      logger.tick()
//      val yLabels = graph.labels(y)
//      val yProperties = graph.successorRelations(y)
//      for (x <- graph.nodes() if !(x equals y)) {
//        if ((graph.labels(x) subsetOf yLabels) && (graph.successorRelations(x) subsetOf yProperties)) {
//          simulation.add(x, y)
//        }
//      }
//    }).awaitEach()
//    println("Phase 1: " + Util.formatTime(System.currentTimeMillis() - start1))
////    logger.reset()
//
//    logger.println("Computing initial mapping R(x,r)...")
//    val start2 = System.currentTimeMillis()
//    val bitR = ConcurrentBitMap[(Int, OWLObjectProperty)](graph.nodes().size - 1)
//
////    graph.nodes().unsorted.map(x => scala.concurrent.Future {
//////      logger.tick()
////      for (yy <- graph.nodes()) {
////        for (property <- graph.successorRelations(yy)) {
////          val successors = graph.successorsForRelation(yy, property)
////          if (!successors.contains(x) && (simulation.rowImmutable(x) intersect successors).isEmpty) {
////            bitR.add((x, property), yy)
////          }
////        }
////      }
////    }).foreach(scala.concurrent.Await.ready(_, scala.concurrent.duration.Duration.Inf))
//
//    graph.nodes().unsorted.map(x => scala.concurrent.Future {
//      //      logger.tick()
//      for (yy <- graph.nodes()) {
//        for (property <- graph.successorRelations(yy)) {
//          val successors = graph.successorsForRelation(yy, property)
//          if (!successors.contains(x) && (simulation.rowImmutable(x) intersect successors).isEmpty) {
//            bitR.add((x, property), yy)
//          }
//        }
//      }
//    }).awaitEach()
//    println("Phase 2: " + Util.formatTime(System.currentTimeMillis() - start2))
////    logger.reset()
//
//    logger.println("Computing simulation...")
//    val start3 = System.currentTimeMillis()
//    while (bitR.rows().nonEmpty) {
////      logger.tick()
//      val (x, r) = bitR.rows().head
//      graph.predecessorsForRelation(x, r).unsorted.map(xx => scala.concurrent.Future {
//        val row = simulation.row(xx)
//        if (row.isDefined) {
//          (bitR.rowImmutable((x, r)) intersect row.get.asImmutableBitSet()).foreach { yy =>
////            simulation.remove(xx, yy)
//            row.get.remove(yy)
//            simulation.col(yy).get.remove(xx)
//            graph.predecessors(yy).foreach { (rr, yyy) =>
//              val successors = graph.successorsForRelation(yyy, rr)
//              if (!successors.contains(xx) && (row.get.asImmutableBitSet() intersect successors).isEmpty) {
//                bitR.add((xx, rr), yyy)
//              }
//            }
//          }
//        }
//      }).awaitEach()
//      bitR.clearRow((x, r))
//    }
//
////    while (bitR.rows().nonEmpty) {
////      //      logger.tick()
////      val (x, r) = bitR.rows().head
////      graph.predecessorsForRelation(x, r).foreach { xx =>
////        val row = simulation.row(xx)
////        if (row.isDefined) {
////          (bitR.rowImmutable((x, r)) intersect row.get.asImmutableBitSet()).unsorted.map(yy => scala.concurrent.Future {
////            //            simulation.remove(xx, yy)
////            row.get.remove(yy)
////            simulation.col(yy).get.remove(xx)
////            graph.predecessors(yy).foreach { (rr, yyy) =>
////              val successors = graph.successorsForRelation(yyy, rr)
////              if (!(successors contains xx) && (row.get.asImmutableBitSet() intersect successors).isEmpty) {
////                bitR.add((xx, rr), yyy)
////              }
////            }
////          }).foreach(scala.concurrent.Await.ready(_, scala.concurrent.duration.Duration.Inf))
////        }
////      }
////      bitR.clearRow((x, r))
////    }
//
////    while (bitR.rows().nonEmpty) {
////      //      logger.tick()
////      val (x, r) = bitR.rows().head
////      graph.predecessorsForRelation(x, r).foreach { xx =>
////        val row = simulation.row(xx)
////        if (row.isDefined) {
////          (bitR.rowImmutable((x, r)) intersect row.get.asImmutableBitSet()).foreach(yy => {
////            //            simulation.remove(xx, yy)
////            row.get.remove(yy)
////            simulation.col(yy).get.remove(xx)
////            graph.predecessors(yy).foreach { (rr, yyy) =>
////              val successors = graph.successorsForRelation(yyy, rr)
////              if (!(successors contains xx) && (row.get.asImmutableBitSet() intersect successors).isEmpty) {
////                bitR.add((xx, rr), yyy)
////              }
////            }
////          })
////        }
////      }
////      bitR.clearRow((x, r))
////    }
//    println("Phase 3: " + Util.formatTime(System.currentTimeMillis() - start3))
//    logger.reset()
//
//    simulation
//
//  }

}
