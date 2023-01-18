package de.tu_dresden.inf.lat
package axiomatization

import collection.parallel.CollectionConverters.*
import java.io.File
import org.phenoscape.scowl.*
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.model.{NodeID, OWLAnonymousIndividual, OWLClass, OWLClassExpression, OWLIndividual, OWLObjectProperty, OWLOntology}
import uk.ac.manchester.cs.owl.owlapi.OWLAnonymousIndividualImpl

import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.StreamConverters.*

import de.tu_dresden.inf.lat.parallel.NestedParallelComputations._

object Interpretation {

  def fromOntology(ontology: OWLOntology): BitGraph[OWLClass, OWLObjectProperty] = {

    val variables = mutable.HashMap[OWLClassExpression, OWLAnonymousIndividual]()

    //    def variableFor(classExpression: OWLClassExpression): OWLAnonymousIndividual = {
    //      variables.getOrElseUpdate(classExpression, new OWLAnonymousIndividualImpl(NodeID.getNodeID()))
    //    }
    def variableFor = variables.getOrElseUpdate(_, new OWLAnonymousIndividualImpl(NodeID.getNodeID()))

    def unfoldABoxAssertion(classExpression: OWLClassExpression, individual: OWLIndividual): Unit = {
      classExpression.conjunctSet().forEach({

        case owlClass@Class(_) if !(owlClass equals OWLNothing) =>
          ontology.addAxiom(individual Type owlClass)

        case ObjectSomeValuesFrom(property@ObjectProperty(_), filler) =>
          val successor = variableFor(filler)
          ontology.addAxiom(individual Fact(property, successor))
          unfoldABoxAssertion(filler, successor)

        case ObjectMinCardinality(cardinality, property@ObjectProperty(_), filler) if cardinality > 0 =>
          val successor = variableFor(filler)
          ontology.addAxiom(individual Fact(property, successor))
          unfoldABoxAssertion(filler, successor)

        case ObjectExactCardinality(cardinality, property@ObjectProperty(_), filler) if cardinality > 0 =>
          val successor = variableFor(filler)
          ontology.addAxiom(individual Fact(property, successor))
          unfoldABoxAssertion(filler, successor)

        case ObjectHasSelf(property@ObjectProperty(_)) =>
          ontology.addAxiom(individual Fact(property, individual))

        case ObjectHasValue(property@ObjectProperty(_), successor) =>
          ontology.addAxiom(individual Fact(property, successor))

        case _ =>
//          Console.err.println("Unsupported class expression in ABox: " + classExpression)
      })
    }

    ontology.aboxAxioms(Imports.INCLUDED).toScala(LazyList)
      .foreach({
        case ClassAssertion(_, classExpression, individual) =>
          unfoldABoxAssertion(classExpression, individual)
        case _ => {}
      })

    val graph = BitGraph[OWLClass, OWLObjectProperty]()
    val index = mutable.HashMap[OWLIndividual, Int]()
    var k = 0
    (ontology.individualsInSignature().toScala(LazyList) concat ontology.anonymousIndividuals().toScala(LazyList))
      .foreach(individual => {
        index(individual) = k
//        graph.addNode(k)
        k += 1
      })
    graph.nodes().addAll(0 until k)
    ontology.classesInSignature().toScala(LazyList).filterNot(_ equals OWLThing).filterNot(_ equals OWLNothing)
      .foreach(graph.labels().addOne(_))
//    graph.labels().addAll(ontology.classesInSignature().toScala(LazyList).filterNot(_ equals OWLThing).filterNot(_ equals OWLNothing))
    ontology.objectPropertiesInSignature().toScala(LazyList)
      .foreach(graph.relations().addOne(_))
//    graph.relations().addAll(ontology.objectPropertiesInSignature().toScala(LazyList))
    ontology.axioms().toScala(LazyList)
      .foreach({
        case ClassAssertion(_, c@Class(_), x) if !(c equals OWLNothing) && !(c equals OWLThing) =>
          graph.addLabel(index(x), c)
        case ObjectPropertyAssertion(_, property@ObjectProperty(_), source, target) =>
          graph.addEdge(index(source), property, index(target))
        case ax =>
        //          Console.err.println("Unsupported assertion in ABox: " + ax)
      })

    graph

  }

  @Deprecated(forRemoval = true)
  def maximalSimulationOn(graph: BitGraph[OWLClass, OWLObjectProperty])(using logger: Logger): BitBiMap = {

    logger.reset()
    logger.println("Computing pre-simulation...")
    val start1 = System.currentTimeMillis()
    //    val simulation = ConcurrentReflexiveRelation[OWLIndividual]()
    //    val simulation = ReflexiveBitRelation[OWLIndividual]()
    val simulation = BitBiMap()
    //    val nodesPar = new scala.collection.parallel.mutable.ParArray[OWLIndividual](graph.nodes.toArray)

    for (y <- graph.nodes().par) {
      logger.tick()
      val yLabels = graph.labels(y)
      val yProperties = graph.successorRelations(y)
      //      for (x <- nodesPar if !(x equals y)) {
      for (x <- graph.nodes() if !(x equals y)) {
        if ((graph.labels(x) subsetOf yLabels)
          && (graph.successorRelations(x) subsetOf yProperties)) {
          simulation.synchronized {
            simulation.add(x, y)
          }
        }
      }
    }
    logger.reset()
    println("Phase 1: " + Util.formatTime(System.currentTimeMillis() - start1))

    logger.println("Computing initial mapping R(x,r)...")
    val start2 = System.currentTimeMillis()
    //     val R = new mutable.HashMap[(OWLIndividual, OWLObjectProperty), mutable.Set[OWLIndividual]]
    //    val bitR = new LongBitRelation[(OWLIndividual, OWLObjectProperty), OWLIndividual]
    val bitR = BitMap[(Int, OWLObjectProperty)]()
//    val bitR = ConcurrentBitMap[(Int, OWLObjectProperty)](graph.nodes().size - 1)

    for (x <- graph.nodes().par) {
      logger.tick()
      for (yy <- graph.nodes()) {
        for (property <- graph.successorRelations(yy)) {
          //          if (!graph.successorsForRelation(yy, property).exists(simulation(x, _))) {
          if (!graph.successorsForRelation(yy, property).contains(x)
            && (simulation.row(x) intersect graph.successorsForRelation(yy, property)).isEmpty) {
            //            R.synchronized {
            //              R.getOrElseUpdate((x, property), { mutable.HashSet[OWLIndividual]() }).addOne(yy)
            //            }
            bitR.synchronized {
              bitR.add((x, property), yy)
            }
          }
        }
      }
    }
    logger.reset()
    println("Phase 2: " + Util.formatTime(System.currentTimeMillis() - start2))

    logger.println("Computing simulation...")
    val start3 = System.currentTimeMillis()
//    @tailrec def loop(): Unit = {
//      ////      if (R.nonEmpty) {
//      //      val opt = bitR.nonEmptyRow()
//      //      if (opt.isDefined) {
//      ////        val (x, r) = R.keySet.head
//      //        val (x, r) = opt.get
//      if (bitR.rows().nonEmpty) {
//        logger.tick()
//        val (x, r) = bitR.rows().head
//        for (xx <- graph.predecessorsForRelation(x, r)) {
//          ////          for (yy <- R(x, r)) {
//          //          for (yy <- bitR.rowAsIterable((x, r)))
//          //            if (simulation(xx, yy)) {
//          //              simulation.remove(xx, yy)
//          val row = simulation.row(xx)
////          for (yy <- bitR.rowImmutable((x, r)) intersect row) {
//          for (yy <- bitR.row((x, r)) intersect row) {
//            row.remove(yy)
//            simulation.col(yy).remove(xx)
//            for ((rr, yyy) <- graph.predecessors(yy)) {
//              //                if (!graph.successorsForRelation(yyy, rr).exists(simulation(xx, _))) {
//              if (!graph.successorsForRelation(yyy, rr).contains(xx)
//                && (row intersect graph.successorsForRelation(yyy, rr)).isEmpty) {
//                //                  R.getOrElseUpdate((xx, rr), { mutable.HashSet[OWLIndividual]() }).addOne(yyy)
//                bitR.add((xx, rr), yyy)
//              }
//            }
//            //            }
//          }
//        }
//        //        R.remove((x, r))
//        bitR.clearRow((x, r))
//        loop()
//      }
//    }
//    loop()
    while (bitR.rows().nonEmpty) {
      logger.tick()
      val (x, r) = bitR.rows().head
      //val bitRRow = bitR.row((x, r))
      //bitR.clearRow((x, r))
      val bitRRow = bitR.clearRow((x, r)).get
      for (xx <- graph.predecessorsForRelation(x, r)) {
        val simRow = simulation.row(xx)
        for (yy <- bitRRow intersect simRow) {
          simRow.remove(yy)
          simulation.col(yy).remove(xx)
          for ((rr, yyy) <- graph.predecessors(yy)) {
            val successors = graph.successorsForRelation(yyy, rr)
            if (!(successors contains xx) && (simRow intersect successors).isEmpty) {
              bitR.add((xx, rr), yyy)
            }
          }
        }
      }
    }

    logger.reset()
    println("Phase 3: " + Util.formatTime(System.currentTimeMillis() - start3))
    simulation

  }

  @Deprecated(forRemoval = true)
  def reductionOf(graph: BitGraph[OWLClass, OWLObjectProperty])(using logger: Logger):
  (BitGraph[OWLClass, OWLObjectProperty], Int => mutable.BitSet, Int => Int, BitBiMap) = {

    val simulation = maximalSimulationOn(graph)

    logger.println("Computing equivalence classes...")

    //    val equivalenceClasses = mutable.ListBuffer[collection.Set[OWLIndividual]]()
    //    val remainingNodes = mutable.HashSet[OWLIndividual]()
    val equivalenceClasses = mutable.ListBuffer[mutable.BitSet]()
    val remainingNodes = mutable.BitSet()
    remainingNodes.addAll(graph.nodes())
    while (remainingNodes.nonEmpty) {
      logger.tick()
      val representative = remainingNodes.head
      //      val equivalenceClass = simulation.rowAsArrayBuffer(representative).filter(simulation.colAsSet(representative)(_))
      //      val equivalenceClass = simulation.rowAsSet(representative) intersect simulation.colAsSet(representative)
      val equivalenceClass = (simulation.row(representative) intersect simulation.col(representative)) + representative
      equivalenceClasses.addOne(equivalenceClass)
      //      remainingNodes.filterInPlace(!equivalenceClass(_))
      //      equivalenceClass.foreach(remainingNodes.remove(_))
      remainingNodes --= equivalenceClass
    }
    logger.reset()

    logger.println(equivalenceClasses.size + " equivalence classes")

    logger.println("Computing reduction...")

    //    equivalenceClasses.map(_.hashCode).foreach(reduction.addNode(_))
    //    val index = mutable.HashMap[collection.Set[OWLIndividual], Int]()
    val index = mutable.HashMap[mutable.BitSet, Int]()
    val representedBy = new Array[Int](graph.nodes().size) // mutable.HashMap[Int, Int]()
    val representativeOf = new Array[mutable.BitSet](equivalenceClasses.size) // mutable.HashMap[Int, mutable.BitSet]()
//    var i = 1
    var i = 0
    for (xs <- equivalenceClasses) {
      index(xs) = i
      xs.foreach(representedBy(_) = i)
      representativeOf(i) = xs
      i += 1
    }
//    val n = i - 1
    val reduction = BitGraph[OWLClass, OWLObjectProperty]()
    val simulationOnReduction = BitBiMap()
    reduction.nodes().addAll(0 until i)
    reduction.labels().addAll(graph.labels())
    reduction.relations().addAll(graph.relations())
    //    val ecPar = new scala.collection.parallel.mutable.ParArray[Set[OWLIndividual]](equivalenceClasses.toArray)
    for (xs <- equivalenceClasses.par) {
      logger.tick()
      val i = index(xs)
      val x = xs.head
//      reduction.addNode(index(xs))
      //      graph.labels(xs.head).foreach(reduction.addLabel(index(xs), _))
      val labels = graph.labels(x)
      if (labels.nonEmpty)
        reduction._labelsByNode.synchronized {
          reduction.addLabels(i, labels)
        }
      for (ys <- equivalenceClasses) {
        val j = index(ys)
        val y = ys.head
        if (simulation(x, y)) {
          simulationOnReduction.synchronized {
            simulationOnReduction.add(i, j)
          }
        }
        graph.successorRelations(x).foreach(r => {
          //          if (graph.successorsForRelation(xs.head, r).exists(simulation(ys.head, _))
          //            && graph.successorsForRelation(xs.head, r).forall(z => !simulation(ys.head, z) || simulation(z, ys.head))) {
          // if ((graph.successorsForRelation(x, r) intersect (simulation.row(y) + y)).nonEmpty
          //  && (graph.successorsForRelation(x, r) intersect (simulation.row(y) diff simulation.col(y))).isEmpty) {
          val successors = graph.successorsForRelation(x, r)
          val sir = successors intersect simulation.row(y)
          if ((successors.contains(y) || sir.nonEmpty) && (sir diff simulation.col(y)).isEmpty) {
            reduction.synchronized {
              reduction.addEdge(i, r, j)
            }
          }
        })
      }
    }
    logger.reset()

    (reduction, representativeOf, representedBy, simulationOnReduction)

  }

  def reductionOf2(graph: BitGraph[OWLClass, OWLObjectProperty])(using logger: Logger):
  (BitGraph[OWLClass, OWLObjectProperty], Int => mutable.BitSet, Int => Int, ConcurrentArrayBitBiMap) = {

    val simulation = GraphSimulator.computeMaximalSimulation3(graph, graph)

    logger.println("Computing equivalence classes...")

    val equivalenceClasses = mutable.ListBuffer[mutable.BitSet]()
    val remainingNodes = mutable.BitSet()
    remainingNodes.addAll(graph.nodes())
    while (remainingNodes.nonEmpty) {
      logger.tick()
      val representative = remainingNodes.head
      val equivalenceClass = simulation.row(representative).viewAsMutableBitSet intersect simulation.col(representative).viewAsMutableBitSet
      equivalenceClasses.addOne(equivalenceClass)
      remainingNodes --= equivalenceClass
    }
    logger.reset()

    logger.println(equivalenceClasses.size + " equivalence classes")

    logger.println("Computing reduction...")

    val index = mutable.HashMap[mutable.BitSet, Int]()
    val representedBy = new Array[Int](graph.nodes().size)
    val representativeOf = new Array[mutable.BitSet](equivalenceClasses.size)
    var i = 0
    equivalenceClasses.foreach(xs => {
      index(xs) = i
      xs.foreach(representedBy(_) = i)
      representativeOf(i) = xs
      i += 1
    })
    val reduction = BitGraph[OWLClass, OWLObjectProperty]()
    val simulationOnReduction = ConcurrentArrayBitBiMap(i, i)
    reduction.nodes().addAll(0 until i)
    reduction.labels().addAll(graph.labels())
    reduction.relations().addAll(graph.relations())
    equivalenceClasses.foreachPar(xs => {
      logger.tick()
      val i = index(xs)
      val x = xs.head
      val labels = graph.labels(x)
      if (labels.nonEmpty)
        reduction._labelsByNode.synchronized {
          reduction.addLabels(i, labels)
        }
      equivalenceClasses.foreach(ys => {
        val j = index(ys)
        val y = ys.head
        if (simulation(x, y)) {
          simulationOnReduction.add(i, j)
        }
        graph.successorRelations(x).foreach(r => {
          val successors = graph.successorsForRelation(x, r)
          val sir = successors intersect simulation.row(y).viewAsMutableBitSet
          if (sir.nonEmpty && (sir diff simulation.col(y).viewAsMutableBitSet).isEmpty) {
            reduction.synchronized {
              reduction.addEdge(i, r, j)
            }
          }
        })
      })
    })
    logger.reset()

    (reduction, representativeOf, representedBy, simulationOnReduction)

  }

}
