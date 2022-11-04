package de.tu_dresden.inf.lat
package axiomatization

import collection.parallel.CollectionConverters.*
import java.io.File
import org.phenoscape.scowl.*
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.model.{NodeID, OWLAnonymousIndividual, OWLClass, OWLClassExpression, OWLIndividual, OWLObjectProperty}
import uk.ac.manchester.cs.owl.owlapi.OWLAnonymousIndividualImpl
import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.StreamConverters.*

import de.tu_dresden.inf.lat.axiomatization.Util.GLOBAL_COUNTER

object Interpretation {

  def loadFromOntologyFile(ontologyFile: File): BitGraph[OWLClass, OWLObjectProperty] = {

    val manager = OWLManager.createOWLOntologyManager()
    val ontology = manager.loadOntologyFromOntologyDocument(ontologyFile)

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
        case _ =>
          Console.err.println("Unsupported class expression: " + classExpression)
      })
    }

    ontology.aboxAxioms(Imports.INCLUDED).toScala(LazyList)
      .foreach({
        case ClassAssertion(_, classExpression, individual) =>
          unfoldABoxAssertion(classExpression, individual)
        case _ => {}
      })

    //    val graph = OntologyGraph(ontology)
    //    val graph = HashGraph.fromOntology(ontology)
    val graph = BitGraph.fromOntology(ontology)
    manager.clearOntologies()

    graph

  }

  def maximalSimulationOn(graph: BitGraph[OWLClass, OWLObjectProperty]): BitBiMap = {

    println("Computing pre-simulation...")

    //    val simulation = ConcurrentReflexiveRelation[OWLIndividual]()
    //    val simulation = ReflexiveBitRelation[OWLIndividual]()
    val simulation = BitBiMap()
    //    val nodesPar = new scala.collection.parallel.mutable.ParArray[OWLIndividual](graph.nodes.toArray)

    for (y <- graph.nodes().par) {
      GLOBAL_COUNTER.tick()
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
    GLOBAL_COUNTER.reset()

    println("Computing initial mapping R(x,r)...")

    //     val R = new mutable.HashMap[(OWLIndividual, OWLObjectProperty), mutable.Set[OWLIndividual]]
    //    val bitR = new LongBitRelation[(OWLIndividual, OWLObjectProperty), OWLIndividual]
    val bitR = BitMap[(Int, OWLObjectProperty)]()

    for (x <- graph.nodes().par) {
      GLOBAL_COUNTER.tick()
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

    GLOBAL_COUNTER.reset()

    println("Computing simulation...")

    @tailrec
    def loop(): Unit = {
      GLOBAL_COUNTER.tick()
      ////      if (R.nonEmpty) {
      //      val opt = bitR.nonEmptyRow()
      //      if (opt.isDefined) {
      ////        val (x, r) = R.keySet.head
      //        val (x, r) = opt.get
      if (bitR.rows().nonEmpty) {
        val (x, r) = bitR.rows().head
        for (xx <- graph.predecessorsForRelation(x, r)) {
          ////          for (yy <- R(x, r)) {
          //          for (yy <- bitR.rowAsIterable((x, r)))
          //            if (simulation(xx, yy)) {
          //              simulation.remove(xx, yy)
          val row = simulation.row(xx)
          for (yy <- bitR.row((x, r)) intersect row) {
            row.remove(yy)
            simulation.col(yy).remove(xx)
            for ((rr, yyy) <- graph.predecessors(yy)) {
              //                if (!graph.successorsForRelation(yyy, rr).exists(simulation(xx, _))) {
              if (!graph.successorsForRelation(yyy, rr).contains(xx)
                && (row intersect graph.successorsForRelation(yyy, rr)).isEmpty) {
                //                  R.getOrElseUpdate((xx, rr), { mutable.HashSet[OWLIndividual]() }).addOne(yyy)
                bitR.add((xx, rr), yyy)
              }
            }
            //            }
          }
        }
        //        R.remove((x, r))
        bitR.clearRow((x, r))
        loop()
      }
    }

    loop()
    GLOBAL_COUNTER.reset()

    simulation

  }

  def reductionOf(graph: BitGraph[OWLClass, OWLObjectProperty]): BitGraph[OWLClass, OWLObjectProperty] = {

    val simulation = maximalSimulationOn(graph)

    println("Computing equivalence classes...")

    //    val equivalenceClasses = mutable.ListBuffer[collection.Set[OWLIndividual]]()
    //    val remainingNodes = mutable.HashSet[OWLIndividual]()
    val equivalenceClasses = mutable.ListBuffer[mutable.BitSet]()
    val remainingNodes = mutable.BitSet()
    remainingNodes.addAll(graph.nodes())
    while (remainingNodes.nonEmpty) {
      GLOBAL_COUNTER.tick()
      val representative = remainingNodes.head
      //      val equivalenceClass = simulation.rowAsArrayBuffer(representative).filter(simulation.colAsSet(representative)(_))
      //      val equivalenceClass = simulation.rowAsSet(representative) intersect simulation.colAsSet(representative)
      val equivalenceClass = (simulation.row(representative) intersect simulation.col(representative)) + representative
      equivalenceClasses.addOne(equivalenceClass)
      //      remainingNodes.filterInPlace(!equivalenceClass(_))
      //      equivalenceClass.foreach(remainingNodes.remove(_))
      remainingNodes --= equivalenceClass
    }
    GLOBAL_COUNTER.reset()

    println(equivalenceClasses.size + " equivalence classes")

    println("Computing reduction...")

    val reduction = BitGraph[OWLClass, OWLObjectProperty]()
    reduction.labels().addAll(graph.labels())
    reduction.relations().addAll(graph.relations())
    //    equivalenceClasses.map(_.hashCode).foreach(reduction.addNode(_))
    //    val index = mutable.HashMap[collection.Set[OWLIndividual], Int]()
    val index = mutable.HashMap[mutable.BitSet, Int]()
    var i = 1
    for (xs <- equivalenceClasses) {
      index(xs) = i
      i += 1
    }
//    val n = i - 1
    //    val ecPar = new scala.collection.parallel.mutable.ParArray[Set[OWLIndividual]](equivalenceClasses.toArray)
    for (xs <- equivalenceClasses) {
      GLOBAL_COUNTER.tick()
      reduction.addNode(index(xs))
      //      graph.labels(xs.head).foreach(reduction.addLabel(index(xs), _))
      reduction.addLabels(index(xs), graph.labels(xs.head))
      for (ys <- equivalenceClasses) {
        graph.successorRelations(xs.head).foreach(r => {
          //          if (graph.successorsForRelation(xs.head, r).exists(simulation(ys.head, _))
          //            && graph.successorsForRelation(xs.head, r).forall(z => !simulation(ys.head, z) || simulation(z, ys.head))) {
          if ((graph.successorsForRelation(xs.head, r) intersect (simulation.row(ys.head) + ys.head)).nonEmpty
            && (graph.successorsForRelation(xs.head, r) intersect (simulation.row(ys.head) diff simulation.col(ys.head))).isEmpty) {
            reduction.addEdge(index(xs), r, index(ys))
          }
        })
      }
    }
    GLOBAL_COUNTER.reset()

    reduction

  }

}
