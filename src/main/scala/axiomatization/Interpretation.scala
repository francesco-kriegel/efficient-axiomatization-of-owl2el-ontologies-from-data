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

  def computeReduction(graph: BitGraph[OWLClass, OWLObjectProperty])(using logger: Logger):
  (BitGraph[OWLClass, OWLObjectProperty], Int => mutable.BitSet, Int => Int, ConcurrentArrayBitBiMap) = {

    val simulation = GraphSimulator.computeMaximalSimulation(graph, graph)

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
