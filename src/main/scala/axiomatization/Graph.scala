package de.tu_dresden.inf.lat
package axiomatization

import org.phenoscape.scowl.*
import org.semanticweb.owlapi.model.{OWLClass, OWLIndividual, OWLObjectProperty, OWLOntology}

import scala.collection.{IterableOps, StrictOptimizedIterableOps, mutable}
import scala.collection.mutable.ListBuffer
import scala.jdk.StreamConverters.*

//trait Graph[C[_] <: Iterable[_] with IterableOps[_, C, C[_]] with StrictOptimizedIterableOps[_, C, C[_]], N, L, R] {
trait Graph[C[_] <: Iterable[_], N, L, R] {

  def nodes: C[N]
  def labels(node: N): C[L]

  def successors(node: N): C[(R, N)]
  def predecessors(node: N): C[(R, N)]

  def successorRelations(node: N): C[R]
  def predecessorRelations(node: N): C[R]

  def successorsForRelation(node: N, relation: R): C[N]
  def predecessorsForRelation(node: N, relation: R): C[N]

//  def successorRelations(node: N): C[R] = successors(node).map({ case (r: R, _) => r })
//  def predecessorRelations(node: N): C[R] = predecessors(node).map({ case (r: R, _) => r })
//
//  def successorsForRelation(node: N, relation: R): C[N] = successors(node).collect({ case (property, target) if property equals relation => target })
//  def predecessorsForRelation(node: N, relation: R): C[N] = predecessors(node).collect({ case (property, source) if property equals relation => source })

}

class ListGraph[N, L, R](val initNodes: N*) extends Graph[mutable.ListBuffer, N, L, R] {
  val _nodes = mutable.ListBuffer[N]()
  val _labels = new mutable.HashMap[N, mutable.ListBuffer[L]]
  val _successors = new mutable.HashMap[N, mutable.ListBuffer[(R, N)]]
  val _predecessors = new mutable.HashMap[N, mutable.ListBuffer[(R, N)]]

  def nodes: mutable.ListBuffer[N] =
    _nodes
  def labels(node: N): mutable.ListBuffer[L] =
    _labels.getOrElse(node, mutable.ListBuffer.empty)

  def successors(node: N): mutable.ListBuffer[(R, N)] =
    _successors.getOrElse(node, mutable.ListBuffer.empty)
  def predecessors(node: N): mutable.ListBuffer[(R, N)] =
    _predecessors.getOrElse(node, mutable.ListBuffer.empty)

  def successorRelations(node: N): mutable.ListBuffer[R] =
    successors(node).map({ case (r, _) => r })
  def predecessorRelations(node: N): mutable.ListBuffer[R] =
    predecessors(node).map({ case (r, _) => r })

  def successorsForRelation(node: N, relation: R): mutable.ListBuffer[N] =
    successors(node).collect({ case (property, target) if property equals relation => target })
  def predecessorsForRelation(node: N, relation: R): mutable.ListBuffer[N] =
    predecessors(node).collect({ case (property,source) if property equals relation => source })

  _nodes.addAll(initNodes)

  def addNode(node: N): Unit = {
    _nodes.addOne(node)
  }

  def addLabel(node: N, label: L): Unit = {
    _labels.getOrElseUpdate(node, { mutable.ListBuffer[L]() }).addOne(label)
  }

  def addLabels(node: N, labels: IterableOnce[L]): Unit = {
    _labels.getOrElseUpdate(node, { mutable.ListBuffer[L]() }).addAll(labels)
  }

  def addEdge(source: N, relation: R, target: N): Unit = {
    _successors.getOrElseUpdate(source, { mutable.ListBuffer[(R, N)]() }).addOne(relation, target)
    _predecessors.getOrElseUpdate(target, { mutable.ListBuffer[(R, N)]() }).addOne(relation, source)
  }

  def clear(): Unit = {
    _nodes.clear()
    _labels.clear()
    _successors.clear()
    _predecessors.clear()
  }

}

class HashGraph[N, L, R](val initNodes: N*) { //extends Graph[mutable.HashSet, N, L, R] {

  val _nodes = mutable.HashSet[N]()
  val _labels = new mutable.HashMap[N, mutable.HashSet[L]]

  val _successorRelations = new mutable.HashMap[N, mutable.HashSet[R]]
//  val _predecessorRelations = new mutable.HashMap[N, mutable.HashSet[R]]

  val _successorsByRelation = new mutable.HashMap[(N, R), mutable.HashSet[N]]
  val _predecessorsByRelation = new mutable.HashMap[(N, R), mutable.HashSet[N]]

//  val _successors = new mutable.HashMap[N, mutable.HashSet[(R, N)]]
  val _predecessors = new mutable.HashMap[N, mutable.HashSet[(R, N)]]

  def nodes: mutable.HashSet[N] =
    _nodes
  def labels(node: N): mutable.HashSet[L] =
    _labels.getOrElse(node, mutable.HashSet.empty)

  def successorRelations(node: N): mutable.HashSet[R] =
    _successorRelations.getOrElse(node, mutable.HashSet.empty)
//  def predecessorRelations(node: N): mutable.HashSet[R] =
//    _predecessorRelations.getOrElse(node, mutable.HashSet.empty)

  def successorsForRelation(node: N, relation: R): mutable.HashSet[N] =
    _successorsByRelation.getOrElse((node, relation), mutable.HashSet.empty)
  def predecessorsForRelation(node: N, relation: R): mutable.HashSet[N] =
    _predecessorsByRelation.getOrElse((node, relation), mutable.HashSet.empty)

//  def successors(node: N): mutable.HashSet[(R, N)] =
//    _successors.getOrElse(node, mutable.HashSet.empty)
  def predecessors(node: N): mutable.HashSet[(R, N)] =
    _predecessors.getOrElse(node, mutable.HashSet.empty)

  _nodes.addAll(initNodes)

  def addNode(node: N): Unit = {
    _nodes.addOne(node)
  }

  def addLabel(node: N, label: L): Unit = {
    _labels.getOrElseUpdate(node, { mutable.HashSet[L]() }).addOne(label)
  }

  def addLabels(node: N, labels: IterableOnce[L]): Unit = {
    _labels.getOrElseUpdate(node, { mutable.HashSet[L]() }).addAll(labels)
  }

  def addEdge(source: N, relation: R, target: N): Unit = {
    _successorRelations.getOrElseUpdate(source, { mutable.HashSet[R]() }).addOne(relation)
//    _predecessorRelations.getOrElseUpdate(target, { mutable.HashSet[R]() }).addOne(relation)
    _successorsByRelation.getOrElseUpdate((source, relation), { mutable.HashSet[N]() }).addOne(target)
    _predecessorsByRelation.getOrElseUpdate((target, relation), { mutable.HashSet[N]() }).addOne(source)
//    _successors.getOrElseUpdate(source, { mutable.HashSet[(R, N)]() }).addOne(relation, target)
    _predecessors.getOrElseUpdate(target, { mutable.HashSet[(R, N)]() }).addOne(relation, source)
  }

  def clear(): Unit = {
    _nodes.clear()
    _labels.clear()
    _successorRelations.clear()
//    _predecessorRelations.clear()
    _successorsByRelation.clear()
    _predecessorsByRelation.clear()
//    _successors.clear()
    _predecessors.clear()
  }

}

object HashGraph {

  def fromOntology(ontology: OWLOntology): HashGraph[OWLIndividual, OWLClass, OWLObjectProperty] = {
    val graph = HashGraph[OWLIndividual, OWLClass, OWLObjectProperty]()
    (ontology.individualsInSignature().toScala(LazyList) concat ontology.anonymousIndividuals().toScala(LazyList))
      .foreach(graph.addNode(_))
    ontology.axioms().toScala(LazyList)
      .foreach({
        case ClassAssertion(_, c @ Class(_), x) if !(c equals OWLNothing) && !(c equals OWLThing) =>
          graph.addLabel(x, c)
        case ObjectPropertyAssertion(_, property @ ObjectProperty(_), source, target) =>
          graph.addEdge(source, property, target)
        case ax =>
//          println("Ignored axiom: " + ax)
      })
    graph
  }

}

class BitGraph[L, R](val initNodes: Int*) { //extends Graph[mutable.HashSet, Int, L, R] {

  val _nodes = mutable.BitSet()
  val _labels = new mutable.HashMap[Int, mutable.HashSet[L]]

  val _successorRelations = new mutable.HashMap[Int, mutable.HashSet[R]]
  //  val _predecessorRelations = new mutable.HashMap[Int, mutable.HashSet[R]]

  val _successorsByRelation = new mutable.HashMap[(Int, R), mutable.BitSet]
  val _predecessorsByRelation = new mutable.HashMap[(Int, R), mutable.BitSet]

  //  val _successors = new mutable.HashMap[Int, mutable.HashSet[(R, Int)]]
  val _predecessors = new mutable.HashMap[Int, mutable.HashSet[(R, Int)]]

  def nodes: mutable.BitSet =
    _nodes
  def labels(node: Int): mutable.HashSet[L] =
    _labels.getOrElse(node, mutable.HashSet.empty)

  def successorRelations(node: Int): mutable.HashSet[R] =
    _successorRelations.getOrElse(node, mutable.HashSet.empty)
  //  def predecessorRelations(node: Int): mutable.HashSet[R] =
  //    _predecessorRelations.getOrElse(node, mutable.HashSet.empty)

  def successorsForRelation(node: Int, relation: R): mutable.BitSet =
    _successorsByRelation.getOrElse((node, relation), mutable.BitSet.empty)
  def predecessorsForRelation(node: Int, relation: R): mutable.BitSet =
    _predecessorsByRelation.getOrElse((node, relation), mutable.BitSet.empty)

  //  def successors(node: Int): mutable.HashSet[(R, Int)] =
  //    _successors.getOrElse(node, mutable.HashSet.empty)
  def predecessors(node: Int): mutable.HashSet[(R, Int)] =
    _predecessors.getOrElse(node, mutable.HashSet.empty)

  _nodes.addAll(initNodes)

  def addNode(node: Int): Unit = {
    _nodes.addOne(node)
  }

  def addLabel(node: Int, label: L): Unit = {
    _labels.getOrElseUpdate(node, { mutable.HashSet[L]() }).addOne(label)
  }

  def addLabels(node: Int, labels: IterableOnce[L]): Unit = {
    _labels.getOrElseUpdate(node, { mutable.HashSet[L]() }).addAll(labels)
  }

  def addEdge(source: Int, relation: R, target: Int): Unit = {
    _successorRelations.getOrElseUpdate(source, { mutable.HashSet[R]() }).addOne(relation)
    //    _predecessorRelations.getOrElseUpdate(target, { mutable.HashSet[R]() }).addOne(relation)
    _successorsByRelation.getOrElseUpdate((source, relation), { mutable.BitSet() }).addOne(target)
    _predecessorsByRelation.getOrElseUpdate((target, relation), { mutable.BitSet() }).addOne(source)
    //    _successors.getOrElseUpdate(source, { mutable.HashSet[(R, Int)]() }).addOne(relation, target)
    _predecessors.getOrElseUpdate(target, { mutable.HashSet[(R, Int)]() }).addOne(relation, source)
  }

  def clear(): Unit = {
    _nodes.clear()
    _labels.clear()
    _successorRelations.clear()
    //    _predecessorRelations.clear()
    _successorsByRelation.clear()
    _predecessorsByRelation.clear()
    //    _successors.clear()
    _predecessors.clear()
  }

}

class OntologyGraph(ontology: OWLOntology) extends Graph[LazyList, OWLIndividual, OWLClass, OWLObjectProperty] {

  val indexedOntology = new OWLOntologyWithFurtherIndexes(ontology)

  def nodes: LazyList[OWLIndividual] = {
    ontology.individualsInSignature().toScala(LazyList) concat ontology.anonymousIndividuals().toScala(LazyList)
  }

  def labels(node: OWLIndividual): LazyList[OWLClass] = {
    ontology.classAssertionAxioms(node).toScala(LazyList).collect({ case ClassAssertion(_, c @ Class(_), _) => c })
  }

  def successors(node: OWLIndividual): LazyList[(OWLObjectProperty, OWLIndividual)] = {
    indexedOntology.objectPropertyAssertionAxiomsWithSubject(node).toScala(LazyList)
      .collect({ case ObjectPropertyAssertion(_, property @ ObjectProperty(_), _, target) => (property, target) })
  }

  def predecessors(node: OWLIndividual): LazyList[(OWLObjectProperty, OWLIndividual)] = {
    indexedOntology.objectPropertyAssertionAxiomsWithObject(node).toScala(LazyList)
      .collect({ case ObjectPropertyAssertion(_, property@ObjectProperty(_), subject, _) => (property, subject) })
  }

  override def successorRelations(node: OWLIndividual): LazyList[OWLObjectProperty] = {
    successors(node).map({ case (property, _) => property })
  }

  override def predecessorRelations(node: OWLIndividual): LazyList[OWLObjectProperty] = {
    predecessors(node).map({ case (property, _) => property })
  }

  def successorsForRelation(node: OWLIndividual, relation: OWLObjectProperty): LazyList[OWLIndividual] = {
    successors(node).collect({ case (property, target) if property equals relation => target })
  }

  def predecessorsForRelation(node: OWLIndividual, relation: OWLObjectProperty): LazyList[OWLIndividual] = {
    predecessors(node).collect({ case (property, subject) if property equals subject => subject })
  }

}

object BitGraph {

  def fromOntology(ontology: OWLOntology): BitGraph[OWLClass, OWLObjectProperty] = {
    val graph = BitGraph[OWLClass, OWLObjectProperty]()
    val index = mutable.HashMap[OWLIndividual, Int]()
    var k = 0
    (ontology.individualsInSignature().toScala(LazyList) concat ontology.anonymousIndividuals().toScala(LazyList))
      .foreach(individual => {
        index(individual) = k
        graph.addNode(k)
        k += 1
      })
    ontology.axioms().toScala(LazyList)
      .foreach({
        case ClassAssertion(_, c @ Class(_), x) if !(c equals OWLNothing) && !(c equals OWLThing) =>
          graph.addLabel(index(x), c)
        case ObjectPropertyAssertion(_, property @ ObjectProperty(_), source, target) =>
          graph.addEdge(index(source), property, index(target))
        case ax =>
        //          println("Ignored axiom: " + ax)
      })
    graph
  }

}
