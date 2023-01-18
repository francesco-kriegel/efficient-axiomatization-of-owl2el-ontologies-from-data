package de.tu_dresden.inf.lat
package axiomatization

import org.phenoscape.scowl.*
import org.semanticweb.owlapi.model.{OWLClass, OWLIndividual, OWLObjectProperty, OWLOntology}

import scala.collection.BitSet.fromSpecific
import scala.collection.{IterableOps, StrictOptimizedIterableOps, mutable}
import scala.collection.mutable.ListBuffer
import scala.jdk.StreamConverters.*

class BitGraph[L, R]() extends Graph[Int, L, R, mutable.BitSet](() => mutable.BitSet()) {}

class HashGraph[N, L, R]() extends Graph[N, L, R, mutable.HashSet[N]](() => mutable.HashSet[N]()) {}

abstract class Graph[N, L, R, SetN <: mutable.Set[N]](newSetN: () => SetN) {

  val _nodes = newSetN()
  val _labels = mutable.HashSet[L]()
  val _relations = mutable.HashSet[R]()

  val _labelsByNode = new mutable.HashMap[N, mutable.HashSet[L]]

  val _successorRelations = new mutable.HashMap[N, mutable.HashSet[R]]
  val _predecessorRelations = new mutable.HashMap[N, mutable.HashSet[R]]

  val _successorsByRelation = new mutable.HashMap[(N, R), SetN]
  val _predecessorsByRelation = new mutable.HashMap[(N, R), SetN]

  //  val _successors = new mutable.HashMap[N, mutable.HashSet[(R, N)]]
  val _predecessors = new mutable.HashMap[N, mutable.HashSet[(R, N)]]

  def nodes(): SetN =
    _nodes

  def labels(): mutable.HashSet[L] =
    _labels

  def relations(): mutable.HashSet[R] =
    _relations

  def labels(node: N): mutable.HashSet[L] =
    _labelsByNode.getOrElse(node, mutable.HashSet.empty)

  private val emptyHashSet_R = mutable.HashSet.empty[R]
  private val emptyHashSet_R_N = mutable.HashSet.empty[(R, N)]
  private val emptySetN = newSetN()

  def successorRelations(node: N): mutable.HashSet[R] =
    _successorRelations.getOrElse(node, emptyHashSet_R)

  def predecessorRelations(node: N): mutable.HashSet[R] =
    _predecessorRelations.getOrElse(node, emptyHashSet_R)

  def successorsForRelation(node: N, relation: R): SetN =
    _successorsByRelation.getOrElse((node, relation), emptySetN)

  def predecessorsForRelation(node: N, relation: R): SetN =
    _predecessorsByRelation.getOrElse((node, relation), emptySetN)

  //  def successors(node: N): mutable.HashSet[(R, N)] =
  //    _successors.getOrElse(node, mutable.HashSet.empty)

  def predecessors(node: N): mutable.HashSet[(R, N)] =
    _predecessors.getOrElse(node, emptyHashSet_R_N)

  def addNode(node: N): Unit = {
    _nodes.addOne(node)
  }

  def addLabel(node: N, label: L): Unit = {
    _labelsByNode.getOrElseUpdate(node, { mutable.HashSet[L]() }).addOne(label)
  }

  def addLabels(node: N, labels: IterableOnce[L]): Unit = {
    _labelsByNode.getOrElseUpdate(node, { mutable.HashSet[L]() }).addAll(labels)
  }

  def addEdge(source: N, relation: R, target: N): Unit = {
    _successorRelations.getOrElseUpdate(source, { mutable.HashSet[R]() }).addOne(relation)
    _predecessorRelations.getOrElseUpdate(target, { mutable.HashSet[R]() }).addOne(relation)
    _successorsByRelation.getOrElseUpdate((source, relation), { newSetN() }).addOne(target)
    _predecessorsByRelation.getOrElseUpdate((target, relation), { newSetN() }).addOne(source)
    //    _successors.getOrElseUpdate(source, { mutable.HashSet[(R, N)]() }).addOne(relation, target)
    _predecessors.getOrElseUpdate(target, { mutable.HashSet[(R, N)]() }).addOne((relation, source))
  }

  def clear(): Unit = {
    _nodes.clear()
    _labels.clear()
    _relations.clear()
    _labelsByNode.clear()
    _successorRelations.clear()
    _predecessorRelations.clear()
    _successorsByRelation.clear()
    _predecessorsByRelation.clear()
    //    _successors.clear()
    _predecessors.clear()
  }

  def sizeCode(): String = {
    _nodes.size + "." + _labels.size + "." + _relations.size + "." +
      _labelsByNode.size + "." + _labelsByNode.values.map(_.size).sum + "." +
      _successorRelations.size + "." + _successorRelations.values.map(_.size).sum + "." +
      _predecessorRelations.size + "." + _predecessorRelations.values.map(_.size).sum + "." +
      _successorsByRelation.size + "." + _successorsByRelation.values.map(_.size).sum + "." +
      _predecessorsByRelation.size + "." + _predecessorsByRelation.values.map(_.size).sum + "." +
      _predecessors.size + "." + _predecessors.values.map(_.size).sum
  }

}
