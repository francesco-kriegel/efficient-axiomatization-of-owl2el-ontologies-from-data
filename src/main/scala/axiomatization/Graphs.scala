package de.tu_dresden.inf.lat
package axiomatization

import org.phenoscape.scowl.*
import org.semanticweb.owlapi.model.{OWLClass, OWLIndividual, OWLObjectProperty, OWLOntology}

import scala.collection.BitSet.fromSpecific
import scala.collection.{IterableOps, StrictOptimizedIterableOps, mutable}
import scala.collection.mutable.ListBuffer
import scala.jdk.StreamConverters.*

object Graph {

  def product(graph1: BitGraph[OWLClass, OWLObjectProperty], x1: Int,
              graph2: BitGraph[OWLClass, OWLObjectProperty], x2: Int): (BitGraph[OWLClass, OWLObjectProperty], Int) = {
    val productGraph = BitGraph[OWLClass, OWLObjectProperty]()
    val root = (x1, x2)
    productGraph.labels().addAll(graph1.labels())
    productGraph.labels().addAll(graph2.labels())
    productGraph.relations().addAll(graph1.relations())
    productGraph.relations().addAll(graph2.relations())
    var i = 0
    val index = mutable.ArrayBuffer[(Int, Int)]()
    val table = mutable.HashMap[(Int, Int), Int]()

    def addNodeToProduct(pair: (Int, Int)): Boolean = {
      !table.contains(pair) && {
        index.addOne(pair)
        table(pair) = i
        productGraph.nodes().addOne(i)
        i += 1
        true
      }
    }

    def extendProduct(pairs: Iterable[(Int, Int)], edges: Iterable[((Int, Int), OWLObjectProperty, (Int, Int))]): Unit = {
      val nextPairs = mutable.HashSet[(Int, Int)]()
      val nextEdges = mutable.HashSet[((Int, Int), OWLObjectProperty, (Int, Int))]()
      pairs.foreach(pair => {
        if (addNodeToProduct(pair)) {
          productGraph.addLabels(table(pair), graph1.labels(pair._1) intersect graph2.labels(pair._2))
          (graph1.successorRelations(pair._1) intersect graph2.successorRelations(pair._2)).foreach(r => {
            graph1.successorsForRelation(pair._1, r).foreach(y1 => {
              graph2.successorsForRelation(pair._2, r).foreach(y2 => {
                val nextPair = (y1, y2)
                nextPairs.addOne(nextPair)
                nextEdges.addOne((pair, r, nextPair))
              })
            })
          })
        }
      })
      edges.foreach {
        case (source, r, target) => productGraph.addEdge(table(source), r, table(target))
      }
      if (nextPairs.nonEmpty || nextEdges.nonEmpty)
        extendProduct(nextPairs, nextEdges)
    }

    extendProduct(Iterable.single(root), Iterable.empty)
    (productGraph, table(root))
  }

}

//@Deprecated(forRemoval = true)
//trait Graph[N, L, R] {
//
//  def nodes(): collection.Set[N]
//  def labels(): collection.Seq[L]
//  def relations(): collection.Seq[R]
//
//  def labels(node: N): collection.Set[L]
//
////  def successors(node: N): collection.Set[(R, N)]
//  def predecessors(node: N): collection.Set[(R, N)]
//
//  def successorRelations(node: N): collection.Set[R]
////  def predecessorRelations(node: N): collection.Set[R]
//
//  def successorsForRelation(node: N, relation: R): collection.Set[N]
//  def predecessorsForRelation(node: N, relation: R): collection.Set[N]
//
//}

//@Deprecated(forRemoval = true)
//class HashGraph[N, L, R](val initNodes: N*) extends Graph[N, L, R] {
//
//  val _nodes = mutable.HashSet[N]()
//  val _labels = mutable.ArrayBuffer[L]()
//  val _relations = mutable.ArrayBuffer[R]()
//
//  val _labelsByNode = new mutable.HashMap[N, mutable.HashSet[L]]
//
//  val _successorRelations = new mutable.HashMap[N, mutable.HashSet[R]]
////  val _predecessorRelations = new mutable.HashMap[N, mutable.HashSet[R]]
//
//  val _successorsByRelation = new mutable.HashMap[(N, R), mutable.HashSet[N]]
//  val _predecessorsByRelation = new mutable.HashMap[(N, R), mutable.HashSet[N]]
//
////  val _successors = new mutable.HashMap[N, mutable.HashSet[(R, N)]]
//  val _predecessors = new mutable.HashMap[N, mutable.HashSet[(R, N)]]
//
//  def nodes(): mutable.HashSet[N] =
//    _nodes
//  def labels(): mutable.ArrayBuffer[L] =
//    _labels
//  def relations(): mutable.ArrayBuffer[R] =
//    _relations
//
//  def labels(node: N): mutable.HashSet[L] =
//    _labelsByNode.getOrElse(node, mutable.HashSet.empty)
//
//  def successorRelations(node: N): mutable.HashSet[R] =
//    _successorRelations.getOrElse(node, mutable.HashSet.empty)
////  def predecessorRelations(node: N): mutable.HashSet[R] =
////    _predecessorRelations.getOrElse(node, mutable.HashSet.empty)
//
//  def successorsForRelation(node: N, relation: R): mutable.HashSet[N] =
//    _successorsByRelation.getOrElse((node, relation), mutable.HashSet.empty)
//  def predecessorsForRelation(node: N, relation: R): mutable.HashSet[N] =
//    _predecessorsByRelation.getOrElse((node, relation), mutable.HashSet.empty)
//
////  def successors(node: N): mutable.HashSet[(R, N)] =
////    _successors.getOrElse(node, mutable.HashSet.empty)
//  def predecessors(node: N): mutable.HashSet[(R, N)] =
//    _predecessors.getOrElse(node, mutable.HashSet.empty)
//
//  _nodes.addAll(initNodes)
//
//  def addNode(node: N): Unit = {
//    _nodes.addOne(node)
//  }
//
//  def addLabel(node: N, label: L): Unit = {
//    _labelsByNode.getOrElseUpdate(node, { mutable.HashSet[L]() }).addOne(label)
//  }
//
//  def addLabels(node: N, labels: IterableOnce[L]): Unit = {
//    _labelsByNode.getOrElseUpdate(node, { mutable.HashSet[L]() }).addAll(labels)
//  }
//
//  def addEdge(source: N, relation: R, target: N): Unit = {
//    _successorRelations.getOrElseUpdate(source, { mutable.HashSet[R]() }).addOne(relation)
////    _predecessorRelations.getOrElseUpdate(target, { mutable.HashSet[R]() }).addOne(relation)
//    _successorsByRelation.getOrElseUpdate((source, relation), { mutable.HashSet[N]() }).addOne(target)
//    _predecessorsByRelation.getOrElseUpdate((target, relation), { mutable.HashSet[N]() }).addOne(source)
////    _successors.getOrElseUpdate(source, { mutable.HashSet[(R, N)]() }).addOne(relation, target)
//    _predecessors.getOrElseUpdate(target, { mutable.HashSet[(R, N)]() }).addOne(relation, source)
//  }
//
//  def clear(): Unit = {
//    _nodes.clear()
//    _labels.clear()
//    _relations.clear()
//    _labelsByNode.clear()
//    _successorRelations.clear()
////    _predecessorRelations.clear()
//    _successorsByRelation.clear()
//    _predecessorsByRelation.clear()
////    _successors.clear()
//    _predecessors.clear()
//  }
//
//}

//object HashGraph {
//
//  def fromOntology(ontology: OWLOntology): HashGraph[OWLIndividual, OWLClass, OWLObjectProperty] = {
//    val graph = HashGraph[OWLIndividual, OWLClass, OWLObjectProperty]()
//    (ontology.individualsInSignature().toScala(LazyList) concat ontology.anonymousIndividuals().toScala(LazyList))
//      .foreach(graph.addNode(_))
//    ontology.classesInSignature().toScala(LazyList).filterNot(_ equals OWLThing).filterNot(_ equals OWLNothing)
//      .foreach(graph.labels().addOne(_))
//    ontology.objectPropertiesInSignature().toScala(LazyList)
//      .foreach(graph.relations().addOne(_))
//    ontology.axioms().toScala(LazyList)
//      .foreach({
//        case ClassAssertion(_, c @ Class(_), x) if !(c equals OWLNothing) && !(c equals OWLThing) =>
//          graph.addLabel(x, c)
//        case ObjectPropertyAssertion(_, property @ ObjectProperty(_), source, target) =>
//          graph.addEdge(source, property, target)
//        case ax =>
////          println("Ignored axiom: " + ax)
//      })
//    graph
//  }
//
//}

//@Deprecated(forRemoval = true)
//class BitGraph[L, R](val initNodes: Int*) extends Graph[Int, L, R] {
//
//  val _nodes = mutable.BitSet()
//  val _labels = mutable.ArrayBuffer[L]()
//  val _relations = mutable.ArrayBuffer[R]()
//
//  val _labelsByNode = new mutable.HashMap[Int, mutable.HashSet[L]]
//
//  val _successorRelations = new mutable.HashMap[Int, mutable.HashSet[R]]
//  //  val _predecessorRelations = new mutable.HashMap[Int, mutable.HashSet[R]]
//
//  val _successorsByRelation = new mutable.HashMap[(Int, R), mutable.BitSet]
//  val _predecessorsByRelation = new mutable.HashMap[(Int, R), mutable.BitSet]
//
//  //  val _successors = new mutable.HashMap[Int, mutable.HashSet[(R, Int)]]
//  val _predecessors = new mutable.HashMap[Int, mutable.HashSet[(R, Int)]]
//
//  def nodes(): mutable.BitSet =
//    _nodes
//
//  def labels(): mutable.ArrayBuffer[L] =
//    _labels
//
//  def relations(): mutable.ArrayBuffer[R] =
//    _relations
//
//  def labels(node: Int): mutable.HashSet[L] =
//    _labelsByNode.getOrElse(node, mutable.HashSet.empty)
//
//  def successorRelations(node: Int): mutable.HashSet[R] =
//    _successorRelations.getOrElse(node, mutable.HashSet.empty)
//  //  def predecessorRelations(node: Int): mutable.HashSet[R] =
//  //    _predecessorRelations.getOrElse(node, mutable.HashSet.empty)
//
//  def successorsForRelation(node: Int, relation: R): mutable.BitSet =
//    _successorsByRelation.getOrElse((node, relation), mutable.BitSet.empty)
//  def predecessorsForRelation(node: Int, relation: R): mutable.BitSet =
//    _predecessorsByRelation.getOrElse((node, relation), mutable.BitSet.empty)
//
//  //  def successors(node: Int): mutable.HashSet[(R, Int)] =
//  //    _successors.getOrElse(node, mutable.HashSet.empty)
//  def predecessors(node: Int): mutable.HashSet[(R, Int)] =
//    _predecessors.getOrElse(node, mutable.HashSet.empty)
//
//  _nodes.addAll(initNodes)
//
//  def addNode(node: Int): Unit = {
//    _nodes.addOne(node)
//  }
//
//  def addLabel(node: Int, label: L): Unit = {
//    _labelsByNode.getOrElseUpdate(node, { mutable.HashSet[L]() }).addOne(label)
//  }
//
//  def addLabels(node: Int, labels: IterableOnce[L]): Unit = {
//    _labelsByNode.getOrElseUpdate(node, { mutable.HashSet[L]() }).addAll(labels)
//  }
//
//  def addEdge(source: Int, relation: R, target: Int): Unit = {
//    _successorRelations.getOrElseUpdate(source, { mutable.HashSet[R]() }).addOne(relation)
//    //    _predecessorRelations.getOrElseUpdate(target, { mutable.HashSet[R]() }).addOne(relation)
//    _successorsByRelation.getOrElseUpdate((source, relation), { mutable.BitSet() }).addOne(target)
//    _predecessorsByRelation.getOrElseUpdate((target, relation), { mutable.BitSet() }).addOne(source)
//    //    _successors.getOrElseUpdate(source, { mutable.HashSet[(R, Int)]() }).addOne(relation, target)
//    _predecessors.getOrElseUpdate(target, { mutable.HashSet[(R, Int)]() }).addOne((relation, source))
//  }
//
//  def clear(): Unit = {
//    _nodes.clear()
//    _labels.clear()
//    _relations.clear()
//    _labelsByNode.clear()
//    _successorRelations.clear()
//    //    _predecessorRelations.clear()
//    _successorsByRelation.clear()
//    _predecessorsByRelation.clear()
//    //    _successors.clear()
//    _predecessors.clear()
//  }
//
//  def sizeCode(): String = {
//    _nodes.size + "." + _labels.size + "." + _relations.size + "." +
//      _labelsByNode.size + "." + _labelsByNode.values.map(_.size).sum + "." +
//      _successorRelations.size + "." + _successorRelations.values.map(_.size).sum + "." +
//      _successorsByRelation.size + "." + _successorsByRelation.values.map(_.size).sum + "." +
//      _predecessorsByRelation.size + "." + _predecessorsByRelation.values.map(_.size).sum + "." +
//      _predecessors.size + "." + _predecessors.values.map(_.size).sum
//  }
//
//}

class BitGraph[L, R]() extends Graph[Int, L, R, mutable.BitSet](() => mutable.BitSet()) {}

class HashGraph[N, L, R]() extends Graph[N, L, R, mutable.HashSet[N]](() => mutable.HashSet[N]()) {}

abstract class Graph[N, L, R, SetN <: mutable.Set[N]](newSetN: () => SetN) {

  val _nodes = newSetN()
  val _labels = mutable.HashSet[L]()
  val _relations = mutable.HashSet[R]()

  val _labelsByNode = new mutable.HashMap[N, mutable.HashSet[L]]

  val _successorRelations = new mutable.HashMap[N, mutable.HashSet[R]]
  //  val _predecessorRelations = new mutable.HashMap[N, mutable.HashSet[R]]

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

  def successorRelations(node: N): mutable.HashSet[R] =
    _successorRelations.getOrElse(node, mutable.HashSet.empty)
  //  def predecessorRelations(node: N): mutable.HashSet[R] =
  //    _predecessorRelations.getOrElse(node, mutable.HashSet.empty)

  def successorsForRelation(node: N, relation: R): SetN =
    _successorsByRelation.getOrElse((node, relation), newSetN())
  def predecessorsForRelation(node: N, relation: R): SetN =
    _predecessorsByRelation.getOrElse((node, relation), newSetN())

  //  def successors(node: N): mutable.HashSet[(R, N)] =
  //    _successors.getOrElse(node, mutable.HashSet.empty)
  def predecessors(node: N): mutable.HashSet[(R, N)] =
    _predecessors.getOrElse(node, mutable.HashSet.empty)

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
    //    _predecessorRelations.getOrElseUpdate(target, { mutable.HashSet[R]() }).addOne(relation)
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
    //    _predecessorRelations.clear()
    _successorsByRelation.clear()
    _predecessorsByRelation.clear()
    //    _successors.clear()
    _predecessors.clear()
  }

  def sizeCode(): String = {
    _nodes.size + "." + _labels.size + "." + _relations.size + "." +
      _labelsByNode.size + "." + _labelsByNode.values.map(_.size).sum + "." +
      _successorRelations.size + "." + _successorRelations.values.map(_.size).sum + "." +
      _successorsByRelation.size + "." + _successorsByRelation.values.map(_.size).sum + "." +
      _predecessorsByRelation.size + "." + _predecessorsByRelation.values.map(_.size).sum + "." +
      _predecessors.size + "." + _predecessors.values.map(_.size).sum
  }

}
