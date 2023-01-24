package de.tu_dresden.inf.lat
package axiomatization

import axiomatization.NestedParallelComputations.*

import org.phenoscape.scowl.*
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.*
import org.semanticweb.owlapi.model.parameters.Imports
import uk.ac.manchester.cs.owl.owlapi.OWLAnonymousIndividualImpl

import java.io.File
import scala.annotation.tailrec
import scala.collection.BitSet.fromSpecific
import scala.collection.mutable.ListBuffer
import scala.collection.{IterableOps, StrictOptimizedIterableOps, mutable}
import scala.jdk.CollectionConverters.*
import scala.jdk.StreamConverters.*


class BitGraph[L, R]() extends Graph[Int, L, R, mutable.BitSet](() => mutable.BitSet()) {

  def computeReduction()(using logger: Logger): (BitGraph[L, R], Int => mutable.BitSet, Int => Int, ConcurrentArrayBitBiMap) = {
    BitGraph.computeReduction(this)
  }

}

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
    val nodesThatHaveACommonPredecessorWith = java.util.concurrent.ConcurrentHashMap[collection.BitSet, collection.mutable.HashSet[collection.BitSet]]().asScala
    source.nodes().foreachPar(node => {
      val predecessors = collection.mutable.HashSet[collection.BitSet]()
      source.predecessors(node).foreach({ (_, pred) => predecessors.add(pred) })
      nodesThatHaveACommonPredecessorWith(node) =
        predecessors.flatMap(pred => source.successorRelations(pred).flatMap(rel => source.successorsForRelation(pred, rel)))
    })
    computeMaximalSimulation[collection.BitSet, L, R, collection.mutable.HashSet[collection.BitSet], HashGraph[collection.BitSet, L, R], ConcurrentBitSetToIntRelation](
      source, target, nodesThatHaveACommonPredecessorWith,
      () => ConcurrentBitSetToIntRelation(if target.nodes().isEmpty then 0 else target.nodes().max),
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
          R(x).rowMap.getOrElseUpdate(r, ConcurrentBoundedBitSet(target.nodes().toBitMask))
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

object BitGraph {

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

  def computeReduction[L, R](graph: BitGraph[L, R])(using logger: Logger):
  (BitGraph[L, R], Int => mutable.BitSet, Int => Int, ConcurrentArrayBitBiMap) = {

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
    val reduction = BitGraph[L, R]()
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
