package de.tu_dresden.inf.lat
package axiomatization

import de.tu_dresden.inf.lat.axiomatization.Axiomatization.MAX_POWERING_SIZE
import org.phenoscape.scowl.*
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.*
import org.semanticweb.owlapi.model.parameters.Imports
import uk.ac.manchester.cs.owl.owlapi.OWLAnonymousIndividualImpl

import java.io.*
import java.text.SimpleDateFormat
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec
import scala.collection.immutable.BitSet
import scala.collection.mutable
import scala.collection.mutable.ArraySeq
import scala.io.StdIn.readLine
import scala.jdk.CollectionConverters.*
import scala.jdk.StreamConverters.*
import scala.util.control.Breaks
import scala.util.control.Breaks.*


class PoweringClosureOperator(graph: BitGraph[OWLClass, OWLObjectProperty],
                              knownSimulation: Option[ConcurrentArrayBitBiMap] = None,
                              maxConjunctionSize: Option[Int] = None,
                              throwExceptionWhenSomeConjunctionIsTooLarge: Boolean = false,
                              maxRoleDepth: Option[Int] = None)
  extends PoweringSimulator(graph, graph, knownSimulation, maxConjunctionSize, throwExceptionWhenSomeConjunctionIsTooLarge, maxRoleDepth) {

  override def apply(xs: collection.BitSet): collection.BitSet = {
    if xs equals graph.nodes() then graph.nodes() else super.apply(xs)
  }

}

class PoweringTooLargeException(msg: String) extends java.lang.Exception(msg)

class PoweringSimulator(val source: BitGraph[OWLClass, OWLObjectProperty],
                        val target: BitGraph[OWLClass, OWLObjectProperty],
                        val knownSimulation: Option[ConcurrentArrayBitBiMap] = None,
                        val maxConjunctionSize: Option[Int] = None,
                        val throwExceptionWhenSomeConjunctionIsTooLarge: Boolean = false,
                        val maxRoleDepth: Option[Int] = None)
  extends (collection.BitSet => collection.BitSet) {

  given logger: Logger = NoLogger()

  val simulation = if knownSimulation.isDefined then knownSimulation.get else GraphSimulator.computeMaximalSimulation(source, source)

  private def isTooLarge(hypergraph: collection.Set[collection.BitSet], limit: Int): Boolean = {
    var tooLarge = false
    var hypergraphSize = 1L
    val it = hypergraph.iterator
    val whileLoop = new Breaks
    whileLoop.breakable {
      while (it.hasNext)
        hypergraphSize *= it.next().size
        if (hypergraphSize > limit)
          tooLarge = true
          whileLoop.break()
    }
    tooLarge
  }

  def apply(xs: collection.BitSet): collection.BitSet = {

    val powering = HashGraph[collection.BitSet, OWLClass, OWLObjectProperty]()

    @tailrec
    def extendPowering(current: collection.Set[collection.BitSet], maxRoleDepth: Option[Int]): Unit = {
      if powering.nodes().size + current.size > MAX_POWERING_SIZE
      then throw PoweringTooLargeException("The powering has more than " + MAX_POWERING_SIZE + " objects.")
      val next = mutable.HashSet[collection.BitSet]()
      for (xs <- current) {
        if (!powering.nodes().contains(xs)) {
          powering.nodes().addOne(xs)
          val labels = xs.unsorted.tail.map(source.labels(_)).foldLeft(source.labels(xs.head))(_ intersect _)
          powering.addLabels(xs, labels)
          if (maxConjunctionSize.isDefined && (labels.size > maxConjunctionSize.get))
            throw PoweringTooLargeException("An object in the powering has more than " + maxConjunctionSize.get + " labels.")
          if (maxRoleDepth.isEmpty || maxRoleDepth.get > 0) {
            val relations = xs.unsorted.tail.foldLeft(source.successorRelations(xs.head))(_ intersect source.successorRelations(_))
            relations.foreach(r => {
              val hypergraph: collection.Set[collection.BitSet] = xs.unsorted.map(x => source.successorsForRelation(x, r))
              if (maxConjunctionSize.isDefined && isTooLarge(hypergraph, maxConjunctionSize.get - labels.size)) {
                throw PoweringTooLargeException("An object in the powering has more than " + maxConjunctionSize.get + " labels or successors.  Hypergraph: " + hypergraph.tail.foldLeft(hypergraph.head.size + "")(_ + " x " + _.size))
              } else {
                //HSdagBits.allMinimalHittingSets(hypergraph)
                HSdagBitsPar.allMinimalHittingSets(source.nodes().size - 1, hypergraph)
                  // We only keep the simulation-minimal elements, i.e., which are not simulated by another element.
                  // We assume the the source is reduced, otherwise we would need to modify the filter condition.
                  .map(mhs => mhs.filter(x => mhs.forall(y => (x equals y) || !simulation(y, x))))
                  .foreach(ys => {
                    powering.addEdge(xs, r, ys)
                    next.addOne(ys)
                  })
              }
            })
          }
        }
      }
      if (next.nonEmpty)
        extendPowering(next, maxRoleDepth.map(_ - 1))
    }

    try {
      extendPowering(Set(xs), maxRoleDepth)
      val poweringSimulation = GraphSimulator.computeMaximalSimulation(powering, target)
      poweringSimulation.row(xs).viewAsImmutableBitSet
    } catch case e: PoweringTooLargeException => {
      if throwExceptionWhenSomeConjunctionIsTooLarge then throw e
      else if source equals target then source.nodes() else collection.BitSet.empty
    }

  }

}
