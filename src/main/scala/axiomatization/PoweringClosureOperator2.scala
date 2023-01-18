package de.tu_dresden.inf.lat
package axiomatization

import org.phenoscape.scowl.*
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.model.*
import uk.ac.manchester.cs.owl.owlapi.OWLAnonymousIndividualImpl

import java.io.*
import java.text.SimpleDateFormat
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec
import scala.collection.immutable.BitSet
import scala.collection.mutable
import scala.collection.mutable.ArraySeq
import scala.collection.parallel.CollectionConverters.*
import scala.io.StdIn.readLine
import scala.jdk.CollectionConverters.*
import scala.jdk.StreamConverters.*
import scala.util.control.Breaks
import scala.util.control.Breaks.*

@Deprecated(forRemoval = true)
class PoweringClosureOperator2(val graph: BitGraph[OWLClass, OWLObjectProperty],
                               val knownSimulation: Option[BitBiMap] = None,
                               val maxConjunctionSize: Option[Int] = None,
                               val throwExceptionWhenSomeConjunctionIsTooLarge: Boolean = false,
                               val maxRoleDepth: Option[Int] = None,
                              ) extends Function[collection.BitSet, collection.BitSet] {

  given logger: Logger = NoLogger()
  val simulation = if knownSimulation.isDefined then knownSimulation.get else Interpretation.maximalSimulationOn(graph)

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

    if (xs equals graph.nodes()) {
      graph.nodes()
    } else {

      val powering = HashGraph[collection.BitSet, OWLClass, OWLObjectProperty]()

      @tailrec
      def extendPowering(current: IterableOnce[collection.BitSet], maxRoleDepth: Option[Int]): Option[IllegalArgumentException] = {
        var exception: Option[IllegalArgumentException] = None
        val next = mutable.HashSet[collection.BitSet]()
        val forLoop = new Breaks
        forLoop.breakable {
          for (xs <- current) {
            if (!powering.nodes().contains(xs)) {
//              if (!(xs subsetOf graph.nodes()))
//                throw new IllegalArgumentException()
              powering.nodes().addOne(xs)
              val labels = xs.unsorted.tail.map(graph.labels(_)).foldLeft(graph.labels(xs.head))(_ intersect _)
              powering.addLabels(xs, labels)
              if (maxConjunctionSize.isDefined && (labels.size > maxConjunctionSize.get))
                exception = Some(IllegalArgumentException("There are too many labels."))
                forLoop.break()
              if (maxRoleDepth.isEmpty || maxRoleDepth.get > 0) {
                val relations = xs.unsorted.tail.foldLeft(graph.successorRelations(xs.head))(_ intersect graph.successorRelations(_))
                relations.foreach(r => {
                  val hypergraph: collection.Set[collection.BitSet] = xs.unsorted.map(x => graph.successorsForRelation(x, r))
                  if (maxConjunctionSize.isDefined && isTooLarge(hypergraph, maxConjunctionSize.get - labels.size)) {
                    exception = Some(IllegalArgumentException("The hypergraph is too big: " + hypergraph.tail.foldLeft(hypergraph.head.size + "")(_ + " x " + _.size)))
                    forLoop.break()
                  } else {
                    HSdagBits.allMinimalHittingSets(hypergraph)
                      .map(mhs => mhs.filter(x => mhs.forall(y => !simulation(y, x))))
                      .foreach(ys => {
                        powering.addEdge(xs, r, ys)
                        next.addOne(ys)
                      })
                  }
                })
              }
            }
          }
        }
        if (exception.isDefined)
          exception
        else if (next.nonEmpty)
          extendPowering(next, maxRoleDepth.map(_ - 1))
        else
          None
      }

      val exception = extendPowering(Set(xs), maxRoleDepth)
      if (exception.isDefined) {
        if (throwExceptionWhenSomeConjunctionIsTooLarge)
          throw exception.get
        else
          graph.nodes()
      } else {

        val poweringSimulation = BitSetToIntRelationThatExtendsInverseElementhood()

        for (y <- graph.nodes()) {
          val yLabels = graph.labels(y)
          val yProperties = graph.successorRelations(y)
          for (ds <- powering.nodes()) {
            if (ds(y)) {
              // poweringSimulation.add(ds, y)
            } else if ((powering.labels(ds) subsetOf yLabels)
              && (powering.successorRelations(ds) subsetOf yProperties)) {
              poweringSimulation.add(ds, y)
            }
          }
        }

        //        println("Computing the mapping R(x,r)...")
        //        val powR = new collection.concurrent.TrieMap[(BitSet, OWLObjectProperty), mutable.BitSet] //with mutable.MultiMap[(BitSet, OWLObjectProperty), Int]
        // val powR = new mutable.HashMap[(collection.BitSet, OWLObjectProperty), mutable.BitSet]
        val powR = ConcurrentBitMap[(collection.BitSet, OWLObjectProperty)](graph.nodes().size - 1)

        // for (x <- powering.nodes()) {
        for (x <- powering.nodes().par) {
          for (yy <- graph.nodes()) {
            for (property <- graph.successorRelations(yy)) {
              if ((x intersect graph.successorsForRelation(yy, property)).isEmpty
                && (poweringSimulation.rawRow(x) intersect graph.successorsForRelation(yy, property)).isEmpty) {
                //powR.getOrElseUpdate((x, property), mutable.BitSet()).addOne(yy)
                powR.add((x, property), yy)
              }
            }
          }
        }

        //        println("Updating the simulation...")
        @tailrec
        def powLoop(): Unit = {
          // if (powR.keySet.nonEmpty) {
          if (powR.rows().nonEmpty) {
            // val (x, r) = powR.keySet.head
            val (x, r) = powR.rows().head
            for (xx <- powering.predecessorsForRelation(x, r)) {
              // for (yy <- powR(x, r)) {
              for (yy <- powR.rowImmutable((x, r))) {
                if (!xx(yy) && poweringSimulation(xx, yy)) {
                  poweringSimulation.remove(xx, yy)
                  for ((rr, yyy) <- graph.predecessors(yy)) {
                    if ((xx intersect graph.successorsForRelation(yyy, rr)).isEmpty
                      && (poweringSimulation.rawRow(xx) intersect graph.successorsForRelation(yyy, rr)).isEmpty) {
                      // powR.getOrElseUpdate((xx, rr), mutable.BitSet()).addOne(yyy)
                      powR.add((xx, rr), yyy)
                    }
                  }
                }
              }
            }
            // powR.remove((x, r))
            powR.clearRow((x, r))
            powLoop()
          }
        }

        powLoop()

        poweringSimulation.row(xs)

      }

    }

  }

}
