package de.tu_dresden.inf.lat
package axiomatization

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
import scala.collection.parallel.CollectionConverters.*
import scala.io.StdIn.readLine
import scala.jdk.CollectionConverters.*
import scala.jdk.StreamConverters.*
import scala.util.control.Breaks.*

@Deprecated(forRemoval = true)
class PoweringClosureOperator2_CachedValues(val reduction: BitGraph[OWLClass, OWLObjectProperty]) extends Function[collection.BitSet, collection.BitSet] {

  given logger: Logger = NoLogger()
  val simulation = Interpretation.maximalSimulationOn(reduction)
  val poweringSimulation = BitSetToIntRelationThatExtendsInverseElementhood()

  def apply(xs: collection.BitSet): collection.BitSet = {

//    if (xs.max >= reduction.nodes().size)
//      throw new IllegalArgumentException()
    if (poweringSimulation.rows.contains(xs))
      poweringSimulation.row(xs)
    else {

      val powering = HashGraph[collection.BitSet, OWLClass, OWLObjectProperty]()

      @tailrec
      def extendPowering(current: IterableOnce[collection.BitSet]): Unit = {
        val next = mutable.HashSet[collection.BitSet]()
        for (xs <- current) {
          if (!powering.nodes().contains(xs)) {
            if (!(xs subsetOf reduction.nodes()))
              throw new IllegalArgumentException()
            powering.nodes().addOne(xs)
            val labels = xs.unsorted.tail.map(reduction.labels(_)).foldLeft(reduction.labels(xs.head))(_ intersect _)
            powering.addLabels(xs, labels)
  //          val relations = reduction.successorRelations(xs.head)
            val relations = xs.unsorted.tail.foldLeft(reduction.successorRelations(xs.head))(_ intersect reduction.successorRelations(_))
            relations.foreach(r => {
  //            val hypergraph: collection.Set[collection.Set[Int]] = xs.unsorted.map(x => BitSet.fromSpecific(reduction.successorsForRelation(x, r)))
              val hypergraph: collection.Set[collection.BitSet] = xs.unsorted.map(x => reduction.successorsForRelation(x, r))
              var hypergraphSize = 1l
              val it = hypergraph.iterator
              while (it.hasNext)
                hypergraphSize *= it.next().size
                if (hypergraphSize > 1048576)
                  throw IllegalArgumentException("The hypergraph is too big: " + hypergraph.tail.foldLeft(hypergraph.head.size + "")(_ + " x " + _.size))
              HSdagBits.allMinimalHittingSets(hypergraph)
                .map(mhs => mhs.filter(x => mhs.forall(y => !simulation(y, x))))
                .foreach(ys => {
                  powering.addEdge(xs, r, ys)
                  next.addOne(ys)
                })
            })
          }
        }
        if (next.nonEmpty)
          extendPowering(next)
      }

      extendPowering(Set(xs))
  //    Util.writeExecutionTime({ extendPowering(Set(xs)) }, duration => println("Unfolding the powering took " + Util.formatTime(duration)))

      for (y <- reduction.nodes()) {
        val yLabels = reduction.labels(y)
        val yProperties = reduction.successorRelations(y)
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
      val powR = new mutable.HashMap[(collection.BitSet, OWLObjectProperty), mutable.BitSet]

      for (x <- powering.nodes()) {
        for (yy <- reduction.nodes()) {
          for (property <- reduction.successorRelations(yy)) {
            if ((x intersect reduction.successorsForRelation(yy, property)).isEmpty
              && (poweringSimulation.rawRow(x) intersect reduction.successorsForRelation(yy, property)).isEmpty) {
              powR.getOrElseUpdate((x, property), mutable.BitSet()).addOne(yy)
            }
          }
        }
      }

      //        println("Updating the simulation...")
      @tailrec
      def powLoop(): Unit = {
        if (powR.keySet.nonEmpty) {
          val (x, r) = powR.keySet.head
          for (xx <- powering.predecessorsForRelation(x, r)) {
            for (yy <- powR(x, r)) {
              if (!xx(yy) && poweringSimulation(xx, yy)) {
                poweringSimulation.remove(xx, yy)
                for ((rr, yyy) <- reduction.predecessors(yy)) {
                  if ((xx intersect reduction.successorsForRelation(yyy, rr)).isEmpty
                    && (poweringSimulation.rawRow(xx) intersect reduction.successorsForRelation(yyy, rr)).isEmpty) {
                    powR.getOrElseUpdate((xx, rr), mutable.BitSet()).addOne(yyy)
                  }
                }
              }
            }
          }
          powR.remove((x, r))
          powLoop()
        }
      }

      powLoop()

      poweringSimulation.row(xs)

    }

  }

}
