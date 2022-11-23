package de.tu_dresden.inf.lat
package axiomatization

import org.phenoscape.scowl.*
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.model.{NodeID, OWLAnonymousIndividual, OWLClass, OWLClassExpression, OWLIndividual, OWLObjectProperty}
import uk.ac.manchester.cs.owl.owlapi.OWLAnonymousIndividualImpl

import java.io.{BufferedReader, BufferedWriter, File, FileReader, FileWriter}
import java.text.SimpleDateFormat
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec
import scala.collection.immutable.BitSet
import scala.collection.mutable
import scala.io.StdIn.readLine
import scala.jdk.StreamConverters.*
import util.control.Breaks.*
import collection.parallel.CollectionConverters.*
import scala.collection.mutable.ArraySeq
import scala.jdk.CollectionConverters.*

class PoweringClosureOperator(val reduction: BitGraph[OWLClass, OWLObjectProperty]) extends Function[collection.BitSet, collection.BitSet] {

  def apply(xs: collection.BitSet): collection.BitSet = {

//    if (xs.max >= reduction.nodes().size)
//      throw new IllegalArgumentException()

    val powering = HashGraph[collection.BitSet, OWLClass, OWLObjectProperty]()

    @tailrec
    def extendPowering(current: IterableOnce[collection.BitSet]): Unit = {
      val next = mutable.HashSet[collection.BitSet]()
      for (xs <- current) {
        if (!powering.nodes().contains(xs)) {
          powering.nodes().addOne(xs)
          val labels = xs.unsorted.tail.map(reduction.labels(_)).foldLeft(reduction.labels(xs.head))(_ intersect _)
          powering.addLabels(xs, labels)
          val relations = reduction.successorRelations(xs.head)
          relations.foreach(r => {
//            val hypergraph: collection.Set[collection.Set[Int]] = xs.unsorted.map(x => BitSet.fromSpecific(reduction.successorsForRelation(x, r)))
            val hypergraph: collection.Set[collection.Set[Int]] = xs.unsorted.map(x => reduction.successorsForRelation(x, r))
            HSdag.allMinimalHittingSets(hypergraph).foreach(mhs => {
              val ys = BitSet.fromSpecific(mhs)
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

    val poweringSimulation = BitSetToIntRelation()

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
