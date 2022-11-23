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

class PoweringClosureOperator_Incremental(val reduction: BitGraph[OWLClass, OWLObjectProperty]) extends Function[collection.BitSet, collection.BitSet] {

  //    val reductionPar = new scala.collection.parallel.mutable.ParArray[OWLIndividual](reduction.nodes.toArray)
  //    val reductionNodesPar = new scala.collection.parallel.mutable.ParHashSet[Int]()
  //    reductionNodesPar.addAll(reduction.nodes)

  val powering = HashGraph[collection.BitSet, OWLClass, OWLObjectProperty]()

  private def extendPowering(xs: collection.BitSet): collection.Set[collection.BitSet] = {
    val delta = mutable.HashSet[collection.BitSet]()

    @tailrec
    def extend(current: IterableOnce[collection.BitSet]): Unit = {
      //        println("ext...")
      val next = mutable.HashSet[collection.BitSet]()
      for (xs <- current) {
        if (!powering.nodes().contains(xs)) {
          powering.nodes().addOne(xs)
          delta.add(xs)
          val labels = xs.unsorted.tail.map(reduction.labels(_)).foldLeft(reduction.labels(xs.head))(_ intersect _)
          //        powering.labels(xs).addAll(labels)
          powering.addLabels(xs, labels)
          val relations = reduction.successorRelations(xs.head)
          relations.foreach(r => {
//            val hypergraph: collection.Set[collection.Set[Int]] = xs.unsorted.map(x => BitSet.fromSpecific(reduction.successorsForRelation(x, r)))
            val hypergraph: collection.Set[collection.Set[Int]] = xs.unsorted.map(x => reduction.successorsForRelation(x, r))
            HSdag.allMinimalHittingSets(hypergraph).foreach(mhs => {
              val ys = BitSet.fromSpecific(mhs)
              powering.addEdge(xs, r, ys)
              //                extendPowering(ys)
              //                if (!powering.nodes.contains(ys))
              next.addOne(ys)
            })
          })
        }
      }
      if (next.nonEmpty)
        extend(next)
    }

    extend(Set(xs))
    //      println("Added " + delta.size + " elements to the powering")
    delta
  }

  //    val poweringSimulation = ConcurrentRelation[mutable.BitSet, Int]()
  val poweringSimulation = BitSetToIntRelation()

  //    def closure(_xs: BitSet): BitSet = {
  //      var __xs = mutable.BitSet()
  //      __xs.addAll(_xs)
  ////      println("Computing a closure...")
  ////      counter.tick()
  //      poweringSimulation.rows.foreach(ys => {
  //        if (ys subsetOf __xs)
  //          __xs.addAll(poweringSimulation.row(ys))
  //      })
  //      val xs = __xs.toImmutable
  def apply(xs: collection.BitSet): collection.BitSet = {

//    if (xs.max >= reduction.nodes().size)
//      throw new IllegalArgumentException()

    val delta = extendPowering(xs)
    if (delta.nonEmpty) {
      //        counter.tick()
      //        print("\r      #####")
      //        println("Computing the border...")
      val border = powering.nodes().filter(!delta(_)).filter(u => powering.predecessors(u).exists((_, v) => delta(v))).toSet
      for (y <- reduction.nodes()) {
        val yLabels = reduction.labels(y)
        val yProperties = reduction.successorRelations(y)
        for (ds <- delta) {
          if (ds(y)) {
            //              poweringSimulation.add(ds, y)
          } else if ((powering.labels(ds) subsetOf yLabels)
            && (powering.successorRelations(ds) subsetOf yProperties)) {
            poweringSimulation.add(ds, y)
          }
        }
      }

      //        println("Computing the mapping R(x,r)...")
      //        val powR = new collection.concurrent.TrieMap[(BitSet, OWLObjectProperty), mutable.BitSet] //with mutable.MultiMap[(BitSet, OWLObjectProperty), Int]
      val powR = new mutable.HashMap[(collection.BitSet, OWLObjectProperty), mutable.BitSet]

      for (x <- delta union border) {
        //          counter.tick()
        for (yy <- reduction.nodes()) {
          for (property <- reduction.successorRelations(yy)) {
            //              if (!reduction.successorsForRelation(yy, property).exists(poweringSimulation(x, _))) {
            if ((x intersect reduction.successorsForRelation(yy, property)).isEmpty
              && (poweringSimulation.rawRow(x) intersect reduction.successorsForRelation(yy, property)).isEmpty) {
              //                (x, property).synchronized {
              // powR.addBinding((x, property), yy)
              powR.getOrElseUpdate((x, property), mutable.BitSet()).addOne(yy)
              //                }
            }
          }
        }
      }

      //        println("Updating the simulation...")
      @tailrec
      def powLoop(): Unit = {
        //          counter.tick()
        //          val opt = powR.keys.find({ case (x, r) => powR.contains(x, r) && powR(x, r).nonEmpty })
        //          val opt = powR.keys.find({ case (x, r) => powR(x, r).nonEmpty })
        //          if (opt.isDefined) {
        //            val (x, r) = opt.get
        if (powR.keySet.nonEmpty) {
          val (x, r) = powR.keySet.head
          for (xx <- powering.predecessorsForRelation(x, r)) {
            for (yy <- powR(x, r)) {
              if (!xx(yy) && poweringSimulation(xx, yy)) {
                poweringSimulation.remove(xx, yy)
                for ((rr, yyy) <- reduction.predecessors(yy)) {
                  //                    if (!reduction.successorsForRelation(yyy, rr).exists(poweringSimulation(xx, _))) {
                  if ((xx intersect reduction.successorsForRelation(yyy, rr)).isEmpty
                    && (poweringSimulation.rawRow(xx) intersect reduction.successorsForRelation(yyy, rr)).isEmpty) {
                    //                      powR.addBinding((xx, rr), yyy)
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

    }
//    powering.clear()
    //      println("Closure computation done.")
    poweringSimulation.row(xs)
  }

}
