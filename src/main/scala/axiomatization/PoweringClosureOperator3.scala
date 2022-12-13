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

@Deprecated
class PoweringClosureOperator3(val reduction: BitGraph[OWLClass, OWLObjectProperty]) extends Function[collection.BitSet, collection.BitSet] {

  given logger: Logger = NoLogger()

  def apply(xs: collection.BitSet): collection.BitSet = {

    if (xs.isEmpty)
      reduction.nodes()
    else  {

      val it = xs.iterator
      var (powering, root) = (reduction, it.next())

      while (it.hasNext)
        val (product, rootInProduct) = Graph.product(powering, root, reduction, it.next())
        val (reducedProduct, _, representedBy, _) = Interpretation.reductionOf(product)
        powering = reducedProduct
        root = representedBy(rootInProduct)

      val poweringSimulation = BitMap[Int]()

      for (y <- reduction.nodes()) {
        val yLabels = reduction.labels(y)
        val yProperties = reduction.successorRelations(y)
        for (ds <- powering.nodes()) {
//          if (ds(y)) {
//            // poweringSimulation.add(ds, y)
//          } else
          if ((powering.labels(ds) subsetOf yLabels)
            && (powering.successorRelations(ds) subsetOf yProperties)) {
            poweringSimulation.add(ds, y)
          }
        }
      }

      //        println("Computing the mapping R(x,r)...")
      //        val powR = new collection.concurrent.TrieMap[(BitSet, OWLObjectProperty), mutable.BitSet] //with mutable.MultiMap[(BitSet, OWLObjectProperty), Int]
      val powR = new mutable.HashMap[(Int, OWLObjectProperty), mutable.BitSet]

      for (x <- powering.nodes()) {
        for (yy <- reduction.nodes()) {
          for (property <- reduction.successorRelations(yy)) {
//            if ((x intersect reduction.successorsForRelation(yy, property)).isEmpty
//              && (poweringSimulation.rawRow(x) intersect reduction.successorsForRelation(yy, property)).isEmpty) {
            if ((poweringSimulation.row(x) intersect reduction.successorsForRelation(yy, property)).isEmpty) {
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
//              if (!xx(yy) && poweringSimulation(xx, yy)) {
              if (poweringSimulation(xx, yy)) {
                poweringSimulation.remove(xx, yy)
                for ((rr, yyy) <- reduction.predecessors(yy)) {
//                  if ((xx intersect reduction.successorsForRelation(yyy, rr)).isEmpty
//                    && (poweringSimulation.rawRow(xx) intersect reduction.successorsForRelation(yyy, rr)).isEmpty) {
                  if ((poweringSimulation.row(xx) intersect reduction.successorsForRelation(yyy, rr)).isEmpty) {
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

      poweringSimulation.row(root)

    }

  }

}
