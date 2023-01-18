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
import scala.util.control.Breaks
import scala.util.control.Breaks.*

@Deprecated(forRemoval = true)
class PoweringSimulator(val source: BitGraph[OWLClass, OWLObjectProperty],
                        val knownSimulation: Option[BitBiMap] = None,
                        val maxRoleDepth: Option[Int] = None,
                        val target: BitGraph[OWLClass, OWLObjectProperty])
  extends Function[collection.BitSet, collection.BitSet] {

  given logger: Logger = NoLogger()
  val simulation = if knownSimulation.isDefined then knownSimulation.get else Interpretation.maximalSimulationOn(source)

  def apply(xs: collection.BitSet): collection.BitSet = {

    val poweringSimulation = BitSetToIntRelation()
    val powering = HashGraph[collection.BitSet, OWLClass, OWLObjectProperty]()

    @tailrec
    def extendPowering(current: IterableOnce[collection.BitSet], maxRoleDepth: Option[Int]): Unit = {
      val next = mutable.HashSet[collection.BitSet]()
      val forLoop = new Breaks
      forLoop.breakable {
        for (xs <- current) {
          if (!powering.nodes().contains(xs)) {
//              if (!(xs subsetOf source.nodes()))
//                throw new IllegalArgumentException()
            powering.nodes().addOne(xs)
            val labels = xs.unsorted.tail.map(source.labels(_)).foldLeft(source.labels(xs.head))(_ intersect _)
            powering.addLabels(xs, labels)
            if (maxRoleDepth.isEmpty || maxRoleDepth.get > 0) {
              val relations = xs.unsorted.tail.foldLeft(source.successorRelations(xs.head))(_ intersect source.successorRelations(_))
              relations.foreach(r => {
                val hypergraph: collection.Set[collection.BitSet] = xs.unsorted.map(x => source.successorsForRelation(x, r))
                HSdagBits.allMinimalHittingSets(hypergraph)
                  .map(mhs => mhs.filter(x => mhs.forall(y => !simulation(y, x))))
                  .foreach(ys => {
                    powering.addEdge(xs, r, ys)
                    next.addOne(ys)
                  })
              })
            }
          }
        }
      }
      if (next.nonEmpty)
        extendPowering(next, maxRoleDepth.map(_ - 1))
    }

    extendPowering(Set(xs), maxRoleDepth)

    //      val poweringSimulation = BitSetToIntRelation()

    for (y <- target.nodes()) {
      val yLabels = target.labels(y)
      val yProperties = target.successorRelations(y)
      for (ds <- powering.nodes()) {
        if ((powering.labels(ds) subsetOf yLabels)
          && (powering.successorRelations(ds) subsetOf yProperties)) {
          poweringSimulation.add(ds, y)
        }
      }
    }

    //        println("Computing the mapping R(x,r)...")
    //        val powR = new collection.concurrent.TrieMap[(BitSet, OWLObjectProperty), mutable.BitSet] //with mutable.MultiMap[(BitSet, OWLObjectProperty), Int]
    val powR = new mutable.HashMap[(collection.BitSet, OWLObjectProperty), mutable.BitSet]
    // val powR = ConcurrentBitMap[(collection.BitSet, OWLObjectProperty)](target.nodes().size - 1)

    for (x <- powering.nodes()) {
    // for (x <- powering.nodes().par) {
      for (yy <- target.nodes()) {
        for (property <- target.successorRelations(yy)) {
          if ((poweringSimulation.row(x) intersect target.successorsForRelation(yy, property)).isEmpty) {
            // powR.add((x, property), yy)
            powR.getOrElseUpdate((x, property), mutable.BitSet()).addOne(yy)
          }
        }
      }
    }

    //        println("Updating the simulation...")
    @tailrec
    def powLoop(): Unit = {
      // if (powR.rows().nonEmpty) {
      if (powR.keySet.nonEmpty) {
        // val (x, r) = powR.rows().head
        val (x, r) = powR.keySet.head
        for (xx <- powering.predecessorsForRelation(x, r)) {
          // for (yy <- powR.row((x, r))) {
          for (yy <- powR(x, r)) {
            if (poweringSimulation(xx, yy)) {
              poweringSimulation.remove(xx, yy)
              for ((rr, yyy) <- target.predecessors(yy)) {
                if ((poweringSimulation.row(xx) intersect target.successorsForRelation(yyy, rr)).isEmpty) {
                  // powR.add((xx, rr), yyy)
                  powR.getOrElseUpdate((xx, rr), mutable.BitSet()).addOne(yyy)
                }
              }
            }
          }
        }
        powR.remove((x, r))
        // powR.clearRow((x, r))
        powLoop()
      }
    }

    powLoop()

    poweringSimulation.row(xs)

  }

}
