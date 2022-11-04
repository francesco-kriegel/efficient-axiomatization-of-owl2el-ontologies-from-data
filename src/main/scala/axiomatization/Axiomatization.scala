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

import de.tu_dresden.inf.lat.axiomatization.Interpretation
import de.tu_dresden.inf.lat.axiomatization.PoweringClosureOperator
import de.tu_dresden.inf.lat.axiomatization.Util.{measureExecutionTime,formatTime,GLOBAL_COUNTER}

object Axiomatization {

  def run(ontologyFile: File, ont: String): Unit = {

    val startTime = System.currentTimeMillis()

    val graph = Interpretation.loadFromOntologyFile(ontologyFile)
    println(graph.nodes().size + " individuals")

    val reduction = Interpretation.reductionOf(graph)
    val n = reduction.nodes().size

    println("Computing closures...")
    val closures = FCbO.computeAllClosures(n, PoweringClosureOperator(reduction))
    println(closures.size + " closures")
    GLOBAL_COUNTER.reset()

    val cxt = InducedFormalContext.fromReduction(reduction, closures)
    val base = LinCbO.computeCanonicalBase(cxt, ont)
    println(base.size + " implications")

    val computationTime = System.currentTimeMillis() - startTime
    println(formatTime(computationTime))

  }

}
