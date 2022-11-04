package de.tu_dresden.inf.lat
package axiomatization

import org.phenoscape.scowl.*

import java.io.{BufferedWriter, File, FileWriter}
import org.semanticweb.owlapi.model.{OWLClass, OWLObjectProperty}

import scala.collection.immutable.BitSet
import scala.collection.mutable

class InducedFormalContext(reduction: BitGraph[OWLClass, OWLObjectProperty], closures: collection.Set[BitSet]) {

  def objects(): mutable.BitSet =
    reduction.nodes()

  def attributes(): List[OWLClass | (OWLObjectProperty, BitSet)] =
    List(OWLNothing) ++ reduction.labels() ++ reduction.relations().flatMap(r => closures.map(c => (r, c)))

  def incidence(g: Int, m: OWLClass | (OWLObjectProperty, BitSet)): Boolean = {
    m match
      case OWLNothing => false
      case A @ Class(_) => reduction.labels(g).contains(A)
      //        case (r @ ObjectProperty(_), c: BitSet) => reduction.successorsForRelation(g, r).exists(c(_))
      case (r @ ObjectProperty(_), c: BitSet) => (reduction.successorsForRelation(g, r) intersect c).nonEmpty
    //        case _ => false
  }

  def isOccupiedAttribute(m: OWLClass | (OWLObjectProperty, BitSet)): Boolean = {
    (m equals OWLNothing) || objects().exists(g => incidence(g, m))
  }

  lazy val occupiedAttributes = attributes().filter(isOccupiedAttribute(_))

  def toString(m: OWLClass | (OWLObjectProperty, BitSet)): String = {
    m match
      case OWLNothing => OWLNothing.toString
      case A@Class(_) => A.toString
      case (r@ObjectProperty(_), c: BitSet) => "ObjectSomeValuesFrom(" + r + "," + c.mkString("MMSC(", ",", ")") + ")"
  }

  def writeToFile(cxtFile: File): Unit = {
    val writer = new BufferedWriter(new FileWriter(cxtFile))
    writer.write("B\n")
    writer.write("\n")
    writer.write(objects().size + "\n")
    writer.write(occupiedAttributes.size + "\n")
    writer.write("\n")
    objects().foreach(g => writer.write(g.toString + "\n"))
    occupiedAttributes.foreach(m => writer.write(toString(m) + "\n"))
    objects().foreach(g => {
      occupiedAttributes.foreach(m => {
        if (incidence(g, m)) writer.write("X")
        else writer.write(".")
      })
      writer.write("\n")
    })
    writer.write("\n\n\n")
    attributes().foreach(m => if (!occupiedAttributes.contains(m)) writer.write(toString(m) + "\n"))
    writer.close()
  }

}
