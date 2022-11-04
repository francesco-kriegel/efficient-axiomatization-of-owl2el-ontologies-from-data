package de.tu_dresden.inf.lat
package axiomatization

import org.phenoscape.scowl.*

import java.io.{BufferedWriter, File, FileWriter}
import org.semanticweb.owlapi.model.{OWLClass, OWLObjectProperty}

import scala.collection.immutable.BitSet
import scala.collection.mutable

trait FormalContext[G, M] {

  def objects(): collection.Iterable[G]
  def attributes(): collection.Iterable[M]
  def incidence(g: G, m: M): Boolean

  lazy val occupiedAttributes = attributes().filter(isOccupiedAttribute(_))
//  def isOccupiedObject(g: G): Boolean = true
  def isOccupiedAttribute(m: M): Boolean = true

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
    attributes().foreach(m => if (!isOccupiedAttribute(m)) writer.write(toString(m) + "\n"))
    writer.close()
  }

  def toString(m: M): String = m.toString

}

object InducedFormalContext {

  def fromReduction(reduction: BitGraph[OWLClass, OWLObjectProperty], closures: collection.Set[BitSet]): FormalContext[Int, OWLClass | (OWLObjectProperty, BitSet)] = {

    new FormalContext[Int, OWLClass | (OWLObjectProperty, BitSet)]() {

      override def objects(): mutable.BitSet =
        reduction.nodes()

      override def attributes(): List[OWLClass | (OWLObjectProperty, BitSet)] =
        List(OWLNothing) ++ reduction.labels() ++ reduction.relations().flatMap(r => closures.map(c => (r, c)))

      override def incidence(g: Int, m: OWLClass | (OWLObjectProperty, BitSet)): Boolean = {
        m match
          case OWLNothing => false
          case A @ Class(_) => reduction.labels(g).contains(A)
          //        case (r @ ObjectProperty(_), c: BitSet) => reduction.successorsForRelation(g, r).exists(c(_))
          case (r @ ObjectProperty(_), c: BitSet) => (reduction.successorsForRelation(g, r) intersect c).nonEmpty
        //        case _ => false
      }

      override def isOccupiedAttribute(m: OWLClass | (OWLObjectProperty, BitSet)): Boolean = {
        (m equals OWLNothing) || objects().exists(g => incidence(g, m))
      }

      override def toString(m: OWLClass | (OWLObjectProperty, BitSet)): String = {
        m match
          case OWLNothing => OWLNothing.toString
          case A@Class(_) => A.toString
          case (r@ObjectProperty(_), c: BitSet) => "ObjectSomeValuesFrom(" + r + "," + c.mkString("MMSC(", ",", ")") + ")"
      }

    }

  }

}
