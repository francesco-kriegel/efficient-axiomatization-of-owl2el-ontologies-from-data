package de.tu_dresden.inf.lat
package axiomatization

import org.phenoscape.scowl.*

import java.io.{BufferedWriter, File, FileWriter}
import org.semanticweb.owlapi.model.{OWLClass, OWLObjectProperty}

import scala.collection.immutable.BitSet
import scala.collection.mutable

import Util.intersectionOfBitSets

class InducedFormalContext(reduction: BitGraph[OWLClass, OWLObjectProperty], isIndividual: Int => Boolean, closures: collection.Set[collection.BitSet]) {

  type M = OWLClass | (OWLObjectProperty, collection.BitSet)

  lazy val objects: mutable.BitSet =
    reduction.nodes().filter(isIndividual)

  lazy val attributes: Array[M] =
    Array[M](List(OWLNothing) ++ reduction.labels() ++ reduction.relations().flatMap(r => closures.map(c => (r, c))) : _*)

  lazy val attributeIndex: collection.Map[M, Int] = {
    val index = mutable.HashMap[M, Int]()
    (0 until attributes.length).foreach(i => index.update(attributes(i), i))
    index
  }

//  def fromBits(bs: collection.BitSet): collection.Set[M] = {
//    bs.unsorted.map(attributes)
//  }
//
//  def toBits(ms: collection.Set[M]): collection.BitSet = {
//    mutable.BitSet.fromSpecific(ms.iterator.map(attributeIndex))
//  }

  lazy val incidenceMatrix: BitBiMap = {
    val matrix = BitBiMap()
    objects.foreach(g => {
      (0 until attributes.length).foreach(m => if (incidence(g, attributes(m))) matrix.add(g, m))
    })
    matrix
  }

  private def incidence(g: Int, m: M): Boolean = {
    m match
      case OWLNothing => false
      case A @ Class(_) => reduction.labels(g).contains(A)
      //        case (r @ ObjectProperty(_), c: BitSet) => reduction.successorsForRelation(g, r).exists(c(_))
      case (r @ ObjectProperty(_), c: BitSet) => (reduction.successorsForRelation(g, r) intersect c).nonEmpty
    //        case _ => false
  }

  def isOccupiedAttribute(m: M): Boolean = {
    (m equals OWLNothing) || objects.exists(g => incidence(g, m))
  }

  lazy val occupiedAttributes = attributes.filter(isOccupiedAttribute(_))

  lazy val occupiedAttributeIndex: collection.Map[M, Int] = {
    val index = mutable.HashMap[M, Int]()
    (0 until occupiedAttributes.length).foreach(i => index.update(occupiedAttributes(i), i))
    index
  }

  def fromBits(bs: collection.BitSet): collection.Set[M] = {
    bs.unsorted.map(occupiedAttributes)
  }

  def toBits(ms: collection.Set[M]): collection.BitSet = {
    mutable.BitSet.fromSpecific(ms.iterator.map(occupiedAttributeIndex))
  }

  lazy val occupiedIncidenceMatrix: BitBiMap = {
    val matrix = BitBiMap()
    objects.foreach(g => {
      (0 until occupiedAttributes.length).foreach(m => if (incidence(g, occupiedAttributes(m))) matrix.add(g, m))
    })
    matrix
  }

  lazy val bitsOccupiedAttributes = BitSet.fromSpecific(0 until occupiedAttributes.length)

  def closure(bs: collection.BitSet): mutable.BitSet = {
//    val gs = intersectionOfBitSets(bs.iterator.map(b => occupiedIncidenceMatrix.col(b)), bs.size, objects)
//    intersectionOfBitSets(gs.iterator.map(g => occupiedIncidenceMatrix.row(g)), gs.size, bitsOccupiedAttributes)
    commonAttributes(commonObjects(bs))
  }

  def commonObjects(bs: collection.BitSet): mutable.BitSet = {
    intersectionOfBitSets(bs.iterator.map(b => occupiedIncidenceMatrix.col(b)), bs.size, objects)
  }

  def commonAttributes(gs: collection.BitSet): mutable.BitSet = {
    intersectionOfBitSets(gs.iterator.map(g => occupiedIncidenceMatrix.row(g)), gs.size, bitsOccupiedAttributes)
  }

//  def closure(bs: collection.BitSet): collection.BitSet = {
//    val gs = bs.foldLeft(objects)(_ & occupiedIncidenceMatrix.col(_))
//    gs.foldLeft(cxtAttributes)(_ & occupiedIncidenceMatrix.row(_))
//  }

  def toString(m: M): String = {
    m match
      case OWLNothing => OWLNothing.toString
      case A @ Class(_) => A.toString
      case (r @ ObjectProperty(_), c: BitSet) => "ObjectSomeValuesFrom(" + r + "," + c.mkString("MMSC(", ",", ")") + ")"
  }

  def writeToFile(cxtFile: File): Unit = {
    val writer = new BufferedWriter(new FileWriter(cxtFile))
    writer.write("B\n")
    writer.write("\n")
    writer.write(objects.size + "\n")
    writer.write(occupiedAttributes.size + "\n")
    writer.write("\n")
    objects.foreach(g => writer.write(g.toString + "\n"))
    occupiedAttributes.foreach(m => writer.write(toString(m) + "\n"))
    objects.foreach(g => {
      occupiedAttributes.foreach(m => {
//        if (incidence(g, m)) writer.write("X")
        if (occupiedIncidenceMatrix(g, occupiedAttributeIndex(m))) writer.write("X")
        else writer.write(".")
      })
      writer.write("\n")
    })
//    writer.write("\n\n\n")
//    attributes.foreach(m => if (!occupiedAttributes.contains(m)) writer.write(toString(m) + "\n"))
    writer.close()
  }

}
