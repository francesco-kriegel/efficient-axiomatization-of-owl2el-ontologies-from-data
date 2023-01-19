package de.tu_dresden.inf.lat
package axiomatization

import axiomatization.Util.intersectionOfBitSets

import org.phenoscape.scowl.*
import org.semanticweb.owlapi.model.{OWLClass, OWLObjectProperty}

import java.io.{BufferedWriter, File, FileWriter}
import scala.annotation.threadUnsafe
import scala.collection.immutable.BitSet
import scala.collection.mutable


class InducedFormalContext(reduction: BitGraph[OWLClass, OWLObjectProperty],
                           isIndividual: Int => Boolean = _ => true,
                           closures: collection.Set[collection.BitSet],
                           val whichDisjointnessAxioms: WhichDisjointnessAxioms) {

  val includeOWLNothing = !(whichDisjointnessAxioms equals WhichDisjointnessAxioms.None)
  val full = whichDisjointnessAxioms equals WhichDisjointnessAxioms.Canonical

  @threadUnsafe
  lazy val objects: mutable.BitSet =
    reduction.nodes().filter(isIndividual)

  @threadUnsafe
  lazy val attributes: Array[M] =
    Array[M](List(OWLNothing) ++ reduction.labels() ++ reduction.relations().flatMap(r => closures.map(c => ObjectSomeValuesFromMMSC(r, c))) : _*)

//  lazy val attributeIndex: collection.Map[M, Int] = {
//    val index = mutable.HashMap[M, Int]()
//    (0 until attributes.length).foreach(i => index.update(attributes(i), i))
//    index
//  }

//  def fromBits(bs: collection.BitSet): collection.Set[M] = {
//    bs.unsorted.map(attributes)
//  }

//  def toBits(ms: collection.Set[M]): collection.BitSet = {
//    mutable.BitSet.fromSpecific(ms.iterator.map(attributeIndex))
//  }

//  lazy val incidenceMatrix: BitBiMap = {
//    val matrix = BitBiMap()
//    objects.foreach(g => {
//      (0 until attributes.length).foreach(m => if (incidence(g, attributes(m))) matrix.add(g, m))
//    })
//    matrix
//  }

  private def incidence(g: Int, m: M): Boolean = {
    m match
      case OWLNothing => false
      case A @ Class(_) => reduction.labels(g).contains(A)
      // case (r @ ObjectProperty(_), c: BitSet) => reduction.successorsForRelation(g, r).exists(c(_))
      // case (r @ ObjectProperty(_), c: BitSet) => (reduction.successorsForRelation(g, r) intersect c).nonEmpty
      case ObjectSomeValuesFromMMSC(r, c) => (reduction.successorsForRelation(g, r) intersect c).nonEmpty
      case _ => throw new RuntimeException("This should not happen.") // to be exhaustive
  }

  def isOccupiedAttribute(m: M): Boolean = {
    // (includeOWLNothing && (m equals OWLNothing)) || objects.exists(g => incidence(g, m))
    if (m equals OWLNothing)
      includeOWLNothing
    else
      objects.exists(g => incidence(g, m))
  }

  @threadUnsafe
  lazy val activeAttributes = if full then attributes else attributes.filter(isOccupiedAttribute(_))

  @threadUnsafe
  lazy val activeAttributeIndex: collection.Map[M, Int] = {
    val index = mutable.HashMap[M, Int]()
    (0 until activeAttributes.length).foreach(i => index.update(activeAttributes(i), i))
    index
  }

//  lazy val unsatisfiableAttributes =
//    attributes diff (List(OWLNothing) ++ occupiedAttributes)

//  def fromBits(bs: collection.BitSet): collection.Set[M] = {
//    bs.unsorted.map(occupiedAttributes)
//  }
//
//  def toBits(ms: collection.Set[M]): collection.BitSet = {
//    mutable.BitSet.fromSpecific(ms.iterator.map(occupiedAttributeIndex))
//  }

  @threadUnsafe
  lazy val activeIncidenceMatrix: BitBiMap = {
    val matrix = BitBiMap()
    objects.foreach(g => {
      (0 until activeAttributes.length).foreach(m => if (incidence(g, activeAttributes(m))) matrix.add(g, m))
    })
    matrix
  }

  @threadUnsafe
  lazy val bitsActiveAttributes = BitSet.fromSpecific(0 until activeAttributes.length)

//  def closure(bs: collection.BitSet): collection.BitSet = {
//    val gs = bs.foldLeft(objects)(_ & occupiedIncidenceMatrix.col(_))
//    gs.foldLeft(cxtAttributes)(_ & occupiedIncidenceMatrix.row(_))
//  }

  def closure(bs: collection.BitSet): mutable.BitSet = {
    // val gs = intersectionOfBitSets(bs.iterator.map(b => occupiedIncidenceMatrix.col(b)), bs.size, objects)
    // intersectionOfBitSets(gs.iterator.map(g => occupiedIncidenceMatrix.row(g)), gs.size, bitsOccupiedAttributes)
    commonAttributes(commonObjects(bs))
  }

  def commonObjects(bs: collection.BitSet): mutable.BitSet = {
    // intersectionOfBitSets(bs.iterator.map(b => activeIncidenceMatrix.col(b)), bs.size, objects)
    intersectionOfBitSets(bs.iterator.map(b => activeIncidenceMatrix.col(b)), objects)
//    if (bs.isEmpty)
//      objects
//    else
//      scala.collection.parallel.ParBitSet(bs).map(b => activeIncidenceMatrix.col(b)).reduce(_ & _)
  }

  def commonAttributes(gs: collection.BitSet): mutable.BitSet = {
    // intersectionOfBitSets(gs.iterator.map(g => activeIncidenceMatrix.row(g)), gs.size, bitsActiveAttributes)
    intersectionOfBitSets(gs.iterator.map(g => activeIncidenceMatrix.row(g)), bitsActiveAttributes)
//    if (gs.isEmpty)
//      mutable.BitSet.fromSpecific(bitsActiveAttributes)
//    else
//      scala.collection.parallel.ParBitSet(gs).map(g => activeIncidenceMatrix.row(g)).reduce(_ & _)
  }

  def toString(m: M): String = {
    m match
      case OWLNothing => OWLNothing.toString
      case A @ Class(_) => A.toString
      // case (r @ ObjectProperty(_), c: BitSet) => "ObjectSomeValuesFrom(" + r + "," + c.mkString("MMSC(", ",", ")") + ")"
      case ObjectSomeValuesFromMMSC(r, c) => "ObjectSomeValuesFrom(" + r + "," + c.mkString("MMSC(", ",", ")") + ")"
      case _ => throw new RuntimeException("This should not happen.") // to be exhaustive
  }

  def writeToFile(cxtFile: File): Unit = {
    val writer = new BufferedWriter(new FileWriter(cxtFile))
    writer.write("B\n")
    writer.write("\n")
    writer.write(objects.size + "\n")
    writer.write(activeAttributes.size + "\n")
    writer.write("\n")
    objects.foreach(g => writer.write(g.toString + "\n"))
    activeAttributes.foreach(m => writer.write(toString(m) + "\n"))
    objects.foreach(g => {
      activeAttributes.foreach(m => {
        // if (incidence(g, m)) writer.write("X")
        if (activeIncidenceMatrix(g, activeAttributeIndex(m))) writer.write("X")
        else writer.write(".")
      })
      writer.write("\n")
    })
    // writer.write("\n\n\n")
    // attributes.foreach(m => if (!occupiedAttributes.contains(m)) writer.write(toString(m) + "\n"))
    writer.flush()
    writer.close()
  }

}
