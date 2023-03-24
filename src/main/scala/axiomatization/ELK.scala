package de.tu_dresden.inf.lat
package axiomatization

import org.phenoscape.scowl.*
import org.semanticweb.elk.owlapi.ElkReasonerFactory
import org.semanticweb.owlapi.model.*

import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.StreamConverters.*


class ELK(val reduction: BitGraph[OWLClass, OWLObjectProperty], val elTBox: List[OWLAxiom], val manager: OWLOntologyManager) {

  val elOntology = manager.createOntology()
  elOntology.addAxioms(elTBox: _*)

  val _representativeOf = mutable.HashMap[OWLClassExpression, OWLClass]()
  val _representedBy = mutable.HashMap[OWLClass, OWLClassExpression]()

  def representativeOf(classExpression: OWLClassExpression): OWLClass = {
    classExpression match
      case c@Class(_) => c
      case c => _representativeOf(c)
  }

  def representedBy(clazz: OWLClass): OWLClassExpression = {
    _representedBy.getOrElse(clazz, clazz)
  }

  def isRepresentative(clazz: OWLClass): Boolean = {
    _representedBy.contains(clazz)
  }

  def addRepresentative(classExpression: OWLClassExpression): Boolean = {
    classExpression match
      case _@Class(_) => false
      case _ =>
        if (!_representativeOf.contains(classExpression)) {
          val representative = Class("internal_representative_class_for#" + classExpression)
          _representativeOf(classExpression) = representative
          _representedBy(representative) = classExpression
          elOntology.addAxiom(EquivalentClasses(classExpression, representative))
          true
        } else {
          false
        }
  }

  def collectSubClassExpressions(classExpression: OWLClassExpression): Unit = {
    classExpression.conjunctSet().toScala(LazyList).foreach({
      case _@Class(_) => {}
      case existentialRestriction@ObjectSomeValuesFrom(_@ObjectProperty(_), filler) =>
        // TODO: Why does the 'if' protection not work?
        //          if (addRepresentative(existentialRestriction))
        //            if (addRepresentative(filler))
        addRepresentative(existentialRestriction)
        addRepresentative(filler)
        collectSubClassExpressions(filler)
    })
  }

  elTBox.foreach({
    case SubClassOf(_, premise, classExpression) =>
      // TODO: Is all of the premise needed?
      //        premise.conjunctSet().toScala(LazyList).foreach({
      //          case ObjectSomeValuesFrom(_, filler) => addRepresentative(filler) // only needed for method "premiseMatches"
      //          case _ => {}
      //        })
      collectSubClassExpressions(premise)
      collectSubClassExpressions(classExpression)
      if !classExpression.isOWLClass then addRepresentative(classExpression)
    case ObjectPropertyDomain(_, property@ObjectProperty(_), classExpression) =>
      // TODO: Is this needed?
      collectSubClassExpressions(ObjectSomeValuesFrom(property, OWLThing))
      collectSubClassExpressions(classExpression)
      if !classExpression.isOWLClass then addRepresentative(classExpression)
    case EquivalentClasses(_, classExpressions) =>
//      classExpressions.foreach(collectSubClassExpressions(_))
      classExpressions.foreach(classExpression =>
        collectSubClassExpressions(classExpression)
        if !classExpression.isOWLClass then addRepresentative(classExpression)
      )
  })

  val individualFor = new Array[OWLNamedIndividual](reduction.nodes().size)

  def nodeFor(individual: OWLNamedIndividual): Int = {
    individual.getIRI.toString.substring(39).toInt
  }

  reduction.nodes().foreach(node => {
    individualFor(node) = Individual("internal_representative_individual_for#" + node)
  })
  reduction.nodes().foreach(node => {
    reduction.labels(node).foreach(label => {
      elOntology.addAxiom(ClassAssertion(label, individualFor(node)))
    })
    reduction.predecessors(node).foreach((relation, pred) => {
      elOntology.addAxiom(ObjectPropertyAssertion(relation, individualFor(pred), individualFor(node)))
    })
  })

  val reasoner = ElkReasonerFactory().createNonBufferingReasoner(elOntology)
  reasoner.precomputeInferences()
  reasoner.flush()

  def subsumers(classExpression: OWLClassExpression): LazyList[OWLClass] = {
    reasoner.equivalentClasses(classExpression).toScala(LazyList) concat reasoner.superClasses(classExpression).toScala(LazyList)
  }

  def types(individual: OWLNamedIndividual): LazyList[OWLClass] = {
    reasoner.types(individual).toScala(LazyList)
  }

  def inferredLabels(node: Int | OWLClassExpression): LazyList[OWLClass] = {
    node match
      case node: Int =>
        types(individualFor(node)).filter(!isRepresentative(_))
      case node: OWLClassExpression =>
        subsumers(representativeOf(node)).filter(!isRepresentative(_))
  }

  def inferredSuccessors(node: Int | OWLClassExpression): LazyList[(OWLObjectProperty, Int | OWLClassExpression)] = {
    node match
      case node: Int =>
        LazyList.from(
          reduction
            .successorRelations(node)
            .flatMap(property => reduction.successorsForRelation(node, property).map(succ => (property, succ)))
        ) concat (
          types(individualFor(node))
            .filter(isRepresentative)
            .map(representedBy)
            .collect({ case ObjectSomeValuesFrom(property@ObjectProperty(_), filler) => (property, filler) })
          )
      case node: OWLClassExpression =>
        subsumers(representativeOf(node))
          .filter(isRepresentative)
          .map(representedBy)
          .collect({ case ObjectSomeValuesFrom(property@ObjectProperty(_), filler) => (property, filler) })
  }

  val canonicalModel = BitGraph[OWLClass, OWLObjectProperty]()

  canonicalModel.labels().addAll(reduction.labels())
  canonicalModel.relations().addAll(reduction.relations())

  val _numberOf = mutable.HashMap[OWLClassExpression, Int]()
  val _withNumber = mutable.HashMap[Int, OWLClassExpression]() // new Array[OWLClassExpression]()
  var k = reduction.nodes().size - 1

  def numberOf(node: Int | OWLClassExpression): Int = {
    node match
      case node: Int => node
      case node: OWLClassExpression => _numberOf.getOrElseUpdate(node, {
        k += 1; _withNumber.update(k, node); k
      })
  }

  def withNumber(i: Int): Int | OWLClassExpression = {
    _withNumber.getOrElse(i, i)
  }

  @tailrec
  final def insertIntoCanonicalModel(nodes: Iterable[Int | OWLClassExpression]): Unit = {
    val succs = mutable.ListBuffer[Int | OWLClassExpression]()
    for (node <- nodes) {
      val i = numberOf(node)
      if (!canonicalModel.nodes().contains(i)) {
        canonicalModel.addNode(i)
        canonicalModel.addLabels(i, inferredLabels(node))
        inferredSuccessors(node).foreach((property, succ) => {
          val j = numberOf(succ)
          canonicalModel.addEdge(i, property, j)
          succs.addOne(succ)
        })
      }
    }
    if (succs.nonEmpty)
      insertIntoCanonicalModel(succs)
  }

  insertIntoCanonicalModel(reduction.nodes())

}
