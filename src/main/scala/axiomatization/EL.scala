package de.tu_dresden.inf.lat
package axiomatization

import org.phenoscape.scowl.*
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.model.{OWLClassExpression, OWLLogicalAxiom, OWLOntology}

import scala.annotation.tailrec
import scala.jdk.StreamConverters.*

object EL {

  def extractELTBox(ontology: OWLOntology, excludePremisesNotInEL: Boolean = true, includeOWLNothing: Boolean = false): LazyList[OWLLogicalAxiom] = {

    // TODO: convert all supported axioms to SubClassOf axioms (later code can then be simplified)

    if (excludePremisesNotInEL) {

      ontology.tboxAxioms(Imports.INCLUDED).toScala(LazyList).collect({
        case SubClassOf(_, premise, conclusion) if isEL(premise, includeOWLNothing) =>
          SubClassOf(premise, getELSubExpression(conclusion, includeOWLNothing))
        case ObjectPropertyDomain(_, property@ObjectProperty(_), classExpression) =>
          ObjectPropertyDomain(property, getELSubExpression(classExpression, includeOWLNothing))
        //        SubClassOf(ObjectSomeValuesFrom(property, OWLThing), getELSubExpression(classExpression))
        case EquivalentClasses(_, classExpressions) if classExpressions.filter(isEL(_, includeOWLNothing)).size > 1 =>
          EquivalentClasses(classExpressions.filter(isEL(_, includeOWLNothing)))
      })

    } else {

      ontology.tboxAxioms(Imports.INCLUDED).toScala(LazyList).collect({
        case SubClassOf(_, premise, conclusion) =>
          SubClassOf(getELSubExpression(premise, includeOWLNothing), getELSubExpression(conclusion, includeOWLNothing))
        case ObjectPropertyDomain(_, property@ObjectProperty(_), classExpression) =>
          ObjectPropertyDomain(property, getELSubExpression(classExpression, includeOWLNothing))
        //        SubClassOf(ObjectSomeValuesFrom(property, OWLThing), getELSubExpression(classExpression))
        case EquivalentClasses(_, classExpressions) =>
          EquivalentClasses(classExpressions.map(getELSubExpression(_, includeOWLNothing)))
      })

    }

  }

//  @tailrec
  def isEL(classExpression: OWLClassExpression, includeOWLNothing: Boolean = false): Boolean =
    classExpression.conjunctSet().allMatch({
      case owlClass@Class(_) if includeOWLNothing || !(owlClass equals OWLNothing) => true
      case ObjectSomeValuesFrom(_@ObjectProperty(_), filler) => isEL(filler, includeOWLNothing)
      case _ => false
    })

  def getELSubExpression(classExpression: OWLClassExpression, includeOWLNothing: Boolean = false): OWLClassExpression = {
    //      classExpression.conjunctSet().toScala(LazyList).collect({
    //        case owlClass @ Class(_) if !(owlClass equals OWLNothing) => owlClass
    //        case ObjectSomeValuesFrom(property @ ObjectProperty(_), filler) => ObjectSomeValuesFrom(property, getELSubExpression(filler))
    //      }).fold(OWLThing)(_ and _)
    val operands =
    classExpression.conjunctSet().toScala(LazyList).collect({

      case owlClass@Class(_) if includeOWLNothing || !(owlClass equals OWLNothing) =>
        owlClass

      case ObjectSomeValuesFrom(property@ObjectProperty(_), filler) =>
        val elFiller = getELSubExpression(filler, includeOWLNothing)
        if (elFiller equals OWLNothing)
          OWLNothing
        else
          ObjectSomeValuesFrom(property, elFiller)

      case ObjectMinCardinality(cardinality, property@ObjectProperty(_), filler) if cardinality > 0 =>
        val elFiller = getELSubExpression(filler, includeOWLNothing)
        if (elFiller equals OWLNothing)
          OWLNothing
        else
          ObjectSomeValuesFrom(property, elFiller)

      case ObjectExactCardinality(cardinality, property@ObjectProperty(_), filler) if cardinality > 0 =>
        val elFiller = getELSubExpression(filler, includeOWLNothing)
        if (elFiller equals OWLNothing)
          OWLNothing
        else
          ObjectSomeValuesFrom(property, elFiller)

      case ObjectHasSelf(property@ObjectProperty(_)) =>
        ObjectSomeValuesFrom(property, OWLThing)

      case ObjectHasValue(property@ObjectProperty(_), _) =>
        ObjectSomeValuesFrom(property, OWLThing)

    }).toSet
    if (operands.isEmpty)
      OWLThing
    else if (operands contains OWLNothing)
      OWLNothing
    else if (operands.size equals 1)
      operands.head
    else
      ObjectIntersectionOf(operands)
  }

}
