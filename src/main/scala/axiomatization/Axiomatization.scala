package de.tu_dresden.inf.lat
package axiomatization

import org.phenoscape.scowl.*
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.model.{IRI, NodeID, OWLAnonymousIndividual, OWLClass, OWLClassExpression, OWLIndividual, OWLLogicalAxiom, OWLNamedIndividual, OWLObjectProperty, OWLObjectSomeValuesFrom}
import uk.ac.manchester.cs.owl.owlapi.{OWLAnonymousIndividualImpl, OWLNamedIndividualImpl}

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
import de.tu_dresden.inf.lat.axiomatization.IncrementalPoweringClosureOperator
import de.tu_dresden.inf.lat.axiomatization.Util.{GLOBAL_COUNTER, LocalSet, fixedPoint, formatTime, measureExecutionTime, intersectionOfBitSets}
import org.semanticweb.elk.owlapi.ElkReasonerFactory


type M = OWLClass | (OWLObjectProperty, collection.BitSet)
type Mx = M | OWLObjectSomeValuesFrom
type BitImplication = (collection.BitSet, collection.BitSet)

val withDisjointnessAxioms = false

object Axiomatization {

  def run(ontologyFile: File, ont: String): Unit = {

    val startTime = System.currentTimeMillis()

    val manager = OWLManager.createOWLOntologyManager()
    val ontology = manager.loadOntologyFromOntologyDocument(ontologyFile)

    val graph = Interpretation.loadFromOntologyFile(ontology)
    println(graph.nodes().size + " individuals")

    val (reduction, _, _) = Interpretation.reductionOf(graph)

//    def isEL(classExpression: OWLClassExpression): Boolean =
//      classExpression.conjunctSet().allMatch({
//        case owlClass @ Class(_) if !(owlClass equals OWLNothing) => true
//        case ObjectSomeValuesFrom(_ @ ObjectProperty(_), filler) => isEL(filler)
//        case _ => false
//      })

    val elOntology = manager.createOntology()

    def getELSubExpression(classExpression: OWLClassExpression): OWLClassExpression = {
//      classExpression.conjunctSet().toScala(LazyList).collect({
//        case owlClass @ Class(_) if !(owlClass equals OWLNothing) => owlClass
//        case ObjectSomeValuesFrom(property @ ObjectProperty(_), filler) => ObjectSomeValuesFrom(property, getELSubExpression(filler))
//      }).fold(OWLThing)(_ and _)
      val operands =
        classExpression.conjunctSet().toScala(LazyList).collect({
          case owlClass@Class(_) if !(owlClass equals OWLNothing) => owlClass
          case ObjectSomeValuesFrom(property@ObjectProperty(_), filler) => ObjectSomeValuesFrom(property, getELSubExpression(filler))
        }).toSet
      if (operands.isEmpty)
        OWLThing
      else if (operands.size equals 1)
        operands.head
      else
        ObjectIntersectionOf(operands)
    }

    // TODO: convert all supported axioms to SubClassOf axioms (later code can then be simplified)
    val elTBox = ontology.tboxAxioms(Imports.INCLUDED).toScala(LazyList).collect({
      case SubClassOf(_, premise, conclusion) =>
        SubClassOf(getELSubExpression(premise), getELSubExpression(conclusion))
      case ObjectPropertyDomain(_, property @ ObjectProperty(_), classExpression) =>
        ObjectPropertyDomain(property, getELSubExpression(classExpression))
//        SubClassOf(ObjectSomeValuesFrom(property, OWLThing), getELSubExpression(classExpression))
      case EquivalentClasses(_, classExpressions) =>
        EquivalentClasses(classExpressions.map(getELSubExpression(_)))
    }).toList

    println(elTBox.size + " EL concept inclusions in the TBox")
    println(elTBox.filter(_.nestedClassExpressions().toScala(LazyList).exists(_.isOWLNothing)).size + " CIs with OWLNothing.")

    elOntology.addAxioms(elTBox : _*)

    val _representativeOf = mutable.HashMap[OWLClassExpression, OWLClass]()
    val _representedBy = mutable.HashMap[OWLClass, OWLClassExpression]()

    def representativeOf(classExpression: OWLClassExpression): OWLClass = {
      classExpression match
        case c @ Class(_) => c
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
        case _ @ Class(_) => false
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
        case _ @ Class(_) => {}
        case existentialRestriction @ ObjectSomeValuesFrom(_ @ ObjectProperty(_), filler) =>
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
      case ObjectPropertyDomain(_, property @ ObjectProperty(_), classExpression) =>
        // TODO: Is this needed?
        collectSubClassExpressions(ObjectSomeValuesFrom(property, OWLThing))
        collectSubClassExpressions(classExpression)
      case EquivalentClasses(_, classExpressions) =>
        classExpressions.foreach(collectSubClassExpressions(_))
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

    val elkReasoner = ElkReasonerFactory().createNonBufferingReasoner(elOntology)
    elkReasoner.precomputeInferences()
    elkReasoner.flush()

    def elkReasoner_subsumers(classExpression: OWLClassExpression): LazyList[OWLClass] = {
      elkReasoner.equivalentClasses(classExpression).toScala(LazyList) concat elkReasoner.superClasses(classExpression).toScala(LazyList)
    }

    def elkReasoner_types(individual: OWLNamedIndividual): LazyList[OWLClass] = {
      elkReasoner.types(individual).toScala(LazyList)
    }

    def inferredLabels(node: Int | OWLClassExpression): LazyList[OWLClass] = {
      node match
        case node : Int =>
          elkReasoner_types(individualFor(node)).filter(!isRepresentative(_))
        case node : OWLClassExpression =>
          elkReasoner_subsumers(representativeOf(node)).filter(!isRepresentative(_))
    }

    def inferredSuccessors(node: Int | OWLClassExpression): LazyList[(OWLObjectProperty, Int | OWLClassExpression)] = {
      node match
        case node : Int =>
          LazyList.from(
            reduction
              .successorRelations(node)
              .flatMap(property => reduction.successorsForRelation(node, property).map(succ => (property, succ)))
          ) concat (
            elkReasoner_types(individualFor(node))
              .filter(isRepresentative)
              .map(representedBy)
              .collect({ case ObjectSomeValuesFrom(property @ ObjectProperty(_), filler) => (property, filler) })
          )
        case node : OWLClassExpression =>
          elkReasoner_subsumers(representativeOf(node))
            .filter(isRepresentative)
            .map(representedBy)
            .collect({ case ObjectSomeValuesFrom(property @ ObjectProperty(_), filler) => (property, filler) })
    }

    val canonicalModel = BitGraph[OWLClass, OWLObjectProperty]()

    canonicalModel.labels().addAll(reduction.labels())
    canonicalModel.relations().addAll(reduction.relations())

    val _numberOf = mutable.HashMap[OWLClassExpression, Int]()
    val _withNumber = mutable.HashMap[Int, OWLClassExpression]() // new Array[OWLClassExpression]()
    var k = reduction.nodes().size - 1
    def numberOf(node: Int | OWLClassExpression): Int = {
      node match
        case node : Int => node
        case node : OWLClassExpression => _numberOf.getOrElseUpdate(node, { k += 1; _withNumber.update(k, node); k })
    }
    def withNumber(i: Int): Int | OWLClassExpression = {
      _withNumber.getOrElse(i, i)
    }

    @tailrec
    def insertIntoCanonicalModel(nodes: Iterable[Int | OWLClassExpression]): Unit = {
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

    val (reducedCanonicalModel, _rcmRepresentedBy, _rcmRepresentativeOf) = Interpretation.reductionOf(canonicalModel, true)
    val n = reducedCanonicalModel.nodes().size

    def rcmRepresentedBy(i: Int): collection.Set[Int | OWLClassExpression] = {
      _rcmRepresentedBy(i).unsorted.map(withNumber)
    }

    def rcmRepresentativeOf(node: Int | OWLClassExpression): Int = {
      _rcmRepresentativeOf(numberOf(node))
    }

    def rcmRepresentsIndividual(i: Int): Boolean = {
      rcmRepresentedBy(i).exists({ case _ : Int => true; case _ => false})
    }

    def rcmRepresentsClassExpression(i: Int): Boolean = {
      rcmRepresentedBy(i).exists({ case _ : OWLClassExpression => true; case _ => false})
    }

    println("Computing closures...")
    //    val closures = FCbO.computeAllClosures(n, IncrementalPoweringClosureOperator(reduction))
    val closures = FCbO.computeAllClosures(n, PoweringClosureOperator(reducedCanonicalModel))
    println(closures.size + " closures")
    GLOBAL_COUNTER.reset()

    val cxt = InducedFormalContext(reducedCanonicalModel, rcmRepresentsIndividual, closures)

    measureExecutionTime({
      cxt.objects
      cxt.occupiedAttributes
      cxt.occupiedAttributeIndex
      cxt.bitsOccupiedAttributes
      cxt.occupiedIncidenceMatrix
    }, "Computing the induced context took ")

    println()




    val _additionalAttributes = mutable.HashSet[OWLClass | OWLObjectSomeValuesFrom]()
    val f: PartialFunction[OWLClassExpression, OWLClass | OWLObjectSomeValuesFrom] = {
      case c@Class(_) if !c.isOWLThing => c
      case c@ObjectSomeValuesFrom(_, _) => c
    }
    elTBox.foreach({
      case SubClassOf(_, premise, conclusion) => {
        _additionalAttributes.addAll(premise.conjunctSet().toScala(LazyList).collect(f))
        _additionalAttributes.addAll(conclusion.conjunctSet().toScala(LazyList).collect(f))
      }
      case ObjectPropertyDomain(_, property@ObjectProperty(_), conclusion) => {
        _additionalAttributes.add(ObjectSomeValuesFrom(property, OWLThing))
        _additionalAttributes.addAll(conclusion.conjunctSet().toScala(LazyList).collect(f))
      }
      case EquivalentClasses(_, operands) => {
        operands.foreach(op => _additionalAttributes.addAll(op.conjunctSet().toScala(LazyList).collect(f)))
      }
    })

    // TODO: should be inserted later, after the reduction of the canonical model
    insertIntoCanonicalModel(_additionalAttributes.collect({
      case ObjectSomeValuesFrom(_, filler) => filler
    }))

    val (reducedCanonicalModel2, _rcmRepresentedBy2, _rcmRepresentativeOf2) = Interpretation.reductionOf(canonicalModel, true)

    def rcmRepresentedBy2(i: Int): collection.Set[Int | OWLClassExpression] = {
      _rcmRepresentedBy2(i).unsorted.map(withNumber)
    }

    def rcmRepresentativeOf2(node: Int | OWLClassExpression): Int = {
      _rcmRepresentativeOf2(numberOf(node))
    }

    def rcmRepresentsIndividual2(i: Int): Boolean = {
      rcmRepresentedBy2(i).exists({ case _: Int => true; case _ => false })
    }

    def rcmRepresentsClassExpression2(i: Int): Boolean = {
      rcmRepresentedBy2(i).exists({ case _: OWLClassExpression => true; case _ => false })
    }

    val tboxSaturation = BitGraph[OWLClass, OWLObjectProperty]()
    tboxSaturation.labels().addAll(reducedCanonicalModel2.labels())
    tboxSaturation.relations().addAll(reducedCanonicalModel2.relations())

    val startBackgroundImplications = System.currentTimeMillis()

    @tailrec
    def insertIntoTBoxSaturation(nodes: Iterable[Int]): Unit = {
      val next = mutable.ListBuffer[Int]()
      nodes.foreach(i => {
        if (!tboxSaturation.nodes().contains(i)) {
          tboxSaturation.addNode(i)
          tboxSaturation.addLabels(i, reducedCanonicalModel.labels(i))
          reducedCanonicalModel.successorRelations(i)
            .foreach(property => {
              reducedCanonicalModel.successorsForRelation(i, property)
                .foreach(j => {
                  tboxSaturation.addEdge(i, property, j)
                  next.addOne(j)
                })
            })
        }
      })
      if (next.nonEmpty)
        insertIntoTBoxSaturation(next)
    }

    insertIntoTBoxSaturation(reducedCanonicalModel2.nodes().filter(rcmRepresentsClassExpression2))

    val clop = PoweringClosureOperator(tboxSaturation)

    def simulates(succ: collection.BitSet | OWLClassExpression, filler: OWLClassExpression): Boolean = {
      succ match
        case succ: BitSet =>
          succ.forall(i => {
            rcmRepresentedBy(i).head match
              case j: Int =>
                elkReasoner_types(individualFor(j)).contains(representativeOf(filler))
              case c: OWLClassExpression =>
                elkReasoner_subsumers(representativeOf(c)).contains(representativeOf(filler))
          })
        case succ: OWLClassExpression =>
          elkReasoner_subsumers(representativeOf(succ)).contains(representativeOf(filler))
    }

    // optimized version of dlClosure:
    // use FCA implications over extended attribute set: M ∪ { ∃r.C | ∃r.C ∈ Conj(D)∪Conj(E) for some D⊑E ∈ 𝓣 }
    // use BitSets to represent subsets of the extended attribute set
    // pre-compute implications needed to compute the closure
    // (1)  {(r,X)} → {∃r.C}   if  simulates(X, C) == true
    // (2)   {∃r.C} → {∃r.D}   if  simulates(C, D) == true
    // (3)  Conj(C) → Conj(D)  if  C⊑D ∈ 𝓣
    // (4)  {(r,X)} → {(r,Y)}  if  X⊆Y
    // (5)   {∃r.C} → {(r,X)}  if  C simulates X, i.e., there is a simulation from (𝔓(𝓘),X) to (𝓘_𝓣,C)

    val extendedAttributeSet: Array[Mx] = cxt.occupiedAttributes ++ _additionalAttributes
    val extendedAttributeIndex = mutable.HashMap[Mx, Int]()
    (0 until extendedAttributeSet.length).foreach(i => {
      extendedAttributeIndex.update(extendedAttributeSet(i), i)
    })

    val backgroundImplications = mutable.HashSet[BitImplication]()


    backgroundImplications.addOne((BitSet(0), BitSet.fromSpecific(1 until extendedAttributeSet.length)))
    //    backgroundImplications.addOne((BitSet(0), BitSet.fromSpecific(1 until cxt.occupiedAttributes.length)))
    //    (1 until extendedAttributeSet.length).foreach(i => {
    //      backgroundImplications.addOne((BitSet(0), BitSet(i)))
    //    })

    extendedAttributeSet.foreach({
      case pair1 @ (property1: OWLObjectProperty, mmsc1: collection.BitSet) => {
//        extendedAttributeSet.foreach({
//          case pair2 @ (property2: OWLObjectProperty, mmsc2: collection.BitSet) if property1 equals property2 => {
//            // (4)  {(r,X)} → {(r,Y)}  if  X⊆Y
//            // use iPred
//            if (mmsc1 subsetOf mmsc2)
//              backgroundImplications.addOne((BitSet(extendedAttributeIndex(pair1)), BitSet(extendedAttributeIndex(pair2))))
//          }
//          case exr2 @ ObjectSomeValuesFrom(property2 @ ObjectProperty(_), filler2) if property1 equals property2 => {
//            // (1)  {(r,X)} → {∃r.C}   if  simulates(X, C) == true
//            if (simulates(mmsc1, filler2))
//              backgroundImplications.addOne((BitSet(extendedAttributeIndex(pair1)), BitSet(extendedAttributeIndex(exr2))))
//          }
//          case _ => {}
//        })
        val cs = extendedAttributeSet.collect({
          // (4)  {(r,X)} → {(r,Y)}  if  X⊆Y
          // use iPred
          case pair2@(property2: OWLObjectProperty, mmsc2: collection.BitSet)
            if (property1 equals property2) && (mmsc1 subsetOf mmsc2) => extendedAttributeIndex(pair2)

          // (1)  {(r,X)} → {∃r.C}   if  simulates(X, C) == true
          case exr2@ObjectSomeValuesFrom(property2@ObjectProperty(_), filler2)
            if (property1 equals property2) && (simulates(mmsc1, filler2)) => extendedAttributeIndex(exr2)
        })
        if (cs.nonEmpty)
          backgroundImplications.addOne((BitSet(extendedAttributeIndex(pair1)), BitSet.fromSpecific(cs)))
      }
      case exr1 @ ObjectSomeValuesFrom(property1 @ ObjectProperty(_), filler1) => {
//        extendedAttributeSet.foreach({
//          case pair2 @ (property2: OWLObjectProperty, mmsc2: collection.BitSet) if property1 equals property2 => {
//            // (5)   {∃r.C} → {(r,X)}  if  C simulates X, i.e., there is a simulation from (𝔓(𝓘),X) to (𝓘_𝓣,C)
//            if (clop(mmsc2) contains rcmRepresentativeOf2(filler1))
//              backgroundImplications.addOne((BitSet(extendedAttributeIndex(exr1)), BitSet(extendedAttributeIndex(pair2))))
//          }
//          case exr2 @ ObjectSomeValuesFrom(property2 @ ObjectProperty(_), filler2) if property1 equals property2 => {
//            // (2)   {∃r.C} → {∃r.D}   if  simulates(C, D) == true
//            if (simulates(filler1, filler2))
//              backgroundImplications.addOne((BitSet(extendedAttributeIndex(exr1)), BitSet(extendedAttributeIndex(exr2))))
//          }
//          case _ => {}
//        })
        val cs = extendedAttributeSet.collect({
          // (5)   {∃r.C} → {(r,X)}  if  C simulates X, i.e., there is a simulation from (𝔓(𝓘),X) to (𝓘_𝓣,C)
          case pair2@(property2: OWLObjectProperty, mmsc2: collection.BitSet)
            if (property1 equals property2) && (clop(mmsc2) contains rcmRepresentativeOf2(filler1)) => extendedAttributeIndex(pair2)

          // (2)   {∃r.C} → {∃r.D}   if  simulates(C, D) == true
          case exr2@ObjectSomeValuesFrom(property2@ObjectProperty(_), filler2)
            if (property1 equals property2) && (simulates(filler1, filler2)) => extendedAttributeIndex(exr2)
        })
        if (cs.nonEmpty)
          backgroundImplications.addOne((BitSet(extendedAttributeIndex(exr1)), BitSet.fromSpecific(cs)))
      }
      case _ => {}
    })

    elkReasoner.dispose()

    // (3)  Conj(C) → Conj(D)  if  C⊑D ∈ 𝓣
    elTBox.foreach({
      case SubClassOf(_, premise, conclusion) => {
        val p = premise.conjunctSet().toScala(LazyList).filter(!_.isOWLThing).map({ case x : (OWLClass | OWLObjectSomeValuesFrom) => extendedAttributeIndex(x) }).to(BitSet)
        val c = conclusion.conjunctSet().toScala(LazyList).filter(!_.isOWLThing).map({ case x : (OWLClass | OWLObjectSomeValuesFrom) => extendedAttributeIndex(x) }).to(BitSet)
        if (c.nonEmpty)
          backgroundImplications.addOne((p,c))
      }
      case ObjectPropertyDomain(_, property@ObjectProperty(_), conclusion) => {
        val p = BitSet(extendedAttributeIndex(ObjectSomeValuesFrom(property, OWLThing)))
        val c = conclusion.conjunctSet().toScala(LazyList).filter(!_.isOWLThing).map({ case x: (OWLClass | OWLObjectSomeValuesFrom) => extendedAttributeIndex(x) }).to(BitSet)
        if (c.nonEmpty)
          backgroundImplications.addOne((p, c))
      }
      case EquivalentClasses(_, operands) => {
        val n = operands.size
        val ps = new Array[BitSet](n)
        var i = 0
        operands.foreach(op => {
          ps(i) = op.conjunctSet().toScala(LazyList).filter(!_.isOWLThing).map({ case x : (OWLClass | OWLObjectSomeValuesFrom) => extendedAttributeIndex(x) }).to(BitSet)
          i += 1
        })
        (0 until (n-2)).foreach(j => {
          if (ps(j+1).nonEmpty)
            backgroundImplications.addOne((ps(j), ps(j+1)))
        })
        if (ps(0).nonEmpty)
          backgroundImplications.addOne((ps(n-1), ps(0)))
      }
    })

//    // TODO: remove
//    backgroundImplications.addOne((BitSet.empty, BitSet.fromSpecific(0 until cxt.occupiedAttributes.length)))

    println()
    val endBackgroundImplications = System.currentTimeMillis()
    println("Computing the background implications took " + formatTime(endBackgroundImplications - startBackgroundImplications))
    println(backgroundImplications.size + " background implications computed.")
    println("There are " + backgroundImplications.filter(_._1.isEmpty).size + " background implications with empty premise.")
    println("There are " + backgroundImplications.filter(_._2.isEmpty).size + " background implications with empty conclusion.")

    val cbase = measureExecutionTime({
      LinCbO.computeCanonicalBase(cxt)
    }, "Computing the canonical base took ")
    println()
    println(cbase.size + " implications in cbase")
    println(cbase.filter((_, ys) => !ys.contains(0)).size + " implications in canonical base that are no disjointness axiom")
    println(cbase.filter((xs, _) => xs.contains(0)).size + " implications contain OWLNothing in the premise")
    println()
    val filteredcbase = measureExecutionTime({
      LinCbO.computeCanonicalBase(cxt, cxt.commonObjects(_).nonEmpty)
    }, "Computing the filtered canonical base took ")
    println()
    println(filteredcbase.size + " implications in filtered canonical base")
    println()

    val base = LinCbO_JKK.computeCanonicalBase(cxt, ont)
    println(base.size + " implications in canonical base")

    val cbaseBack = measureExecutionTime({
      LinCbOWithBackgroundImplications.computeCanonicalBase(cxt, backgroundImplications, extendedAttributeSet.length)
    }, "Computing the relative canonical base took ")
    println()
    println(cbaseBack.size + " implications in relative canonical base")
    println(cbaseBack.filter((_, ys) => !ys.contains(0)).size + " implications in relative canonical base that are no disjointness axiom")
    println(cbaseBack.filter((xs, _) => xs.contains(0)).size + " implications contain OWLNothing in the premise")
    println()
    val filteredcbaseBack = measureExecutionTime({
      LinCbOWithBackgroundImplications.computeCanonicalBase(cxt, backgroundImplications, extendedAttributeSet.length, ms => cxt.commonObjects(ms intersect cxt.bitsOccupiedAttributes).nonEmpty)
    }, "Computing the filtered relative canonical base took ")
    println()
    println(filteredcbaseBack.size + " implications in filtered relative canonical base")
    println(filteredcbaseBack.filter((_, ys) => !ys.contains(0)).size + " implications in filtered relative canonical base that are no disjointness axiom")
    println()

    def printlnImp(implication: BitImplication): Unit = {
      val p = implication._1
      val c = implication._2
      var str = ""
      str += (0 until extendedAttributeSet.length).foldLeft("")((s,i) => s + (if (i == cxt.occupiedAttributes.length) "|" else "") + (if (p(i)) "X" else "."))
      str += " ⟶ "
      str += (0 until extendedAttributeSet.length).foldLeft("")((s,i) => s + (if (i == cxt.occupiedAttributes.length) "|" else "") + (if (c(i)) "X" else "."))
      println(str)
    }

    println((cbaseBack diff cbase.toSet).size + " additional implications.")
//    (cbaseBack diff cbase.toSet).foreach(printlnImp)
    println((cbase.toSet diff cbaseBack).size + " additional implications.")
//    (cbase.toSet diff cbaseBack).foreach(printlnImp)
    println()
    println(backgroundImplications.size + " background implications.")
//    backgroundImplications.foreach(printlnImp)
    println()

    val computationTime = System.currentTimeMillis() - startTime
    println(formatTime(computationTime))

    println("Dataset: " + ont)

  }

}
