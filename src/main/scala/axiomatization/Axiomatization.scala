package de.tu_dresden.inf.lat
package axiomatization

import axiomatization.Graph
import axiomatization.NestedParallelComputations.*
import axiomatization.Util.{LocalSet, fixedPoint, formatTime, intersectionOfBitSets, measureExecutionTime}

import org.phenoscape.scowl.*
import org.semanticweb.elk.owlapi.ElkReasonerFactory
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.formats.ManchesterSyntaxDocumentFormat
import org.semanticweb.owlapi.model.*
import org.semanticweb.owlapi.model.parameters.Imports
import uk.ac.manchester.cs.owl.owlapi.{OWLAnonymousIndividualImpl, OWLNamedIndividualImpl}

import java.io.*
import java.text.SimpleDateFormat
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import scala.annotation.tailrec
import scala.collection.immutable.BitSet
import scala.collection.mutable
import scala.collection.mutable.ArraySeq
import scala.io.StdIn.readLine
import scala.jdk.CollectionConverters.*
import scala.jdk.StreamConverters.*
import scala.util.control.Breaks.*


class ObjectSomeValuesFromMMSC(val property: OWLObjectProperty, val mmsc: collection.BitSet) {}

object ObjectSomeValuesFromMMSC {
  def unapply(x: ObjectSomeValuesFromMMSC): Option[(OWLObjectProperty, collection.BitSet)] = {
    Some((x.property, x.mmsc))
  }
}

type M = OWLClass | ObjectSomeValuesFromMMSC
type Mx = M | OWLObjectSomeValuesFrom
type BitImplication = (collection.BitSet, collection.BitSet)

enum WhichDisjointnessAxioms:
  case None, Fast, Canonical

object Axiomatization {

  def run(ontologyFile: File,
          ont: String,
          whichDisjointnessAxioms: WhichDisjointnessAxioms,
          maxConjunctionSize: Option[Int],
          maxRoleDepth: Option[Int],
          onlyComputeReduction: Boolean = false)
         (using logger: Logger): Unit = {

    if (maxConjunctionSize.isDefined && (maxConjunctionSize.get < 0))
      throw IllegalArgumentException()
    if (maxRoleDepth.isDefined && (maxRoleDepth.get < 0))
      throw IllegalArgumentException()

    val withDisjointnessAxioms = !(whichDisjointnessAxioms equals WhichDisjointnessAxioms.None)
    val canonicalDisjointnessAxioms = whichDisjointnessAxioms equals WhichDisjointnessAxioms.Canonical
    val reducedOntologyFilename = "ore2015_pool_sample_experiments/files/" + ont + "_reduced.owl"
    val statisticsFilename = "ore2015_pool_sample_experiments/statistics/" + ont + ".csv"
    val resultsFilename = "ore2015_pool_sample_experiments/results/" + ont + ".csv"
    val reducedOntologyFile = File(reducedOntologyFilename)
    val reductionHasBeenPrecomputed = reducedOntologyFile.exists()

    lazy val statisticsFromFile: Array[String] = {
      val reader = new BufferedReader(new FileReader(statisticsFilename))
      val statistics = reader.readLine().split(';')//.map(_.toLong)
      reader.close()
      statistics
    }

    def writeResults(string: String): Unit = {
      val writer = new BufferedWriter(new FileWriter(resultsFilename, true))
      writer.write(string)
      writer.newLine()
      writer.flush()
      writer.close()
    }



    /* *************************** */
    /* START: Initialization Phase */
    /* *************************** */

    logger.println(
      "\n\n" +
        "*******************************\n" +
        "* START: Initialization Phase *\n" +
        "*******************************\n"
    )

    val startTime0 = System.currentTimeMillis()

    val manager = OWLManager.createOWLOntologyManager()
    val ontology = manager.loadOntologyFromOntologyDocument(if reductionHasBeenPrecomputed then reducedOntologyFile else ontologyFile)
    val graph = BitGraph.fromOntology(ontology)
    val elbotTBox =
      if reductionHasBeenPrecomputed
      then ontology.tboxAxioms(Imports.INCLUDED).toScala(LazyList).toList
      else EL.extractELTBox(ontology, false, true).toList

    val measurement_ComputationTime_LoadOntology =
      if reductionHasBeenPrecomputed
      then statisticsFromFile(2).toLong
      else System.currentTimeMillis() - startTime0
    logger.println("Loading the ontology took " + formatTime(measurement_ComputationTime_LoadOntology))

    if (reductionHasBeenPrecomputed && !(graph.sizeCode() equals statisticsFromFile(4)))
      logger.errPrintln("Size code was: " + statisticsFromFile(4))
      logger.errPrintln("Size code is:  " + graph.sizeCode())
      throw new RuntimeException("Something went wrong with writing and then reading the reduction.")

    val measurement_Number_ObjectsInDomain =
      if reductionHasBeenPrecomputed
      then statisticsFromFile(0).toInt
      else graph.nodes().size
    logger.println(measurement_Number_ObjectsInDomain + " individuals")





    val ((reduction, _, _, _), measurement_ComputationTime_FirstReduction) =
      if reductionHasBeenPrecomputed
      then ((graph, PartialFunction.empty, PartialFunction.empty, BitBiMap()), statisticsFromFile(3).toLong)
      else measureExecutionTime { BitGraph.computeReduction(graph) }
    logger.println("Reducing the input interpretation took " + formatTime(measurement_ComputationTime_FirstReduction))

    val measurement_Number_ObjectsInReducedDomain =
      if reductionHasBeenPrecomputed
      then statisticsFromFile(1).toInt
      else reduction.nodes().size

    if (!reductionHasBeenPrecomputed) {

      val variables = new Array[OWLAnonymousIndividual](reduction.nodes().size)
      reduction.nodes() foreach {
        variables(_) = new OWLAnonymousIndividualImpl(NodeID.getNodeID())
      }
      val store = manager.createOntology()
      reduction.labels().map(Declaration(_)).foreach(store.add)
      reduction.relations().map(Declaration(_)).foreach(store.add)
      reduction.nodes().foreach(node => {
        if (reduction.labels(node).isEmpty && reduction.predecessors(node).isEmpty)
          store.add(ClassAssertion(OWLThing, variables(node)))
        reduction.labels(node).map(ClassAssertion(_, variables(node))).foreach(store.add)
        reduction.predecessors(node).map((property, pred) => ObjectPropertyAssertion(property, variables(pred), variables(node))).foreach(store.add)
      })
      store.add(elbotTBox: _*)
      //      store.addAxioms(ontology.tboxAxioms(Imports.INCLUDED))
      val stream = new FileOutputStream(reducedOntologyFilename)
      manager.saveOntology(store, ManchesterSyntaxDocumentFormat(), stream)
      stream.close()

      val statWriter = new BufferedWriter(new FileWriter(statisticsFilename))
      statWriter.write(
        measurement_Number_ObjectsInDomain + ";" +
          measurement_Number_ObjectsInReducedDomain + ";" +
          measurement_ComputationTime_LoadOntology + ";" +
          measurement_ComputationTime_FirstReduction + ";" +
          reduction.sizeCode()
      )
      statWriter.flush()
      statWriter.close()

    }

    if (onlyComputeReduction) {
      val csv =
        ont + ";" +
          "Reduction" + ";" +
          "Success" + ";;" +
          measurement_ComputationTime_LoadOntology + ";" +
          measurement_ComputationTime_FirstReduction + ";" +
          measurement_Number_ObjectsInDomain + ";" +
          measurement_Number_ObjectsInReducedDomain + ";;;;;;;;;;;;;;;;;;;;"
      writeResults(csv)
      System.exit(0)
    }

    val startTime = System.currentTimeMillis() - (measurement_ComputationTime_LoadOntology + measurement_ComputationTime_FirstReduction)

    //    val elTBox = EL.extractELTBox(ontology, false, withDisjointnessAxioms).toList
    val elTBox =
      if withDisjointnessAxioms
      then elbotTBox
      else elbotTBox.filter(axiom => !axiom.nestedClassExpressions().toScala(LazyList).contains(OWLNothing))

    //    val measurement_TBoxCardinality = elTBox.size
    //    logger.println(measurement_TBoxCardinality + " EL concept inclusions in the TBox")
    ////    logger.println(elTBox.filter(_.nestedClassExpressions().toScala(LazyList).exists(_.isOWLNothing)).size + " CIs with OWLNothing.")
    //    val measurement_NumberOfDisjointnessAxiomsInTBox =
    //      elTBox.map({
    //        case SubClassOf(_, _, conclusion) =>
    //          if conclusion equals OWLNothing then 1 else 0
    //        case ObjectPropertyDomain(_, _, conclusion) =>
    //          if conclusion equals OWLNothing then 1 else 0
    //        case EquivalentClasses(_, operands) =>
    //          operands.count(_ equals OWLNothing)
    //      }).sum

    var measurement_Number_AxiomsInTBox = 0
    var measurement_Number_DisjointnessAxiomsInTBox = 0
    elTBox.foreach({
      case SubClassOf(_, _, conclusion) =>
        measurement_Number_AxiomsInTBox += 1
        if (conclusion equals OWLNothing)
          measurement_Number_DisjointnessAxiomsInTBox += 1
      case ObjectPropertyDomain(_, _, conclusion) =>
        measurement_Number_AxiomsInTBox += 1
        if (conclusion equals OWLNothing)
          measurement_Number_DisjointnessAxiomsInTBox += 1
      case EquivalentClasses(_, operands) =>
        measurement_Number_AxiomsInTBox += operands.size
        measurement_Number_DisjointnessAxiomsInTBox += operands.count(_ equals OWLNothing)
    })




    val elk = ELK(reduction, elTBox, manager)

    if (!elk.reasoner.isConsistent) {
      writeResults(ont + ";" + whichDisjointnessAxioms + "-" + maxRoleDepth.map(_.toString).getOrElse("INF") + "-" + maxConjunctionSize.map(_.toString).getOrElse("INF") + ";Inconsistent;;;;;;;;;;;;;;;;;;;;;;;;;")
      System.exit(4)
    }



    var (reducedCanonicalModel, _rcmRepresentedBy, _rcmRepresentativeOf, simulationOnRCM) = BitGraph.computeReduction(elk.canonicalModel)
    val n = reducedCanonicalModel.nodes().size
    val measurement_Number_ObjectsInReducedCanonicalModel = n

    //    def rcmRepresentedBy(i: Int): collection.Set[Int | OWLClassExpression] = {
    //      _rcmRepresentedBy(i).unsorted.map(elk.withNumber)
    //    }
    //    def rcmRepresentativeOf(node: Int | OWLClassExpression): Int = {
    //      _rcmRepresentativeOf(elk.numberOf(node))
    //    }
    //    def rcmRepresentsIndividual(i: Int): Boolean = {
    //      rcmRepresentedBy(i).exists({ case _ : Int => true; case _ => false})
    //    }
    //    def rcmRepresentsClassExpression(i: Int): Boolean = {
    //      rcmRepresentedBy(i).exists({ case _ : OWLClassExpression => true; case _ => false})
    //    }

    val nodesWithPredecessor = reducedCanonicalModel.nodes().filter(reducedCanonicalModel.predecessors(_).nonEmpty)
    val nodesWithoutPredecessor = reducedCanonicalModel.nodes() diff nodesWithPredecessor
    val remapToRCM = new Array[Int](n)
    val remapFromRCM = new Array[Int](n)
    var j = 0
    nodesWithPredecessor.foreach(i => {
      remapToRCM(j) = i; remapFromRCM(i) = j; j += 1
    })
    nodesWithoutPredecessor.foreach(i => {
      remapToRCM(j) = i; remapFromRCM(i) = j; j += 1
    })
    val remappedRCM = BitGraph[OWLClass, OWLObjectProperty]()
    val remappedSim = ConcurrentArrayBitBiMap(n, n)
    remappedRCM.nodes().addAll(0 until n)
    remappedRCM.labels().addAll(reducedCanonicalModel.labels())
    remappedRCM.relations().addAll(reducedCanonicalModel.relations())
    (0 until n).foreach(i => {
      //      remappedRCM.addNode(i)
      remappedRCM.addLabels(i, reducedCanonicalModel.labels(remapToRCM(i)))
      reducedCanonicalModel.predecessors(remapToRCM(i)).foreach((r, j) => {
        remappedRCM.addEdge(remapFromRCM(j), r, i)
      })
      (0 until n).foreach(j => {
        if (simulationOnRCM(remapToRCM(i), remapToRCM(j))) {
          remappedSim.add(i, j)
        }
      })
    })

    reducedCanonicalModel = remappedRCM
    simulationOnRCM = remappedSim

    def rcmRepresentedBy(i: Int): collection.Set[Int | OWLClassExpression] = {
      _rcmRepresentedBy(remapToRCM(i)).unsorted.map(elk.withNumber)
    }

    def rcmRepresentativeOf(node: Int | OWLClassExpression): Int = {
      remapFromRCM(_rcmRepresentativeOf(elk.numberOf(node)))
    }

    def rcmRepresentsIndividual(i: Int): Boolean = {
      rcmRepresentedBy(i).exists({ case _: Int => true; case _ => false })
    }

    def rcmRepresentsClassExpression(i: Int): Boolean = {
      rcmRepresentedBy(i).exists({ case _: OWLClassExpression => true; case _ => false })
    }

    val measurement_ComputationTime_Initialization = System.currentTimeMillis() - startTime
    logger.println("The initialization phase took " + formatTime(measurement_ComputationTime_Initialization))

    logger.println(
      "\n\n" +
        "*****************************\n" +
        "* END: Initialization Phase *\n" +
        "*****************************\n"
    )

    /* ************************* */
    /* END: Initialization Phase */
    /* ************************* */










    /* *********************************************************** */
    /* START: Computing all closures of the reduced interpretation */
    /* *********************************************************** */

    logger.println(
      "\n\n" +
        "***************************************************************\n" +
        "* START: Computing all closures of the reduced interpretation *\n" +
        "***************************************************************\n"
    )

    logger.println("Computing closures...")

    def isOccupiedAttribute(ms: collection.BitSet): Boolean = {
      //      ms.exists(m => reducedCanonicalModel.predecessors(m).exists((_,p) => rcmRepresentsIndividual(p)))
      ms.exists(m => reducedCanonicalModel.predecessors(m).nonEmpty)
    }
    //    given valueLogger: ValueLogger = FileValueLogger("ore2015_pool_sample_experiments/closures/" + ont + ".csv", whichDisjointnessAxioms.toString)
    val (closureToGeneratorMap, measurement_ComputationTime_Closures) = measureExecutionTime {
      if (maxRoleDepth.isDefined && (maxRoleDepth.get equals 0))
        mutable.HashMap.empty[collection.BitSet, collection.BitSet]
      else {
//        if (withDisjointnessAxioms)
//          FCbOPar.computeAllClosures(n, PoweringClosureOperator(reducedCanonicalModel, Some(simulationOnRCM), maxConjunctionSize, false, maxRoleDepth.map(_ - 1)))
//            //.subtractOne(reducedCanonicalModel.nodes())
//        else
//          FCbOPar.computeAllClosures(n, PoweringClosureOperator(reducedCanonicalModel, Some(simulationOnRCM), maxConjunctionSize, false, maxRoleDepth.map(_ - 1)), isOccupiedAttribute)
//            //.subtractOne(reducedCanonicalModel.nodes())
        try {
          FCbOPar.computeAllClosures(
            n,
            PoweringClosureOperator(
              reducedCanonicalModel,
              Some(simulationOnRCM),
              if maxConjunctionSize.isDefined then maxConjunctionSize else Some(10000000),
              if maxConjunctionSize.isDefined then false else true,
              maxRoleDepth.map(_ - 1)
            ),
            if withDisjointnessAxioms then (_: collection.BitSet) => true else isOccupiedAttribute
          )
        } catch {
          case e: PoweringTooLargeException =>
            writeResults(ont + ";" + whichDisjointnessAxioms + "-" + maxRoleDepth.map(_.toString).getOrElse("INF") + "-" + maxConjunctionSize.map(_.toString).getOrElse("INF") + ";PoweringTooLarge;;;;;;;;;;;;;;;;;;;;;;;;;")
            System.err.println("\n\n" + e)
            System.exit(5)
            mutable.HashMap.empty[collection.BitSet, collection.BitSet] //only for type inference
        }
      }
    }
    //    valueLogger.close()
    val measurement_Number_Closures = closureToGeneratorMap.keySet.size
    logger.println("FCbO computed " + measurement_Number_Closures + " closures in " + formatTime(measurement_ComputationTime_Closures))
    logger.println()
    logger.reset()

    logger.println(
      "\n\n" +
        "*************************************************************\n" +
        "* END: Computing all closures of the reduced interpretation *\n" +
        "*************************************************************\n"
    )

    /* ********************************************************* */
    /* END: Computing all closures of the reduced interpretation */
    /* ********************************************************* */










    /* ******************************************* */
    /* START: Computing the induced formal context */
    /* ******************************************* */

    logger.println(
      "\n\n" +
        "***********************************************\n" +
        "* START: Computing the induced formal context *\n" +
        "***********************************************\n"
    )

    val (cxt, measurement_ComputationTime_InducedContext) = measureExecutionTime {
      val cxt = InducedFormalContext(reducedCanonicalModel, rcmRepresentsIndividual, closureToGeneratorMap.keySet, whichDisjointnessAxioms)
      cxt.objects
      cxt.activeAttributes
      cxt.activeAttributeIndex
      cxt.bitsActiveAttributes
      cxt.activeIncidenceMatrix
      //      if (withDisjointnessAxioms) {
      //        cxt.unsatisfiableAttributes
      //      }
      cxt
    }
    logger.println("Computing the induced context took " + formatTime(measurement_ComputationTime_InducedContext))

    val measurement_Number_AttributesInInducedContext =
      if (withDisjointnessAxioms)
        cxt.attributes.length
      else
        cxt.activeAttributes.length

    val measurement_Number_ActiveAttributesInInducedContext =
      cxt.activeAttributes.length

    val measurement_Number_FastDisjointnessAxioms =
      measurement_Number_AttributesInInducedContext - measurement_Number_ActiveAttributesInInducedContext

    logger.println("There are " + measurement_Number_AttributesInInducedContext + " attributes")
    logger.println("and there are " + measurement_Number_ActiveAttributesInInducedContext + " occupied attributes.")

    logger.println(
      "\n\n" +
        "*********************************************\n" +
        "* END: Computing the induced formal context *\n" +
        "*********************************************\n"
    )

    /* ***************************************** */
    /* END: Computing the induced formal context */
    /* ***************************************** */










    /* ********************************************* */
    /* START: Computation of background implications */
    /* ********************************************* */

    logger.println(
      "\n\n" +
        "*************************************************\n" +
        "* START: Computation of background implications *\n" +
        "*************************************************\n"
    )

    val startBackgroundImplications = System.currentTimeMillis()
    // TODO: Split the additional attributes into
    // TODO: (1) concept names
    // TODO: (2) existential restrictions in a premise
    // TODO: (3) existential restrictions in a conclusion

    val _additionalAttributes = mutable.HashSet[OWLClass | OWLObjectSomeValuesFrom]()
    val activeAttributesInContext = cxt.activeAttributes.toSet
    //    val f: PartialFunction[OWLClassExpression, OWLClass | OWLObjectSomeValuesFrom] = {
    ////       case c@Class(_) if !c.isOWLThing && !activeAttributesInContext.contains(c) => c
    //      case c@Class(_) if !c.isOWLThing => c
    //      case c@ObjectSomeValuesFrom(_, _) => c
    //    }
    val existentialRestrictionsInPremises = mutable.HashSet[OWLObjectSomeValuesFrom]()
    val existentialRestrictionsInConclusions = mutable.HashSet[OWLObjectSomeValuesFrom]()
    //    val g: PartialFunction[OWLClassExpression, OWLObjectSomeValuesFrom] = {
    //      case c@ObjectSomeValuesFrom(_, _) => c
    //    }
    def processPremise(premise: OWLClassExpression): Unit = {
      premise.conjunctSet().toScala(LazyList).foreach({
        case c@Class(_) if !c.isOWLThing && !activeAttributesInContext.contains(c) =>
          _additionalAttributes.addOne(c)
        case c@ObjectSomeValuesFrom(_, _) =>
          _additionalAttributes.addOne(c)
          existentialRestrictionsInPremises.addOne(c)
        case _ => {}
      })
    }
    def processConclusion(conclusion: OWLClassExpression): Unit = {
      conclusion.conjunctSet().toScala(LazyList).foreach({
        case c@Class(_) if !c.isOWLThing && !activeAttributesInContext.contains(c) =>
          _additionalAttributes.addOne(c)
        case c@ObjectSomeValuesFrom(_, _) =>
          _additionalAttributes.addOne(c)
          existentialRestrictionsInConclusions.addOne(c)
        case _ => {}
      })
    }
    elTBox.foreach({
      case SubClassOf(_, premise, conclusion) => {
        //        _additionalAttributes.addAll(premise.conjunctSet().toScala(LazyList).collect(f))
        //        _additionalAttributes.addAll(conclusion.conjunctSet().toScala(LazyList).collect(f))
        //        existentialRestrictionsInConclusions.addAll(conclusion.conjunctSet().toScala(LazyList).collect(g))
        processPremise(premise)
        processConclusion(conclusion)
      }
      case ObjectPropertyDomain(_, property@ObjectProperty(_), conclusion) => {
        //        _additionalAttributes.add(ObjectSomeValuesFrom(property, OWLThing))
        //        _additionalAttributes.addAll(conclusion.conjunctSet().toScala(LazyList).collect(f))
        //        existentialRestrictionsInConclusions.addAll(conclusion.conjunctSet().toScala(LazyList).collect(g))
        processPremise(ObjectSomeValuesFrom(property, OWLThing))
        processConclusion(conclusion)
      }
      case EquivalentClasses(_, operands) => {
        operands.foreach(op => {
          //          _additionalAttributes.addAll(op.conjunctSet().toScala(LazyList).collect(f))
          //          existentialRestrictionsInConclusions.addAll(op.conjunctSet().toScala(LazyList).collect(g))
          processPremise(op)
          processConclusion(op)
        })
      }
    })

    // TODO: build the TBox saturation only from the fillers in conclusions
    //    elk.insertIntoCanonicalModel(_additionalAttributes.collect({
    ////    elk.insertIntoCanonicalModel(existentialRestrictionsInConclusions.collect({
    //      case ObjectSomeValuesFrom(_, filler) => filler
    //    }))
    val fillersInConclusions = existentialRestrictionsInConclusions.map({ case ObjectSomeValuesFrom(_, filler) => filler })
    elk.insertIntoCanonicalModel(fillersInConclusions)

    val (reducedCanonicalModel2, _rcmRepresentedBy2, _rcmRepresentativeOf2, _) = BitGraph.computeReduction(elk.canonicalModel)

    def rcmRepresentedBy2(i: Int): collection.Set[Int | OWLClassExpression] = {
      _rcmRepresentedBy2(i).unsorted.map(elk.withNumber)
    }

    def rcmRepresentativeOf2(node: Int | OWLClassExpression): Int = {
      _rcmRepresentativeOf2(elk.numberOf(node))
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

    //    insertIntoTBoxSaturation(reducedCanonicalModel2.nodes().filter(rcmRepresentsClassExpression2))
    insertIntoTBoxSaturation(fillersInConclusions.map(rcmRepresentativeOf2))

    //    val clop = PoweringClosureOperator(tboxSaturation)
    //    val clop = PoweringClosureOperator_Incremental(tboxSaturation)
    //    val clop = PoweringClosureOperator2_CachedValues(tboxSaturation)
    val poweringSimulator = PoweringSimulator(reducedCanonicalModel, tboxSaturation, Some(simulationOnRCM), None, true, maxRoleDepth.map(_ - 1))

    logger.println("Building powering simulator cache...")
    val cachedPoweringSimulator = collection.concurrent.TrieMap[collection.BitSet, collection.BitSet]()
    closureToGeneratorMap.foreachPar((closure, generator) => {
      cachedPoweringSimulator(closure) = poweringSimulator(generator)
      logger.tick()
    })
    logger.reset()

    def simulates(node1: collection.BitSet | OWLClassExpression, node2: OWLClassExpression): Boolean = {
      node1 match
        case node1: collection.BitSet =>
          node1.forall(i => {
            rcmRepresentedBy(i).head match
              case j: Int =>
                elk.types(elk.individualFor(j)) contains elk.representativeOf(node2)
              case c: OWLClassExpression =>
                elk.subsumers(elk.representativeOf(c)) contains elk.representativeOf(node2)
          })
        case node1: OWLClassExpression =>
          elk.subsumers(elk.representativeOf(node1)) contains elk.representativeOf(node2)
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

    val extendedAttributeSet: Array[Mx] = cxt.activeAttributes ++ _additionalAttributes
    val extendedAttributeIndex = mutable.HashMap[Mx, Int]()
    (0 until extendedAttributeSet.length).foreach(i => {
      extendedAttributeIndex.update(extendedAttributeSet(i), i)
    })

    logger.reset()
    logger.println("Enumerating background implications...")
    val _backgroundImplications = ConcurrentLinkedQueue[BitImplication]()

    if (withDisjointnessAxioms)
      _backgroundImplications.add(BitSet(0) -> BitSet.fromSpecific(1 until extendedAttributeSet.length))
      logger.tick()

    extendedAttributeSet.foreachPar({
      case pair1 @ ObjectSomeValuesFromMMSC(property1, mmsc1) => {
        val cs = extendedAttributeSet.collect({
          // (4)  {(r,X)} → {(r,Y)}  if  X⊆Y
          case pair2 @ ObjectSomeValuesFromMMSC(property2, mmsc2)
            if (property1 equals property2)
              && (mmsc1 subsetOf mmsc2) => extendedAttributeIndex(pair2)

          // (1)  {(r,X)} → {∃r.C}   if  simulates(X, C) == true
          case exr2 @ ObjectSomeValuesFrom(property2 @ ObjectProperty(_), filler2)
            if (property1 equals property2)
              && (existentialRestrictionsInPremises contains exr2)
              && simulates(mmsc1, filler2) => extendedAttributeIndex(exr2)
        })
        if (cs.nonEmpty)
          _backgroundImplications.add(BitSet(extendedAttributeIndex(pair1)) -> BitSet.fromSpecific(cs))
          logger.tick()
      }
      case exr1 @ ObjectSomeValuesFrom(property1 @ ObjectProperty(_), filler1) if existentialRestrictionsInConclusions contains exr1 => {
        val cs = extendedAttributeSet.collect({
          // (5)   {∃r.C} → {(r,X)}  if  C simulates X, i.e., there is a simulation from (𝔓(𝓘),X) to (𝓘_𝓣,C)
          case pair2 @ ObjectSomeValuesFromMMSC(property2, mmsc2)
            if (property1 equals property2)
              // && (poweringSimulator(closureToGeneratorMap(mmsc2)) contains rcmRepresentativeOf2(filler1)) => extendedAttributeIndex(pair2)
              && (cachedPoweringSimulator(mmsc2) contains rcmRepresentativeOf2(filler1)) => extendedAttributeIndex(pair2)

          // (2)   {∃r.C} → {∃r.D}   if  simulates(C, D) == true
          case exr2 @ ObjectSomeValuesFrom(property2 @ ObjectProperty(_), filler2)
            if (property1 equals property2)
              && (existentialRestrictionsInPremises contains exr2)
              && simulates(filler1, filler2) => extendedAttributeIndex(exr2)
        })
        if (cs.nonEmpty)
          _backgroundImplications.add(BitSet(extendedAttributeIndex(exr1)) -> BitSet.fromSpecific(cs))
          logger.tick()
      }
      case _ => {}
    })

    elk.reasoner.dispose()

    // (3)  Conj(C) → Conj(D)  if  C⊑D ∈ 𝓣
    elTBox.foreach({
      case SubClassOf(_, premise, conclusion) => {
        val p = premise.conjunctSet().toScala(LazyList).filter(!_.isOWLThing).map({ case x : (OWLClass | OWLObjectSomeValuesFrom) => extendedAttributeIndex(x) }).to(BitSet)
        val c = conclusion.conjunctSet().toScala(LazyList).filter(!_.isOWLThing).map({ case x : (OWLClass | OWLObjectSomeValuesFrom) => extendedAttributeIndex(x) }).to(BitSet)
        if (c.nonEmpty)
          _backgroundImplications.add(p -> c)
          logger.tick()
      }
      case ObjectPropertyDomain(_, property@ObjectProperty(_), conclusion) => {
        val p = BitSet(extendedAttributeIndex(ObjectSomeValuesFrom(property, OWLThing)))
        val c = conclusion.conjunctSet().toScala(LazyList).filter(!_.isOWLThing).map({ case x: (OWLClass | OWLObjectSomeValuesFrom) => extendedAttributeIndex(x) }).to(BitSet)
        if (c.nonEmpty)
          _backgroundImplications.add(p -> c)
          logger.tick()
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
            _backgroundImplications.add(ps(j) -> ps(j+1))
            logger.tick()
        })
        if (ps(0).nonEmpty)
          _backgroundImplications.add(ps(n-1) -> ps(0))
          logger.tick()
      }
    })


    val backgroundImplications = mutable.HashSet[BitImplication]()
    val it = _backgroundImplications.iterator()
    while (it.hasNext) {
      backgroundImplications.addOne(it.next())
    }
    _backgroundImplications.clear()
    //    val backgroundImplications = _backgroundImplications.asScala
    //    val numberBackgroundImplications = AtomicInteger(0)

    logger.println()
    val measurement_ComputationTime_BackgroundImplications = System.currentTimeMillis() - startBackgroundImplications
    logger.println("Computing the background implications took " + formatTime(measurement_ComputationTime_BackgroundImplications))
    val measurement_Number_BackgroundImplications = backgroundImplications.size
    logger.println(measurement_Number_BackgroundImplications + " background implications computed.")
    //    logger.println("There are " + backgroundImplications.filter(_._1.isEmpty).size + " background implications with empty premise.")
    //    logger.println("There are " + backgroundImplications.filter(_._2.isEmpty).size + " background implications with empty conclusion.")
    logger.reset()

    logger.println(
      "\n\n" +
        "***********************************************\n" +
        "* END: Computation of background implications *\n" +
        "***********************************************\n"
    )

    /* ******************************************* */
    /* END: Computation of background implications */
    /* ******************************************* */










    /* *********************************** */
    /* START: Computing the canonical base */
    /* *********************************** */

    logger.println(
      "\n\n" +
        "***************************************\n" +
        "* START: Computing the canonical base *\n" +
        "***************************************\n"
    )

    //    val (cbase, measurement_ComputationTime_CanonicalBase) = measureExecutionTime {
    //      if (withDisjointnessAxioms)
    //        LinCbO.computeCanonicalBase(cxt)
    ////        mutable.ArrayBuffer.empty
    //      else
    //        LinCbO.computeCanonicalBase(cxt, cxt.commonObjects(_).nonEmpty)
    //    }
    //    logger.println("Computing the canonical base with LinCbO/Scala took " + formatTime(measurement_ComputationTime_CanonicalBase))
    //    val measurement_Number_ImplicationsInCanonicalBase = cbase.size
    //    logger.println(measurement_Number_ImplicationsInCanonicalBase + " implications in canonical base")
    //    val measurement_Number_ImplicationsInFinalBase =
    //      if canonicalDisjointnessAxioms
    //      then measurement_Number_ImplicationsInCanonicalBase
    //      else measurement_Number_ImplicationsInCanonicalBase + measurement_Number_FastDisjointnessAxioms
    //    //    logger.println(cbase.filter((_, ys) => !ys.contains(0)).size + " implications in canonical base that are no disjointness axiom")
    //    //    logger.println(cbase.filter((xs, _) => xs.contains(0)).size + " implications contain OWLNothing in the premise")
    //    logger.println()
    //
    //
    //
    ////    val (base, measurement_ComputationTime_CanonicalBaseJKK) =
    ////      if (withDisjointnessAxioms)
    ////        LinCbO_JKK.computeCanonicalBase(cxt, ont)
    ////      else
    ////        (Seq.empty, -1l)
    ////    logger.println("Computing the canonical base with LinCbO/C++ took " + formatTime(measurement_ComputationTime_CanonicalBaseJKK))
    ////    val measurement_Number_ImplicationsInCanonicalBaseJKK = base.size
    ////    logger.println(measurement_Number_ImplicationsInCanonicalBaseJKK + " implications in canonical base")
    ////    logger.println()
    //    val (measurement_Number_ImplicationsInCanonicalBaseJKK, measurement_ComputationTime_CanonicalBaseJKK) =
    //      if (withDisjointnessAxioms)
    //        LinCbO_JKK.computeCanonicalBase(cxt, ont)
    //      else
    //        (-1, -1l)
    //    logger.println("Computing the canonical base with LinCbO/C++ took " + formatTime(measurement_ComputationTime_CanonicalBaseJKK))
    //    logger.println(measurement_Number_ImplicationsInCanonicalBaseJKK + " implications in canonical base")
    //    logger.println()

    val measurement_ComputationTime_CanonicalBaseJKK = ""
    val measurement_ComputationTime_CanonicalBase = ""
    val measurement_Number_ImplicationsInFinalBase = ""
    val measurement_Number_ImplicationsInCanonicalBase = ""
    val measurement_Number_ImplicationsInCanonicalBaseJKK = ""

    val (cbaseBack, measurement_ComputationTime_RelativeCanonicalBase) = measureExecutionTime {
      if (withDisjointnessAxioms)
        LinCbO_WithPruning_WithBackgroundImplications.computeCanonicalBase(cxt, backgroundImplications, extendedAttributeSet.length)
      else // TODO: inclusion ideal could also check if at least n objects satisfy the premise (or a percentage of all objects), and/or if the premise contains at most k attributes
        LinCbO_WithPruning_WithBackgroundImplications.computeCanonicalBase(cxt, backgroundImplications, extendedAttributeSet.length,
          if maxConjunctionSize.isEmpty
          then ms => cxt.commonObjects(ms intersect cxt.bitsActiveAttributes).nonEmpty
          else ms => (ms.size <= maxConjunctionSize.get) && cxt.commonObjects(ms intersect cxt.bitsActiveAttributes).nonEmpty)
    }
    logger.println("Computing the relative canonical base with BLinCbO/Scala took " + formatTime(measurement_ComputationTime_RelativeCanonicalBase))
    val measurement_Number_ImplicationsInRelativeCanonicalBase = cbaseBack.size
    logger.println(measurement_Number_ImplicationsInRelativeCanonicalBase + " implications in relative canonical base")
    val measurement_Number_ImplicationsInFinalRelativeBase =
      if canonicalDisjointnessAxioms
      then measurement_Number_ImplicationsInRelativeCanonicalBase
      else measurement_Number_ImplicationsInRelativeCanonicalBase + measurement_Number_FastDisjointnessAxioms
    //    val measurement_Number_ImplicationsInRelativeCanonicalBase =
    //      measurement_Number_ImplicationsInRelativeCanonicalBaseWithoutSingletonDisjointnessAxioms + measurement_Number_SingletonDisjointnessAxiomsInCanonicalBase
    //      logger.println(cbaseBack.filter((_, ys) => !ys.contains(0)).size + " implications in relative canonical base that are no disjointness axiom")
    //      logger.println(cbaseBack.filter((xs, _) => xs.contains(0)).size + " implications contain OWLNothing in the premise")
    logger.println()

    logger.println(
      "\n\n" +
        "*************************************\n" +
        "* END: Computing the canonical base *\n" +
        "*************************************\n"
    )

    /* ********************************* */
    /* END: Computing the canonical base */
    /* ********************************* */





    //    def printlnImp(implication: BitImplication): Unit = {
    //      val p = implication._1
    //      val c = implication._2
    //      var str = ""
    //      str += (0 until extendedAttributeSet.length).foldLeft("")((s,i) => s + (if (i == cxt.occupiedAttributes.length) "|" else "") + (if (p(i)) "X" else "."))
    //      str += " ⟶ "
    //      str += (0 until extendedAttributeSet.length).foldLeft("")((s,i) => s + (if (i == cxt.occupiedAttributes.length) "|" else "") + (if (c(i)) "X" else "."))
    //      logger.println(str)
    //    }

    //    logger.println()
    //    logger.println(measurement_NumberOfBackgroundImplications + " background implications.")
    ////    backgroundImplications.foreach(printlnImp)
    //    logger.println()

    val measurement_ComputationTime_Total = System.currentTimeMillis() - startTime


    logger.println()
    logger.println("Dataset ............................................................. " + ont)
    logger.println("Disjointness axioms ................................................. " + whichDisjointnessAxioms)
    logger.println("Maximal role depth .................................................. " + maxRoleDepth)
    logger.println("Maximal conjunction size ............................................ " + maxConjunctionSize)
    logger.println("Total computation time .............................................. " + formatTime(measurement_ComputationTime_Total))
    logger.println("Time for loading ontology ........................................... " + formatTime(measurement_ComputationTime_LoadOntology))
    logger.println("Time for first reduction ............................................ " + formatTime(measurement_ComputationTime_FirstReduction))
    logger.println("Number of objects in domain ......................................... " + measurement_Number_ObjectsInDomain)
    logger.println("Number of objects in reduced domain ................................. " + measurement_Number_ObjectsInReducedDomain)
    logger.println("Number of objects in reduced canonical model ........................ " + measurement_Number_ObjectsInReducedCanonicalModel)
    logger.println("Time for initialization ............................................. " + formatTime(measurement_ComputationTime_Initialization))
    logger.println("Time for computing all closures with FCbO ........................... " + formatTime(measurement_ComputationTime_Closures))
    logger.println("Number of closures .................................................. " + measurement_Number_Closures)
    logger.println("Time for computing the induced context .............................. " + formatTime(measurement_ComputationTime_InducedContext))
    logger.println("Number of attributes in induced context ............................. " + measurement_Number_AttributesInInducedContext)
    logger.println("Number of active attributes in induced context ...................... " + measurement_Number_ActiveAttributesInInducedContext)
    logger.println("Number of axioms in TBox ............................................ " + measurement_Number_AxiomsInTBox)
    logger.println("Number of disjointness axioms in TBox ............................... " + measurement_Number_DisjointnessAxiomsInTBox)
    logger.println("Time for computing the background implications ...................... " + formatTime(measurement_ComputationTime_BackgroundImplications))
    logger.println("Number of background implications ................................... " + measurement_Number_BackgroundImplications)
    //    logger.println("Time for computing the canonical base with LinCbO/C++ ............... " + formatTime(measurement_ComputationTime_CanonicalBaseJKK))
    //    logger.println("Time for computing the canonical base with LinCbO/Scala ............. " + formatTime(measurement_ComputationTime_CanonicalBase))
    logger.println("Time for computing the relative canonical base with BLinCbO/Scala ... " + formatTime(measurement_ComputationTime_RelativeCanonicalBase))
    //    logger.println("Number of implications in final base ................................ " + measurement_Number_ImplicationsInFinalBase)
    //    logger.println("  among which are no fast disjointness axiom (LinCbO/Scala) ......... " + measurement_Number_ImplicationsInCanonicalBase)
    //    logger.println("  among which are no fast disjointness axiom (LinCbO/C++) ........... " + measurement_Number_ImplicationsInCanonicalBaseJKK)
    logger.println("Number of implications in final relative base ....................... " + measurement_Number_ImplicationsInFinalRelativeBase)
    logger.println("  among which are fast disjointness axioms .......................... " + measurement_Number_FastDisjointnessAxioms)
    logger.println("  among which are no fast disjointness axiom (BLinCbO/Scala) ........ " + measurement_Number_ImplicationsInRelativeCanonicalBase)
    logger.println()
    logger.println()


    val csv =
      ont + ";" +
        whichDisjointnessAxioms + "-" + maxRoleDepth.map(_.toString).getOrElse("INF") + "-" + maxConjunctionSize.map(_.toString).getOrElse("INF") + ";" +
        "Success" + ";" +
        measurement_ComputationTime_Total + ";" +
        measurement_ComputationTime_LoadOntology + ";" +
        measurement_ComputationTime_FirstReduction + ";" +
        measurement_Number_ObjectsInDomain + ";" +
        measurement_Number_ObjectsInReducedDomain + ";" +
        measurement_Number_ObjectsInReducedCanonicalModel + ";" +
        measurement_ComputationTime_Initialization + ";" +
        measurement_ComputationTime_Closures + ";" +
        measurement_Number_Closures + ";" +
        measurement_ComputationTime_InducedContext + ";" +
        measurement_Number_AttributesInInducedContext + ";" +
        measurement_Number_ActiveAttributesInInducedContext + ";" +
        measurement_Number_AxiomsInTBox + ";" +
        measurement_Number_DisjointnessAxiomsInTBox + ";" +
        measurement_ComputationTime_BackgroundImplications + ";" +
        measurement_Number_BackgroundImplications + ";" +
        measurement_ComputationTime_CanonicalBaseJKK + ";" +
        measurement_ComputationTime_CanonicalBase + ";" +
        measurement_ComputationTime_RelativeCanonicalBase + ";" +
        measurement_Number_ImplicationsInFinalBase + ";" +
        measurement_Number_ImplicationsInCanonicalBase + ";" +
        measurement_Number_ImplicationsInCanonicalBaseJKK + ";" +
        measurement_Number_FastDisjointnessAxioms + ";" +
        measurement_Number_ImplicationsInFinalRelativeBase + ";" +
        measurement_Number_ImplicationsInRelativeCanonicalBase

    writeResults(csv)

  }

}
