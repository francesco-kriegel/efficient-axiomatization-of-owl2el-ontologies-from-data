package de.tu_dresden.inf.lat
package axiomatization

import org.phenoscape.scowl.*

import scala.annotation.tailrec
import scala.jdk.StreamConverters.*


object Evaluation {

//  private enum Mode:
//    case Reduction, NoDisjointnessAxioms, FastDisjointnessAxioms, CanonicalDisjointnessAxioms
//
//  private object Mode {
//    def apply(string: String): Mode = {
//      string match {
//        case "Reduction" => Reduction
//        case "None" => NoDisjointnessAxioms
//        case "Fast" => FastDisjointnessAxioms
//        case "Canonical" => CanonicalDisjointnessAxioms
//        case _ => throw MatchError(string)
//      }
//    }
//  }

  private sealed abstract class Mode {
    override def toString: String = this match {
      case Reduction() => "Reduction"
      case NoDisjointnessAxioms(roleDepthBound, conjunctionSizeLimit) => "None-" + roleDepthBound.getOrElse("INF") + "-" + conjunctionSizeLimit.getOrElse("INF")
      case FastDisjointnessAxioms(roleDepthBound, conjunctionSizeLimit) => "Fast-" + roleDepthBound.getOrElse("INF") + "-" + conjunctionSizeLimit.getOrElse("INF")
      case CanonicalDisjointnessAxioms(roleDepthBound, conjunctionSizeLimit) => "Canonical-" + roleDepthBound.getOrElse("INF") + "-" + conjunctionSizeLimit.getOrElse("INF")
    }
  }
  private case class Reduction() extends Mode
  private case class NoDisjointnessAxioms(roleDepthBound: Option[Int], conjunctionSizeLimit: Option[Int]) extends Mode
  private case class FastDisjointnessAxioms(roleDepthBound: Option[Int], conjunctionSizeLimit: Option[Int]) extends Mode
  private case class CanonicalDisjointnessAxioms(roleDepthBound: Option[Int], conjunctionSizeLimit: Option[Int]) extends Mode

  private object Mode {
    private val NoneRegex = "None-(\\d+|INF)-(\\d+|INF)".r
    private val FastRegex = "Fast-(\\d+|INF)-(\\d+|INF)".r
    private val CanonicalRegex = "Canonical-(\\d+|INF)-(\\d+|INF)".r
    private def parse(string: String): Option[Int] = {
      if string equals "INF" then None else Some(string.toInt)
    }
    def apply(string: String): Mode = {
      string match {
        case "Reduction" => Reduction()
        case "Reduction-INF-INF" => Reduction()
        case NoneRegex(roleDepthBound, conjunctionSizeLimit) => NoDisjointnessAxioms(parse(roleDepthBound), parse(conjunctionSizeLimit))
        case FastRegex(roleDepthBound, conjunctionSizeLimit) => FastDisjointnessAxioms(parse(roleDepthBound), parse(conjunctionSizeLimit))
        case CanonicalRegex(roleDepthBound, conjunctionSizeLimit) => CanonicalDisjointnessAxioms(parse(roleDepthBound), parse(conjunctionSizeLimit))
        case _ => throw MatchError(string)
      }
    }
  }

  private val Mode_Reduction = Reduction()

  private val Mode_None_0_32 = NoDisjointnessAxioms(Some(0), Some(32))
  private val Mode_None_1_8 = NoDisjointnessAxioms(Some(1), Some(8))
  private val Mode_None_1_32 = NoDisjointnessAxioms(Some(1), Some(32))
  private val Mode_None_2_32 = NoDisjointnessAxioms(Some(2), Some(32))
  private val Mode_None_INF_32 = NoDisjointnessAxioms(None, Some(32))
  private val Mode_None_INF_INF = NoDisjointnessAxioms(None, None)

  private val Mode_Fast_0_32 = FastDisjointnessAxioms(Some(0), Some(32))
  private val Mode_Fast_1_8 = FastDisjointnessAxioms(Some(1), Some(8))
  private val Mode_Fast_1_32 = FastDisjointnessAxioms(Some(1), Some(32))
  private val Mode_Fast_2_32 = FastDisjointnessAxioms(Some(2), Some(32))
  private val Mode_Fast_INF_32 = FastDisjointnessAxioms(None, Some(32))
  private val Mode_Fast_INF_INF = FastDisjointnessAxioms(None, None)

  private val Mode_Canonical_0_32 = CanonicalDisjointnessAxioms(Some(0), Some(32))
  private val Mode_Canonical_1_8 = CanonicalDisjointnessAxioms(Some(1), Some(8))
  private val Mode_Canonical_1_32 = CanonicalDisjointnessAxioms(Some(1), Some(32))
  private val Mode_Canonical_2_32 = CanonicalDisjointnessAxioms(Some(2), Some(32))
  private val Mode_Canonical_INF_32 = CanonicalDisjointnessAxioms(None, Some(32))
  private val Mode_Canonical_INF_INF = CanonicalDisjointnessAxioms(None, None)

  private val Mode_values = collection.immutable.List(
    Mode_Reduction,
    Mode_None_0_32, Mode_None_1_8, Mode_None_1_32, Mode_None_2_32, Mode_None_INF_32, Mode_None_INF_INF,
    Mode_Fast_0_32, Mode_Fast_1_8, Mode_Fast_1_32, Mode_Fast_2_32, Mode_Fast_INF_32, Mode_Fast_INF_INF,
    Mode_Canonical_0_32, Mode_Canonical_1_8, Mode_Canonical_1_32, Mode_Canonical_2_32, Mode_Canonical_INF_32, Mode_Canonical_INF_INF
  )

  private val Mode_parents_first = collection.immutable.Map(
    Mode_Reduction -> collection.immutable.List(),

    Mode_None_0_32 -> collection.immutable.List(),
    Mode_None_1_8 -> collection.immutable.List(),
    Mode_None_1_32 -> collection.immutable.List(),
    Mode_None_2_32 -> collection.immutable.List(Mode_None_1_32),
    Mode_None_INF_32 -> collection.immutable.List(Mode_None_2_32),
    Mode_None_INF_INF -> collection.immutable.List(Mode_None_INF_32),

    Mode_Fast_0_32 -> collection.immutable.List(), // This is correct.
    Mode_Fast_1_8 -> collection.immutable.List(Mode_None_1_8),
    Mode_Fast_1_32 -> collection.immutable.List(Mode_None_1_32),
    Mode_Fast_2_32 -> collection.immutable.List(Mode_None_2_32, Mode_Fast_1_32),
    Mode_Fast_INF_32 -> collection.immutable.List(Mode_None_INF_32, Mode_Fast_2_32),
    Mode_Fast_INF_INF -> collection.immutable.List(Mode_None_INF_INF, Mode_Fast_INF_32),

    Mode_Canonical_0_32 -> collection.immutable.List(Mode_None_0_32), // This is correct.
    Mode_Canonical_1_8 -> collection.immutable.List(Mode_Fast_1_8),
    Mode_Canonical_1_32 -> collection.immutable.List(Mode_Fast_1_32),
    Mode_Canonical_2_32 -> collection.immutable.List(Mode_Fast_2_32, Mode_Canonical_1_32),
    Mode_Canonical_INF_32 -> collection.immutable.List(Mode_Fast_INF_32, Mode_Canonical_2_32),
    Mode_Canonical_INF_INF -> collection.immutable.List(Mode_Fast_INF_INF, Mode_Canonical_INF_32)
  )

  private val Mode_parents_second = collection.immutable.Map(
    Mode_Reduction -> collection.immutable.List(),

    Mode_None_0_32 -> collection.immutable.List(),
    Mode_None_1_8 -> collection.immutable.List(),
    Mode_None_1_32 -> collection.immutable.List(Mode_None_1_8, Mode_None_0_32),
    Mode_None_2_32 -> collection.immutable.List(Mode_None_1_32),
    Mode_None_INF_32 -> collection.immutable.List(Mode_None_2_32),
    Mode_None_INF_INF -> collection.immutable.List(Mode_None_INF_32),

    Mode_Fast_0_32 -> collection.immutable.List(Mode_None_0_32),
    Mode_Fast_1_8 -> collection.immutable.List(Mode_None_1_8),
    Mode_Fast_1_32 -> collection.immutable.List(Mode_None_1_32, Mode_Fast_0_32, Mode_Fast_1_8),
    Mode_Fast_2_32 -> collection.immutable.List(Mode_None_2_32, Mode_Fast_1_32),
    Mode_Fast_INF_32 -> collection.immutable.List(Mode_None_INF_32, Mode_Fast_2_32),
    Mode_Fast_INF_INF -> collection.immutable.List(Mode_None_INF_INF, Mode_Fast_INF_32),

    Mode_Canonical_0_32 -> collection.immutable.List(Mode_Fast_0_32),
    Mode_Canonical_1_8 -> collection.immutable.List(Mode_Fast_1_8),
    Mode_Canonical_1_32 -> collection.immutable.List(Mode_Fast_1_32, Mode_Canonical_0_32, Mode_Canonical_1_8),
    Mode_Canonical_2_32 -> collection.immutable.List(Mode_Fast_2_32, Mode_Canonical_1_32),
    Mode_Canonical_INF_32 -> collection.immutable.List(Mode_Fast_INF_32, Mode_Canonical_2_32),
    Mode_Canonical_INF_INF -> collection.immutable.List(Mode_Fast_INF_INF, Mode_Canonical_INF_32)
  )

  private val firstExperiments = collection.immutable.Set(443,
                                                          1117,
                                                          1370,
                                                          1767,
                                                          1833,
                                                          2253,
                                                          2719,
                                                          3313,
                                                          3737,
                                                          4033,
                                                          4242,
                                                          4410,
                                                          4557,
                                                          4572,
                                                          4639,
                                                          5241,
                                                          5324,
                                                          5544,
                                                          5602,
                                                          5755,
                                                          5841,
                                                          6233,
                                                          6443,
                                                          6478,
                                                          7389,
                                                          7680,
                                                          7858,
                                                          9386,
                                                          9792,
                                                          10750,
                                                          11085,
                                                          11287,
                                                          11464,
                                                          11597,
                                                          11703,
                                                          12097,
                                                          12182,
                                                          12932,
                                                          13106,
                                                          13336,
                                                          13413,
                                                          13690,
                                                          13919,
                                                          13949,
                                                          14909,
                                                          15280,
                                                          15288,
                                                          15791,
                                                          15921,
                                                          16105,
                                                          16509,
                                                          16853)

  private def Mode_parents(ont: Int) =
    if firstExperiments contains ont
    then Mode_parents_first
    else Mode_parents_second

  private sealed abstract class Status {
    override def toString: String = this match {
      case Success() => "Success"
      case Timeout(minutes) => s"Timeout(${minutes}m)"
      case OutOfMemory(gigabytes) => s"OutOfMemory(${gigabytes}g)"
      case Inconsistent() => "Inconsistent"
      case PoweringTooLarge() => "PoweringTooLarge"
      case Error(number) => s"Error(${number})"
    }
  }
  private case class Success() extends Status
  private case class Timeout(minutes: Int) extends Status
  private case class OutOfMemory(gigabytes: Int) extends Status
  private case class Inconsistent() extends Status
  private case class PoweringTooLarge() extends Status
  private case class Error(number: Int) extends Status

  private object Status {
    private val TimeoutRegex = "Timeout\\((\\d+)(s|m|h|d)\\)".r
    private val OutOfMemoryRegex = "OutOfMemory\\((\\d+)g\\)".r
    private val ErrorRegex = "Error\\((-?\\d+)\\)".r

    def apply(string: String): Status = {
      string match {
        case "Success" => Success()
        case TimeoutRegex(n, unit) =>
          unit match {
            case "s" => throw IllegalArgumentException()
            case "m" => Timeout(n.toInt)
            case "h" => Timeout(n.toInt * 60)
            case "d" => Timeout(n.toInt * 60 * 24)
          }
        case OutOfMemoryRegex(n) => OutOfMemory(n.toInt)
        case "Inconsistent" => Inconsistent()
        case "PoweringTooLarge" => PoweringTooLarge()
        case ErrorRegex(n) => Error(n.toInt)
        case _ => throw MatchError(string)
      }
    }
  }

  private class Result(private val csl: String) {
    private val _values = csl.split(';')
    private val values = Array.tabulate(28)(i => if i < _values.length && _values(i) != "" then Some(_values(i)) else None)

    val ont: Int = values(0).get.substring(8).toInt
    val mode: Mode = values(1).map(Mode(_)).get
    val status: Status = values(2).map(Status(_)).get
    val ComputationTime_Total: Option[Long] = values(3).map(_.toLong)
    val ComputationTime_LoadOntology: Option[Long] = reductionStatisticsMap.get(ont).map(_.ComputationTime_LoadOntology) // values(4).map(_.toLong)
    val ComputationTime_FirstReduction: Option[Long] = reductionStatisticsMap.get(ont).map(_.ComputationTime_FirstReduction) // values(5).map(_.toLong)
//    val Number_ObjectsInDomain: Option[Int] = reductionStatisticsMap.get(ont).map(_.Number_ObjectsInDomain) // values(6).map(_.toInt)
    val Number_ObjectsInDomain: Int = getInputStatistics(ont).Number_ObjectsInDomain
    val Number_ObjectsInReducedDomain: Option[Int] = reductionStatisticsMap.get(ont).map(_.Number_ObjectsInReducedDomain) // values(7).map(_.toInt)
    val Number_ObjectsInReducedCanonicalModel: Option[Int] = values(8).map(_.toInt)
    val ComputationTime_Initialization: Option[Long] = values(9).map(_.toLong)
    val ComputationTime_Closures: Option[Long] = values(10).map(_.toLong)
    val Number_Closures: Option[Int] = values(11).map(_.toInt)
    val ComputationTime_InducedContext: Option[Long] = values(12).map(_.toLong)
    val Number_AttributesInInducedContext: Option[Int] = values(13).map(_.toInt)
    val Number_ActiveAttributesInInducedContext: Option[Int] = values(14).map(_.toInt)
    val Number_AxiomsInTBox: Option[Int] = values(15).map(_.toInt)
    val Number_DisjointnessAxiomsInTBox: Option[Int] = values(16).map(_.toInt)
    val ComputationTime_BackgroundImplications: Option[Long] = values(17).map(_.toLong)
    val Number_BackgroundImplications: Option[Int] = values(18).map(_.toInt)
    val ComputationTime_CanonicalBaseJKK: Option[Long] = values(19).map(_.toLong)
    val ComputationTime_CanonicalBase: Option[Long] = values(20).map(_.toLong)
    val ComputationTime_RelativeCanonicalBase: Option[Long] = values(21).map(_.toLong)
    val Number_ImplicationsInFinalBase: Option[Int] = values(22).map(_.toInt)
    val Number_ImplicationsInCanonicalBase: Option[Int] = values(23).map(_.toInt)
    val Number_ImplicationsInCanonicalBaseJKK: Option[Int] = values(24).map(_.toInt)
    val Number_FastDisjointnessAxioms: Option[Int] = values(25).map(_.toInt)
    val Number_ImplicationsInFinalRelativeBase: Option[Int] = values(26).map(_.toInt)
    val Number_ImplicationsInRelativeCanonicalBase: Option[Int] = values(27).map(_.toInt)

    val Number_ObjectsInInducedContext: Option[Int] = Number_ObjectsInReducedDomain

    val ComputationTime_LoadAndReduce: Option[Long] =
      if ComputationTime_LoadOntology.isDefined && ComputationTime_FirstReduction.isDefined
      then Some(ComputationTime_LoadOntology.get + ComputationTime_FirstReduction.get)
      else None

    val ComputationTime_InitializationWithoutLoadAndReduce: Option[Long] =
      if ComputationTime_Initialization.isDefined && ComputationTime_LoadAndReduce.isDefined
      then Some(ComputationTime_Initialization.get + ComputationTime_LoadAndReduce.get)
      else None
    
    val Number_NamesInSignature: Option[Int] = reductionStatisticsMap.get(ont).map(_.Number_NamesInSignature)
    val Number_ConceptNamesInSignature: Option[Int] = reductionStatisticsMap.get(ont).map(_.Number_ConceptNamesInSignature)
    val Number_RoleNamesInSignature: Option[Int] = reductionStatisticsMap.get(ont).map(_.Number_RoleNamesInSignature)

    val Number_TriplesInReduction: Option[Int] = reductionStatisticsMap.get(ont).map(_.Number_TriplesInReduction)
    val Number_TypeTriplesInReduction: Option[Int] = reductionStatisticsMap.get(ont).map(_.Number_TypeTriplesInReduction)
    val Number_RelationTriplesInReduction: Option[Int] = reductionStatisticsMap.get(ont).map(_.Number_RelationTriplesInReduction)

    val ComputationTime_TotalRelativeBaseWithoutLoadAndReduce: Option[Long] =
      if ComputationTime_Total.isDefined && ComputationTime_LoadAndReduce.isDefined
      then Some(ComputationTime_Total.get - ComputationTime_LoadAndReduce.get - ComputationTime_CanonicalBase.getOrElse(0l) - ComputationTime_CanonicalBaseJKK.getOrElse(0l))
      else None

    val Flag_AcyclicReduction: Option[Boolean] = reductionStatisticsMap.get(ont).map(_.Flag_Acyclic)

    val Number_TriplesPerObjectOnAverageInReduction: Option[Float] = reductionStatisticsMap.get(ont).map(_.Number_TriplesPerObjectOnAverage)
    val Number_TypeTriplesPerObjectOnAverageInReduction: Option[Float] = reductionStatisticsMap.get(ont).map(_.Number_TypeTriplesPerObjectOnAverage)
    val Number_RelationTriplesPerObjectOnAverageInReduction: Option[Float] = reductionStatisticsMap.get(ont).map(_.Number_RelationTriplesPerObjectOnAverage)

    val Number_TriplesInInput: Int = getInputStatistics(ont).Number_Triples
    val Number_TypeTriplesInInput: Int = getInputStatistics(ont).Number_TypeTriples
    val Number_RelationTriplesInInput: Int = getInputStatistics(ont).Number_RelationTriples

    val Flag_AcyclicInput: Boolean = getInputStatistics(ont).Flag_Acyclic

    val Number_TriplesPerObjectOnAverageInInput: Float = getInputStatistics(ont).Number_TriplesPerObjectOnAverage
    val Number_TypeTriplesPerObjectOnAverageInInput: Float = getInputStatistics(ont).Number_TypeTriplesPerObjectOnAverage
    val Number_RelationTriplesPerObjectOnAverageInInput: Float = getInputStatistics(ont).Number_RelationTriplesPerObjectOnAverage

    val Number_IrreduciblesInReduction: Option[Int] = irreduciblesStatistics.get(ont).map(_._1)
    val Ratio_IrreduciblesInReduction: Option[Float] = irreduciblesStatistics.get(ont).map(_._3)

    override def toString: String = {
      "ore-ont-" + ont + ";" +
      mode + ";" +
      status + ";" +
      computationTimeToString(ComputationTime_Total) + ";" +
      computationTimeToString(ComputationTime_LoadOntology) + ";" +
      computationTimeToString(ComputationTime_FirstReduction) + ";" +
//      Number_ObjectsInDomain.map(_.toString).getOrElse("") + ";" +
      Number_ObjectsInDomain.toString + ";" +
      Number_ObjectsInReducedDomain.map(_.toString).getOrElse("") + ";" +
      Number_ObjectsInReducedCanonicalModel.map(_.toString).getOrElse("") + ";" +
      computationTimeToString(ComputationTime_Initialization) + ";" +
      computationTimeToString(ComputationTime_Closures) + ";" +
      Number_Closures.map(_.toString).getOrElse("") + ";" +
      computationTimeToString(ComputationTime_InducedContext) + ";" +
      Number_AttributesInInducedContext.map(_.toString).getOrElse("") + ";" +
      Number_ActiveAttributesInInducedContext.map(_.toString).getOrElse("") + ";" +
      Number_AxiomsInTBox.map(_.toString).getOrElse("") + ";" +
      Number_DisjointnessAxiomsInTBox.map(_.toString).getOrElse("") + ";" +
      computationTimeToString(ComputationTime_BackgroundImplications) + ";" +
      Number_BackgroundImplications.map(_.toString).getOrElse("") + ";" +
      computationTimeToString(ComputationTime_CanonicalBaseJKK) + ";" +
      computationTimeToString(ComputationTime_CanonicalBase) + ";" +
      computationTimeToString(ComputationTime_RelativeCanonicalBase) + ";" +
      Number_ImplicationsInFinalBase.map(_.toString).getOrElse("") + ";" +
      Number_ImplicationsInCanonicalBase.map(_.toString).getOrElse("") + ";" +
      Number_ImplicationsInCanonicalBaseJKK.map(_.toString).getOrElse("") + ";" +
      Number_FastDisjointnessAxioms.map(_.toString).getOrElse("") + ";" +
      Number_ImplicationsInFinalRelativeBase.map(_.toString).getOrElse("") + ";" +
      Number_ImplicationsInRelativeCanonicalBase.map(_.toString).getOrElse("") + ";" +
      Number_ObjectsInInducedContext.map(_.toString).getOrElse("") + ";" +
      computationTimeToString(ComputationTime_LoadAndReduce) + ";" +
      computationTimeToString(ComputationTime_InitializationWithoutLoadAndReduce) + ";" +
      Number_NamesInSignature.map(_.toString).getOrElse("") + ";" +
      Number_ConceptNamesInSignature.map(_.toString).getOrElse("") + ";" +
      Number_RoleNamesInSignature.map(_.toString).getOrElse("") + ";" +
      Number_TriplesInReduction.map(_.toString).getOrElse("") + ";" +
      Number_TypeTriplesInReduction.map(_.toString).getOrElse("") + ";" +
      Number_RelationTriplesInReduction.map(_.toString).getOrElse("") + ";" +
      computationTimeToString(ComputationTime_TotalRelativeBaseWithoutLoadAndReduce) + ";" +
      Flag_AcyclicReduction.map(_.toString).getOrElse("") + ";" +
      Number_TriplesPerObjectOnAverageInReduction.map(_.toString).getOrElse("") + ";" +
      Number_TypeTriplesPerObjectOnAverageInReduction.map(_.toString).getOrElse("") + ";" +
      Number_RelationTriplesPerObjectOnAverageInReduction.map(_.toString).getOrElse("") + ";" +
      Number_TriplesInInput.toString + ";" +
      Number_TypeTriplesInInput.toString + ";" +
      Number_RelationTriplesInInput.toString + ";" +
      Flag_AcyclicInput.toString + ";" +
      Number_TriplesPerObjectOnAverageInInput.toString + ";" +
      Number_TypeTriplesPerObjectOnAverageInInput.toString + ";" +
      Number_RelationTriplesPerObjectOnAverageInInput.toString + ";" +
      Number_IrreduciblesInReduction.map(_.toString).getOrElse("") + ";" +
      Ratio_IrreduciblesInReduction.map(_.toString).getOrElse("") + "\n"
    }

    private val TIMEOUT_MINUTES = 8 * 60;
    private val TIMEOUT_SECONDS = TIMEOUT_MINUTES * 60;

    private def computationTimeToString(maybeComputationTime: Option[Long]): String = {
      status match {
        case Success() => maybeComputationTime.map(long => (long.toFloat / 1000f).toString).getOrElse("")
//        case Timeout(minutes) => (minutes * 60).toString
        case Timeout(minutes) => if minutes == TIMEOUT_MINUTES then TIMEOUT_SECONDS.toString else throw RuntimeException()
        case OutOfMemory(gigabytes) => (2 * TIMEOUT_SECONDS).toString
        case PoweringTooLarge() => (4 * TIMEOUT_SECONDS).toString
        case _ => ""
      }
    }

  }

//  private val ore2015Folder = java.io.File("/Users/francesco/workspace/Java_Scala/efficient-axiomatization-of-owl2el-ontologies-from-data/ore2015_pool_sample")
  private val ore2015Folder = java.io.File("ore2015_pool_sample")
  private val inputOntologiesFolder = java.io.File(ore2015Folder, "files")

  private val inputStatisticsMap = scala.collection.mutable.HashMap[Int, InputStatistics]()

  private class InputStatistics(val Number_ObjectsInDomain: Int,
                                val Number_NamesInSignature: Int,
                                val Number_ConceptNamesInSignature: Int,
                                val Number_RoleNamesInSignature: Int,
                                val Number_Triples: Int,
                                val Number_TypeTriples: Int,
                                val Number_RelationTriples: Int,
                                val Flag_Acyclic: Boolean,
                                val Number_TriplesPerObjectOnAverage: Float,
                                val Number_TypeTriplesPerObjectOnAverage: Float,
                                val Number_RelationTriplesPerObjectOnAverage: Float) {}

  private def getInputStatistics(ont: Int): InputStatistics = {
    inputStatisticsMap.getOrElseUpdate(ont, {

      val owlFileInputOntology = java.io.File(inputOntologiesFolder, "ore_ont_" + ont + ".owl")
      val owlManager = org.semanticweb.owlapi.apibinding.OWLManager.createOWLOntologyManager()
      val owlOntologyInputOntology = owlManager.loadOntologyFromOntologyDocument(owlFileInputOntology)
      val graph = BitGraph.fromOntology(owlOntologyInputOntology)
      owlManager.clearOntologies()

      val Number_ObjectsInDomain = graph.nodes().size
      val Flag_Acyclic = hasAcyclicABox(graph)

      val Number_ConceptNamesInSignature = graph.labels().size
      val Number_RoleNamesInSignature = graph.relations().size
      val Number_NamesInSignature = Number_ConceptNamesInSignature + Number_RoleNamesInSignature

      var Number_TypeTriples = 0
      var Number_RelationTriples = 0
      val it = graph.nodes().iterator
      while (it.hasNext) {
        val node = it.next()
        Number_TypeTriples += graph.labels(node).size
        Number_RelationTriples += graph.predecessors(node).size
      }
      val Number_Triples = Number_TypeTriples + Number_RelationTriples

      val Number_TriplesPerObjectOnAverage = Number_Triples.toFloat / Number_ObjectsInDomain.toFloat
      val Number_TypeTriplesPerObjectOnAverage = Number_TypeTriples.toFloat / Number_ObjectsInDomain.toFloat
      val Number_RelationTriplesPerObjectOnAverage = Number_RelationTriples.toFloat / Number_ObjectsInDomain.toFloat

      InputStatistics(
        Number_ObjectsInDomain,
        Number_NamesInSignature,
        Number_ConceptNamesInSignature,
        Number_RoleNamesInSignature,
        Number_Triples,
        Number_TypeTriples,
        Number_RelationTriples,
        Flag_Acyclic,
        Number_TriplesPerObjectOnAverage,
        Number_TypeTriplesPerObjectOnAverage,
        Number_RelationTriplesPerObjectOnAverage
      )
    })
  }

  private class ReductionStatistics(val Number_ObjectsInDomain: Int,
                                    val Number_ObjectsInReducedDomain: Int,
                                    val ComputationTime_LoadOntology: Long,
                                    val ComputationTime_FirstReduction: Long,
                                    val Number_NamesInSignature: Int,
                                    val Number_ConceptNamesInSignature: Int,
                                    val Number_RoleNamesInSignature: Int,
                                    val Number_TriplesInReduction: Int,
                                    val Number_TypeTriplesInReduction: Int,
                                    val Number_RelationTriplesInReduction: Int,
                                    val Flag_Acyclic: Boolean,
                                    val Number_TriplesPerObjectOnAverage: Float,
                                    val Number_TypeTriplesPerObjectOnAverage: Float,
                                    val Number_RelationTriplesPerObjectOnAverage: Float) {}

//  private val experimentFolder = java.io.File("/Users/francesco/workspace/Java_Scala/efficient-axiomatization-of-owl2el-ontologies-from-data/ore2015_pool_sample_experiments_russell_performance")
  private val experimentFolder = java.io.File("ore2015_pool_sample_experiments")
//  private val experimentFolder = java.io.File("ore2015_pool_sample_experiments_test")
  private val resultsFolder = java.io.File(experimentFolder,"results")
  private val statisticsFolder = java.io.File(experimentFolder,"statistics")
  private val reductionsFolder = java.io.File(experimentFolder,"files")

  private val resultsMap = scala.collection.mutable.HashMap[Int, scala.collection.mutable.HashSet[Result]]()
  private val reductionStatisticsMap = scala.collection.mutable.HashMap[Int, ReductionStatistics]()

  private def putResult(ont: Int, result: Result): Unit = {
    resultsMap
      .getOrElseUpdate(ont, scala.collection.mutable.HashSet[Result]())
      .add(result)
  }

  private def readResults(): Unit = {
    resultsFolder.listFiles()
      .filter(f => f.isFile && (f.getName startsWith "ore_ont_") && (f.getName endsWith ".csv"))
      .foreach(csvFile => {
        println("Reading " + csvFile)
        val ont = csvFile.getName.split('.')(0).substring(8).toInt
        val csvSource = scala.io.Source.fromFile(csvFile)
        val it = csvSource.getLines()
        it.foreach(csl => {
          val result = Result(csl)
          if (ont equals result.ont)
            putResult(ont, result)
          else
            throw new IllegalArgumentException()
        })
        csvSource.close()
      })
  }

//  private def writeToFile(file: java.io.File, code: java.io.FileWriter => Unit): Unit = {
//    val writer = java.io.FileWriter(file)
//    code(writer)
//    writer.close()
//    println("Data written to " + file)
//  }

  def readAdditionalReductionStatistics(): Unit = {
    statisticsFolder.listFiles()
      .filter(f => f.isFile && (f.getName startsWith "ore_ont_") && (f.getName endsWith ".csv") && !(f.getName endsWith "_reduced.csv"))
      .foreach(csvFile => {
        println("Reading " + csvFile)
        val ont = csvFile.getName.split('.')(0).substring(8).toInt
//        val reductionStatisticsFile = java.io.File(statisticsFolder, "ore_ont_" + ont + "_reduced.csv")
//        if (!reductionStatisticsFile.exists()) {
          val csvSource = scala.io.Source.fromFile(csvFile)
          val line = csvSource.getLines().next()
//          val csl = line.substring(0, line.lastIndexOf(';'))
          val values = line.split(';')
          csvSource.close()

        val Number_ObjectsInDomain = values(0).toInt
        val Number_ObjectsInReducedDomain = values(1).toInt
        val ComputationTime_LoadOntology = values(2).toLong
        val ComputationTime_FirstReduction = values(3).toLong

          val owlFileReduction = java.io.File(reductionsFolder, "ore_ont_" + ont + "_reduced.owl")
          val owlManager = org.semanticweb.owlapi.apibinding.OWLManager.createOWLOntologyManager()
          val owlOntologyReduction = owlManager.loadOntologyFromOntologyDocument(owlFileReduction)
          val graph = BitGraph.fromOntology(owlOntologyReduction)
          owlManager.clearOntologies()

          if (!(graph.nodes().size equals Number_ObjectsInReducedDomain))
            throw new RuntimeException()

          val Flag_Acyclic = hasAcyclicABox(graph)

          val Number_ConceptNamesInSignature = graph.labels().size
          val Number_RoleNamesInSignature = graph.relations().size
          val Number_NamesInSignature = Number_ConceptNamesInSignature + Number_RoleNamesInSignature

          var Number_TypeTriples = 0
          var Number_RelationTriples = 0
          val it = graph.nodes().iterator
          while (it.hasNext) {
            val node = it.next()
            Number_TypeTriples += graph.labels(node).size
            Number_RelationTriples += graph.predecessors(node).size
          }
          val Number_Triples = Number_TypeTriples + Number_RelationTriples

          val Number_TriplesPerObjectOnAverage = Number_Triples.toFloat / Number_ObjectsInReducedDomain.toFloat
          val Number_TypeTriplesPerObjectOnAverage = Number_TypeTriples.toFloat / Number_ObjectsInReducedDomain.toFloat
          val Number_RelationTriplesPerObjectOnAverage = Number_RelationTriples.toFloat / Number_ObjectsInReducedDomain.toFloat

          val stat =
            ReductionStatistics(
              Number_ObjectsInDomain,
              Number_ObjectsInReducedDomain,
              ComputationTime_LoadOntology,
              ComputationTime_FirstReduction,
              Number_NamesInSignature,
              Number_ConceptNamesInSignature,
              Number_RoleNamesInSignature,
              Number_Triples,
              Number_TypeTriples,
              Number_RelationTriples,
              Flag_Acyclic,
              Number_TriplesPerObjectOnAverage,
              Number_TypeTriplesPerObjectOnAverage,
              Number_RelationTriplesPerObjectOnAverage
            )

          reductionStatisticsMap.update(ont, stat)

//          val csl2 =
//            "ore_ont_" + ont + ";"
//            csl + ";" +
//            numberOfNames + ";" +
//            numberOfConceptNames + ";" +
//            numberOfRoleNames + ";" +
//            numberOfTriples + ";" +
//            numberOfTypeTriples + ";" +
//            numberOfRelationTriples
//
//          writeToFile(reductionStatisticsFile, _.write(csl2))
//        }
      })

  }

  def filterResults(): Unit = {

    resultsMap.foreach({
      case ont -> results =>

        println("Filtering results for ore_ont_" + ont)

        object ResultOrdering extends Ordering[Result] {
          override def compare(x: Result, y: Result): Int = {
            if (!(x.mode equals y.mode))
              throw IllegalArgumentException()
            else {
              def n(x: Result): Int = {
                x.status match {
                  case Success() => 1000
                  case Inconsistent() => 900
                  case PoweringTooLarge() => 800
                  case OutOfMemory(_) => 700
                  case Timeout(minutes) => minutes
                  case Error(_) => 1
                }
              }
              if (n(x) > n(y))
                1
              else if (n(x) < n(y))
                -1
              else
                0
            }
          }
        }

        val bestResultFor =
//          (Mode.values: scala.Array[Mode])
          Mode_values
            .map(mode => mode -> results.filter(_.mode equals mode).maxOption(ResultOrdering))
            .toMap

        results.clear()

        def statusToString(mode: Mode): String = {
          bestResultFor(mode).get.status match {
            case Success() => throw RuntimeException()
            case Timeout(minutes) => s"Timeout(${minutes}m)"
            case OutOfMemory(gigabytes) => s"OutOfMemory(${gigabytes}g)"
            case Inconsistent() => "Inconsistent"
            case PoweringTooLarge() => "PoweringTooLarge"
            case Error(number) => s"Error($number)"
          }
        }

//        if (bestResultFor(Mode.Reduction).isDefined)
//          results.addOne(bestResultFor(Mode.Reduction).get)
//          if (bestResultFor(Mode.NoDisjointnessAxioms).isDefined)
//            results.addOne(bestResultFor(Mode.NoDisjointnessAxioms).get)
//            if (bestResultFor(Mode.FastDisjointnessAxioms).isDefined)
//              results.addOne(bestResultFor(Mode.FastDisjointnessAxioms).get)
//              if (bestResultFor(Mode.CanonicalDisjointnessAxioms).isDefined)
//                results.addOne(bestResultFor(Mode.CanonicalDisjointnessAxioms).get)
//              else
//                results.addOne(Result("ore_ont_" + ont + ";Canonical;" + statusToString(Mode.FastDisjointnessAxioms)))
//            else
//              results.addOne(Result("ore_ont_" + ont + ";Fast;" + statusToString(Mode.NoDisjointnessAxioms)))
//              results.addOne(Result("ore_ont_" + ont + ";Canonical;" + statusToString(Mode.NoDisjointnessAxioms)))
//          else
//            results.addOne(Result("ore_ont_" + ont + ";None;" + statusToString(Mode.Reduction)))
//            results.addOne(Result("ore_ont_" + ont + ";Fast;" + statusToString(Mode.Reduction)))
//            results.addOne(Result("ore_ont_" + ont + ";Canonical;" + statusToString(Mode.Reduction)))
//        else
//          throw RuntimeException()

        def findFailedAncestor(mode: Mode): Option[Result] = {
          Mode_parents(ont)(mode).map(bestResultFor).filter(_.isDefined).map(_.get).find(_.status != Success())
            .orElse(Mode_parents(ont)(mode).map(findFailedAncestor).find(_.isDefined).getOrElse(None))
        }

        Mode_values.foreach(mode => {
          if bestResultFor(mode).isDefined
          then results.addOne(bestResultFor(mode).get)
          else {
            val failure = findFailedAncestor(mode)
            if failure.isDefined
            then results.addOne(Result("ore_ont_" + ont + ";" + mode + ";" + statusToString(failure.get.mode)))
            else
              if (bestResultFor(Mode_Reduction).get.status == Success() && bestResultFor(Mode_Reduction).get.Number_ObjectsInReducedDomain.get >= 10)
              throw RuntimeException("No result for " + mode)
          }
        })

    })
  }


//  private val paperFolder = java.io.File("/Users/francesco/workspace/LaTeX/efficient-axiomatization-of-owl2el-ontologies-from-data/KR_2023")
  private val paperFolder = java.io.File("paper")
  private val evaluationFolder = java.io.File(paperFolder, "evaluation")

  def writeEvaluationData(): Unit = {
    val writer =
      Map[Mode, java.io.FileWriter](
        Mode_Reduction -> java.io.FileWriter(java.io.File(evaluationFolder, "Reduction.csv")),
        Mode_None_0_32 -> java.io.FileWriter(java.io.File(evaluationFolder, "NoDisjAx_0_32.csv")),
        Mode_None_1_8 -> java.io.FileWriter(java.io.File(evaluationFolder, "NoDisjAx_1_8.csv")),
        Mode_None_1_32 -> java.io.FileWriter(java.io.File(evaluationFolder, "NoDisjAx_1_32.csv")),
        Mode_None_2_32 -> java.io.FileWriter(java.io.File(evaluationFolder, "NoDisjAx_2_32.csv")),
        Mode_None_INF_32 -> java.io.FileWriter(java.io.File(evaluationFolder, "NoDisjAx_INF_32.csv")),
        Mode_None_INF_INF -> java.io.FileWriter(java.io.File(evaluationFolder, "NoDisjAx_INF_INF.csv")),
        Mode_Fast_0_32 -> java.io.FileWriter(java.io.File(evaluationFolder, "FastDisjAx_0_32.csv")),
        Mode_Fast_1_8 -> java.io.FileWriter(java.io.File(evaluationFolder, "FastDisjAx_1_8.csv")),
        Mode_Fast_1_32 -> java.io.FileWriter(java.io.File(evaluationFolder, "FastDisjAx_1_32.csv")),
        Mode_Fast_2_32 -> java.io.FileWriter(java.io.File(evaluationFolder, "FastDisjAx_2_32.csv")),
        Mode_Fast_INF_32 -> java.io.FileWriter(java.io.File(evaluationFolder, "FastDisjAx_INF_32.csv")),
        Mode_Fast_INF_INF -> java.io.FileWriter(java.io.File(evaluationFolder, "FastDisjAx_INF_INF.csv")),
        Mode_Canonical_0_32 -> java.io.FileWriter(java.io.File(evaluationFolder, "CanDisjAx_0_32.csv")),
        Mode_Canonical_1_8 -> java.io.FileWriter(java.io.File(evaluationFolder, "CanDisjAx_1_8.csv")),
        Mode_Canonical_1_32 -> java.io.FileWriter(java.io.File(evaluationFolder, "CanDisjAx_1_32.csv")),
        Mode_Canonical_2_32 -> java.io.FileWriter(java.io.File(evaluationFolder, "CanDisjAx_2_32.csv")),
        Mode_Canonical_INF_32 -> java.io.FileWriter(java.io.File(evaluationFolder, "CanDisjAx_INF_32.csv")),
        Mode_Canonical_INF_INF -> java.io.FileWriter(java.io.File(evaluationFolder, "CanDisjAx_INF_INF.csv"))
      )
    val writerSuccess =
      Map[Mode, java.io.FileWriter](
        Mode_Reduction -> java.io.FileWriter(java.io.File(evaluationFolder, "Reduction_Success.csv")),
        Mode_None_0_32 -> java.io.FileWriter(java.io.File(evaluationFolder, "NoDisjAx_0_32_Success.csv")),
        Mode_None_1_8 -> java.io.FileWriter(java.io.File(evaluationFolder, "NoDisjAx_1_8_Success.csv")),
        Mode_None_1_32 -> java.io.FileWriter(java.io.File(evaluationFolder, "NoDisjAx_1_32_Success.csv")),
        Mode_None_2_32 -> java.io.FileWriter(java.io.File(evaluationFolder, "NoDisjAx_2_32_Success.csv")),
        Mode_None_INF_32 -> java.io.FileWriter(java.io.File(evaluationFolder, "NoDisjAx_INF_32_Success.csv")),
        Mode_None_INF_INF -> java.io.FileWriter(java.io.File(evaluationFolder, "NoDisjAx_INF_INF_Success.csv")),
        Mode_Fast_0_32 -> java.io.FileWriter(java.io.File(evaluationFolder, "FastDisjAx_0_32_Success.csv")),
        Mode_Fast_1_8 -> java.io.FileWriter(java.io.File(evaluationFolder, "FastDisjAx_1_8_Success.csv")),
        Mode_Fast_1_32 -> java.io.FileWriter(java.io.File(evaluationFolder, "FastDisjAx_1_32_Success.csv")),
        Mode_Fast_2_32 -> java.io.FileWriter(java.io.File(evaluationFolder, "FastDisjAx_2_32_Success.csv")),
        Mode_Fast_INF_32 -> java.io.FileWriter(java.io.File(evaluationFolder, "FastDisjAx_INF_32_Success.csv")),
        Mode_Fast_INF_INF -> java.io.FileWriter(java.io.File(evaluationFolder, "FastDisjAx_INF_INF_Success.csv")),
        Mode_Canonical_0_32 -> java.io.FileWriter(java.io.File(evaluationFolder, "CanDisjAx_0_32_Success.csv")),
        Mode_Canonical_1_8 -> java.io.FileWriter(java.io.File(evaluationFolder, "CanDisjAx_1_8_Success.csv")),
        Mode_Canonical_1_32 -> java.io.FileWriter(java.io.File(evaluationFolder, "CanDisjAx_1_32_Success.csv")),
        Mode_Canonical_2_32 -> java.io.FileWriter(java.io.File(evaluationFolder, "CanDisjAx_2_32_Success.csv")),
        Mode_Canonical_INF_32 -> java.io.FileWriter(java.io.File(evaluationFolder, "CanDisjAx_INF_32_Success.csv")),
        Mode_Canonical_INF_INF -> java.io.FileWriter(java.io.File(evaluationFolder, "CanDisjAx_INF_INF_Success.csv"))
      )
//    val writerSuccessFewerThanTen =
//      Map[Mode, java.io.FileWriter](
//        Mode.Reduction -> java.io.FileWriter(java.io.File(evaluationFolder, "Reduction_Success_FewerThan10.csv")),
//        Mode.NoDisjointnessAxioms -> java.io.FileWriter(java.io.File(evaluationFolder, "NoDisjAx_Success_FewerThan10.csv")),
//        Mode.FastDisjointnessAxioms -> java.io.FileWriter(java.io.File(evaluationFolder, "FastDisjAx_Success_FewerThan10.csv")),
//        Mode.CanonicalDisjointnessAxioms -> java.io.FileWriter(java.io.File(evaluationFolder, "CanDisjAx_Success_FewerThan10.csv"))
//      )
//    val writerSuccessAtLeastTen =
//      Map[Mode, java.io.FileWriter](
//        Mode.Reduction -> java.io.FileWriter(java.io.File(evaluationFolder, "Reduction_Success_AtLeast10.csv")),
//        Mode.NoDisjointnessAxioms -> java.io.FileWriter(java.io.File(evaluationFolder, "NoDisjAx_Success_AtLeast10.csv")),
//        Mode.FastDisjointnessAxioms -> java.io.FileWriter(java.io.File(evaluationFolder, "FastDisjAx_Success_AtLeast10.csv")),
//        Mode.CanonicalDisjointnessAxioms -> java.io.FileWriter(java.io.File(evaluationFolder, "CanDisjAx_Success_AtLeast10.csv"))
//      )
    resultsMap.foreach({
      case ont -> results => {
        results.foreach(result => {
          writer(result.mode).write(result.toString)
          if (result.status equals Success())
            writerSuccess(result.mode).write(result.toString)
//            if (reductionStatisticsMap(ont).Number_ObjectsInReducedDomain > 9)
//              writerSuccessAtLeastTen(result.mode).write(result.toString)
//            else
//              writerSuccessFewerThanTen(result.mode).write(result.toString)
        })
      }
    })
    writer.values.foreach(_.close())
    writerSuccess.values.foreach(_.close())
    val reductionTableWriter = java.io.FileWriter(java.io.File(evaluationFolder, "ReductionStatistics.tex"))
    val it = resultsMap.iterator
    var number = 0
    var acyclic = 0
    var success = 0
    var timeout = 0
    var outOfMemory = 0
    var inconsistent = 0
    var poweringTooLarge = 0
    var error = 0
    while (it.hasNext) {
      val ont -> results = it.next()
      number += 1
      if (getInputStatistics(ont).Flag_Acyclic)
        acyclic += 1
      val maybeResult = results.find(_.mode equals Mode_Reduction)
      if (maybeResult.isDefined) {
        val result = maybeResult.get
        result.status match {
          case Success() => success += 1
          case Timeout(_) => timeout += 1
          case OutOfMemory(_) => outOfMemory += 1
          case Inconsistent() => inconsistent += 1
          case PoweringTooLarge() => poweringTooLarge += 1
          case Error(_) => error += 1
          case _ => throw MatchError(result.status)
        }
      } else {
        throw RuntimeException()
      }
    }
    val percentageAcyclic = (acyclic.toFloat / number.toFloat) * 100
    val percentageSuccess = (success.toFloat / number.toFloat) * 100
    val percentageTimeout = (timeout.toFloat / number.toFloat) * 100
    val percentageOutOfMemory = (outOfMemory.toFloat / number.toFloat) * 100
    val percentageInconsistent = (inconsistent.toFloat / number.toFloat) * 100
    val percentagePoweringTooLarge = (poweringTooLarge.toFloat / number.toFloat) * 100
    val percentageError = (error.toFloat / number.toFloat) * 100
    reductionTableWriter.write(s"\\newcommand{\\ReductionNumberOfDatasets}{$number}\n")
    reductionTableWriter.write(f"\\newcommand{\\ReductionSuccessNumber}{$success}\n")
    reductionTableWriter.write(f"\\newcommand{\\ReductionSuccessPercentage}{$percentageSuccess%1.2f\\,\\%%}\n")
    reductionTableWriter.write(f"\\newcommand{\\ReductionTimeoutNumber}{$timeout}\n")
    reductionTableWriter.write(f"\\newcommand{\\ReductionTimeoutPercentage}{$percentageTimeout%1.2f\\,\\%%}\n")
    reductionTableWriter.write(f"\\newcommand{\\ReductionOutOfMemoryNumber}{$outOfMemory}\n")
    reductionTableWriter.write(f"\\newcommand{\\ReductionOutOfMemoryPercentage}{$percentageOutOfMemory%1.2f\\,\\%%}\n")
    reductionTableWriter.write(f"\\newcommand{\\ReductionInconsistentNumber}{$inconsistent}\n")
    reductionTableWriter.write(f"\\newcommand{\\ReductionInconsistentPercentage}{$percentageInconsistent%1.2f\\,\\%%}\n")
    reductionTableWriter.write(f"\\newcommand{\\ReductionPoweringTooLargeNumber}{$poweringTooLarge}\n")
    reductionTableWriter.write(f"\\newcommand{\\ReductionPoweringTooLargePercentage}{$percentagePoweringTooLarge%1.2f\\,\\%%}\n")
    reductionTableWriter.write(f"\\newcommand{\\ReductionOtherErrorNumber}{$error}\n")
    reductionTableWriter.write(f"\\newcommand{\\ReductionOtherErrorPercentage}{$percentageError%1.2f\\,\\%%}\n")
    reductionTableWriter.write(f"\\newcommand{\\ReductionAcyclicNumber}{$acyclic}\n")
    reductionTableWriter.write(f"\\newcommand{\\ReductionAcyclicPercentage}{$percentageAcyclic%1.2f\\,\\%%}\n")
    reductionTableWriter.close()
  }

  def main(args: Array[String]): Unit = {
    readAdditionalReductionStatistics()
//    computeIrreducibles()
    readResults()
    filterResults()
    writeEvaluationData()
  }

//  private def foo(): Unit = {
//
//    val OreOntology = raw"ore_ont_(\d+)(|_reduced).owl".r
//
//    val subs = scala.collection.immutable.Set("dl/classification", "dl/consistency", "dl/instantiation", "el/classification", "el/consistency", "el/instantiation")
//      .map(sub => {
//        val fileorder = java.io.File("/Users/francesco/workspace/Java_Scala/efficient-axiomatization-of-owl2el-ontologies-from-data/ore2015_pool_sample/" + sub + "/fileorder.txt")
//        val source = scala.io.Source.fromFile(fileorder)
//        val onts = source.getLines().map(OreOntology.findFirstMatchIn(_).get.group(1).toInt).toSet
//        source.close()
//        sub -> onts
//      })
//      .toMap
//
//    def getSubs(ont: Int): scala.collection.Set[String] = {
//      subs.keySet.filter(subs(_) contains ont)
//    }
//
//    ()
//  }

  val irreduciblesStatistics = scala.collection.mutable.HashMap[Int, (Int, Int, Float)]()

  private def computeIrreducibles(): Unit = {

    given logger: Logger = NoLogger()

    val ReducedOreOntologyRegex = raw"ore_ont_(\d+)_reduced.owl".r
    val SuccessRegex = raw";(None|Fast|Canonical);Success;".r.unanchored

    val experimentFolder = java.io.File("/Users/francesco/workspace/Java_Scala/efficient-axiomatization-of-owl2el-ontologies-from-data/ore2015_pool_sample_experiments_russell_performance")
    val reductionFolder = java.io.File(experimentFolder, "files")
    val resultsFolder = java.io.File(experimentFolder, "results")

    reductionFolder.listFiles().foreach(file => {

      if (ReducedOreOntologyRegex matches file.getName) {

        val ontology = org.semanticweb.owlapi.apibinding.OWLManager.createOWLOntologyManager().loadOntologyFromOntologyDocument(file)
        val graph = BitGraph.fromOntology(ontology)
        val size = graph.nodes().size

        val ont = ReducedOreOntologyRegex.findFirstMatchIn(file.getName).get.group(1).toInt

        val resultFile = java.io.File(resultsFolder, s"ore_ont_$ont.csv")
        if (resultFile.exists()) {
          val resultSource = scala.io.Source.fromFile(resultFile)
          val tryComputeIrreducibles = resultSource.getLines().exists(SuccessRegex.matches)
          resultSource.close()

          if (tryComputeIrreducibles) {

            println("Determining number of irreducibles in " + file)

            val simulation = GraphSimulator.computeMaximalSimulation(graph, graph)

            val strictlyAboveArray = new scala.Array[scala.collection.mutable.BitSet](size)
            val doubleStrictlyAboveArray = new scala.Array[scala.collection.mutable.BitSet](size)
            val upperNeighborsArray = new scala.Array[scala.collection.mutable.BitSet](size)

            def arrayGetOrUpdate[T](array: scala.Array[T], index: Int, update: Int => T): T =
              if (array(index) == null)
                array(index) = update(index)
              array(index)

            def strictlyAbove(node: Int): scala.collection.mutable.BitSet =
              arrayGetOrUpdate(strictlyAboveArray, node, n => {
                simulation.row(n).viewAsMutableBitSet diff simulation.col(n).viewAsMutableBitSet
              })

            def doubleStrictlyAbove(node: Int): scala.collection.mutable.BitSet =
              arrayGetOrUpdate(doubleStrictlyAboveArray, node, n => {
                strictlyAbove(n).flatMap(strictlyAbove)
              })

            def upperNeighbors(node: Int): scala.collection.mutable.BitSet =
              arrayGetOrUpdate(upperNeighborsArray, node, n => {
                strictlyAbove(n) diff doubleStrictlyAbove(n)
              })

            val clop = PoweringClosureOperator(graph)

            def isIrreducible(node: Int): Boolean = {
              val uppers = upperNeighbors(node)
              (uppers.size < 2) || !clop(uppers).contains(node)
            }

            val irreducibles = graph.nodes().filter(isIrreducible)

            val ratio = (irreducibles.size.toFloat / size.toFloat) * 100f

            irreduciblesStatistics.put(ont, (irreducibles.size, size, ratio))

          }

        }

      }

    })

  }


  private def hasAcyclicABox(graph: BitGraph[_, _]): Boolean = {

    var isAcyclic = true

    var i = 0
    val index = new Array[Int](graph.nodes().size)
    val lowlink = new Array[Int](graph.nodes().size)
    val stack = scala.collection.mutable.Stack[Int]()
    val isOnStack = scala.collection.mutable.BitSet()
    val remainingNodes = graph.nodes().clone()

    val whileLoop = new scala.util.control.Breaks
    whileLoop.tryBreakable {
      while (remainingNodes.nonEmpty) {
        val v = remainingNodes.head
        val cycleDetected = tarjan(v)
        if (cycleDetected)
          whileLoop.break()
      }
    } catchBreak {
      isAcyclic = false
    }

    def tarjan(v: Int): Boolean = {
      index(v) = i
      lowlink(v) = i
      i += 1
      stack.push(v)
      isOnStack(v) = true
      remainingNodes.remove(v)

      val forLoop = new scala.util.control.Breaks
      forLoop.tryBreakable {
        for (w <- graph.successorRelations(v).flatMap(r => graph.successorsForRelation(v, r))) {
          if (remainingNodes contains w)
            val cycleDetected = tarjan(w)
            if (cycleDetected)
              forLoop.break()
            else
              lowlink(v) = lowlink(v) min lowlink(w)
          else if (isOnStack(w))
            lowlink(v) = lowlink(v) min index(w)
        }
        if (lowlink(v) equals index(v)) {
          val scc = scala.collection.mutable.BitSet()
          while {
            val w = stack.pop()
            isOnStack(w) = false
            scc.add(w)
            !(w equals v)
          } do ()
          scc.size > 1
        } else {
          false
        }
      } catchBreak {
        true
      }
    }

    isAcyclic
  }


  def getConnectedComponents[L, R](graph: BitGraph[L, R]): collection.Set[collection.BitSet] = {
    val connectedComponents = collection.mutable.HashSet[collection.BitSet]()
    val hasNotBeenVisited = graph.nodes().clone()
    while (hasNotBeenVisited.nonEmpty) {
      val connectedComponent = collection.mutable.BitSet()

      @tailrec def dfs(nodes: Iterable[Int]): Unit = {
        val next = collection.mutable.BitSet()
        for (node <- nodes) {
          if (hasNotBeenVisited(node) == true) {
            hasNotBeenVisited(node) = false
            connectedComponent.addOne(node)
            //graph.predecessors(node).foreach({ case (_, pred) => dfs(pred) })
            //graph.successorRelations(node).flatMap(graph.successorsForRelation(node, _)).foreach({ succ => dfs(succ) })
            next.addAll(graph.predecessors(node).map(_._2))
            next.addAll(graph.successorRelations(node).flatMap(graph.successorsForRelation(node, _)))
          }
        }
        if (next.nonEmpty) {
          dfs(next)
        }
      }

      dfs(Iterable.single(hasNotBeenVisited.head))
      connectedComponents.addOne(connectedComponent)
    }
    connectedComponents
  }

}
