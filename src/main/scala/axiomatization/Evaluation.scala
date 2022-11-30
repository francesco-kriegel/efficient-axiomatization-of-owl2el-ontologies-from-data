package de.tu_dresden.inf.lat
package axiomatization

import org.phenoscape.scowl.*
import scala.jdk.StreamConverters.*

object Evaluation {

  private enum Mode:
    case Reduction, NoDisjointnessAxioms, FastDisjointnessAxioms, CanonicalDisjointnessAxioms

  private object Mode {
    def apply(string: String): Mode = {
      string match {
        case "Reduction" => Reduction
        case "None" => NoDisjointnessAxioms
        case "Fast" => FastDisjointnessAxioms
        case "Canonical" => CanonicalDisjointnessAxioms
        case _ => throw MatchError(string)
      }
    }
  }

  private sealed abstract class Status
  private case class Success() extends Status
  private case class Timeout(minutes: Int) extends Status
  private case class OutOfMemory(gigabytes: Int) extends Status
  private case class Inconsistent() extends Status
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

    val ComputationTime_TotalWithoutLoadAndReduce: Option[Long] =
      if ComputationTime_Total.isDefined && ComputationTime_LoadAndReduce.isDefined
      then Some(ComputationTime_Total.get - ComputationTime_LoadAndReduce.get)
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
      computationTimeToString(ComputationTime_TotalWithoutLoadAndReduce) + ";" +
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
      Number_RelationTriplesPerObjectOnAverageInInput.toString + "\n"
    }

    private def computationTimeToString(maybeComputationTime: Option[Long]): String = {
      status match {
        case Success() => maybeComputationTime.map(long => (long.toFloat / 1000f).toString).getOrElse("")
        case Timeout(minutes) => (minutes * 60).toString
        case _ => ""
      }
    }

  }

  private val ore2015Folder = java.io.File("/Users/francesco/workspace/Java_Scala/efficient-axiomatization-of-owl2el-ontologies-from-data/ore2015_pool_sample")
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
      val graph = Interpretation.loadFromOntologyFile(owlOntologyInputOntology)

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

  private val experimentFolder = java.io.File("/Users/francesco/workspace/Java_Scala/efficient-axiomatization-of-owl2el-ontologies-from-data/ore2015_pool_sample_experiments_russell_performance")
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
          val graph = Interpretation.loadFromOntologyFile(owlOntologyReduction)

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


  private val paperFolder = java.io.File("/Users/francesco/workspace/LaTeX/constructing-owl-2-el-ontologies-from-data/SAC-KRR_2023")
  private val evaluationFolder = java.io.File(paperFolder, "evaluation")

  def writeEvaluationData(): Unit = {
    val writer =
      Map[Mode, java.io.FileWriter](
        Mode.Reduction -> java.io.FileWriter(java.io.File(evaluationFolder, "Reduction.csv")),
        Mode.NoDisjointnessAxioms -> java.io.FileWriter(java.io.File(evaluationFolder, "NoDisjAx.csv")),
        Mode.FastDisjointnessAxioms -> java.io.FileWriter(java.io.File(evaluationFolder, "FastDisjAx.csv")),
        Mode.CanonicalDisjointnessAxioms -> java.io.FileWriter(java.io.File(evaluationFolder, "CanDisjAx.csv"))
      )
    val writerSuccess =
      Map[Mode, java.io.FileWriter](
        Mode.Reduction -> java.io.FileWriter(java.io.File(evaluationFolder, "Reduction_Success.csv")),
        Mode.NoDisjointnessAxioms -> java.io.FileWriter(java.io.File(evaluationFolder, "NoDisjAx_Success.csv")),
        Mode.FastDisjointnessAxioms -> java.io.FileWriter(java.io.File(evaluationFolder, "FastDisjAx_Success.csv")),
        Mode.CanonicalDisjointnessAxioms -> java.io.FileWriter(java.io.File(evaluationFolder, "CanDisjAx_Success.csv"))
      )
    resultsMap.foreach({
      case ont -> results => {
        results.foreach(result => {
          writer(result.mode).write(result.toString)
          if (result.status equals Success())
            writerSuccess(result.mode).write(result.toString)
        })
      }
    })
    writer.values.foreach(_.close())
    writerSuccess.values.foreach(_.close())

  }

  def main(args: Array[String]): Unit = {
    readAdditionalReductionStatistics()
    readResults()
    writeEvaluationData()
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

}
