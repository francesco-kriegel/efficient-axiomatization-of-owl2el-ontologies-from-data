package de.tu_dresden.inf.lat
package axiomatization

import java.io.File
import scala.annotation.tailrec
import scala.io.StdIn.readLine

object Main {

  def main(args: Array[String]): Unit = {

    try {

      if (args.isEmpty) {

        val (ontologyFile, ont) = readInputFileName("Enter number of test ontology in the ORE2015 pool sample: ")
        print("Which disjointness axioms be computed (None, Fast, Canonical)? ")
        val answer = readLine().toLowerCase
        val disjointnessAxioms =
          if (answer.startsWith("c")) WhichDisjointnessAxioms.Canonical
          else if (answer.startsWith("f")) WhichDisjointnessAxioms.Fast
          else WhichDisjointnessAxioms.None
        println()
        given logger: Logger = ConsoleLogger()
        Axiomatization.run(ontologyFile, ont, disjointnessAxioms)

      } else {

        val ont = "ore_ont_" + args(0)
        val ontologyFile = new File("ore2015_pool_sample/files/" + ont + ".owl")
        if (!ontologyFile.exists())
          throw new IllegalArgumentException("The file " + ontologyFile + " does not exist.")
        val disjointnessAxioms =
          if (args(1).toLowerCase.startsWith("c")) WhichDisjointnessAxioms.Canonical
          else if (args(1).toLowerCase.startsWith("f")) WhichDisjointnessAxioms.Fast
          else WhichDisjointnessAxioms.None
        val onlyComputeReduction = args(2).startsWith("onlyComputeReduction")
        given logger: Logger =
          if args(3).toLowerCase.startsWith("quiet")
          then NoLogger()
          else ConsoleLogger()
        Axiomatization.run(ontologyFile, ont, disjointnessAxioms, onlyComputeReduction)

      }

    } catch {

      case _: OutOfMemoryError => System.exit(3)

    }

  }

  @tailrec
  private def readInputFileName(message: String): (File, String) = {
    print(message)
    val ont = "ore_ont_" + readLine()
    // val file = new File("/Users/francesco/workspace/Data/ore2015_pool_sample/files/" + ont + ".owl")
    val file = new File("ore2015_pool_sample/files/" + ont + ".owl")
    if (file.exists) {
      (file, ont)
    } else {
      System.err.println("The file " + file + " does not exist.")
      println()
      readInputFileName(message)
    }
  }

}
