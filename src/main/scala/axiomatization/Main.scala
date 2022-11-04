package de.tu_dresden.inf.lat
package axiomatization

import java.io.File
import scala.annotation.tailrec
import scala.io.StdIn.readLine

object Main {

  def main(args: Array[String]): Unit = {
    val (ontologyFile, ont) =
      if (args.isEmpty) readInputFileName("Enter number of test ontology in the ORE2015 pool sample: ")
      else (new File("ore2015_pool_sample/files/ore_ont_" + args(0) + ".owl"), "ore_ont_" + args(0))
    Axiomatization.run(ontologyFile, ont)
  }

  @tailrec
  private def readInputFileName(message: String): (File, String) = {
    print(message)
    val ont = "ore_ont_" + readLine()
//    val file = new File("/Users/francesco/workspace/Data/ore2015_pool_sample/files/" + ont + ".owl")
    val file = new File("ore2015_pool_sample/files/" + ont + ".owl")
    if (file.exists) {
      (file, ont)
    } else {
      System.err.println("The file could not be found.")
      println()
      readInputFileName(message)
    }
  }

}
