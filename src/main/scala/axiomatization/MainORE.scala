package de.tu_dresden.inf.lat
package axiomatization

import java.io.File
import org.semanticweb.owlapi.apibinding.OWLManager
import scala.collection.immutable
import scala.collection.mutable
import scala.jdk.StreamConverters.*

object MainORE {

  def main(args: Array[String]): Unit = {

    val sizeMap = mutable.HashMap[Int, Int]()
    val manager = OWLManager.createOWLOntologyManager()

    for (n <- 20 to 16855) {
      val ontologyFile = new File("/Users/francesco/workspace/Data/ore2015_pool_sample/files/ore_ont_" + n + ".owl")
      if (ontologyFile.exists()) {
        val ontology = manager.loadOntologyFromOntologyDocument(ontologyFile)
        var size = 0
        ontology.individualsInSignature().toScala(LazyList).foreach(_ => size += 1)
        ontology.anonymousIndividuals().toScala(LazyList).foreach(_ => size += 1)
        sizeMap(n) = size
        print("\rore_ont_" + n + ".owl   " + size + " individuals")
        manager.clearOntologies()
      }
    }
    print("\r")

    sizeMap.toSeq.sortBy(-_._2).foreach((n, size) => println("ore_ont_" + n + ".owl   " + size + " individuals"))

  }

}
