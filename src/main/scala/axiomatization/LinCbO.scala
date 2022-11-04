package de.tu_dresden.inf.lat
package axiomatization

import axiomatization.Util.GLOBAL_COUNTER

import java.io.{BufferedReader, File, FileReader}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object LinCbO {

  def computeCanonicalBase[M](cxt: FormalContext[_, M], identifier: String): collection.Seq[(collection.Set[M], collection.Set[M])] = {

    //    val basepath = "/Users/francesco/workspace/Java_Scala/efficient-axiomatization-of-owl2el-ontologies-from-data/"
    val basepath = ""
    val cxtFile = new File(basepath + identifier + ".cxt")

    cxt.writeToFile(cxtFile)

    println("Induced context written to " + cxtFile)

    println("Running LinCbO...")
    println(sys.process.Process(basepath + "LinCbO/fcai/CanonicalBasis/fcai " + basepath + identifier + ".cxt " + basepath + identifier + ".canbase 1").!!)

    def read(string: String): collection.Set[M] = {
      val set = mutable.HashSet[M]()
      val i = string.iterator
      val j = cxt.occupiedAttributes.iterator
      while (i.hasNext && j.hasNext)
        if (i.next() equals '1')
          set.addOne(j.next())
      set
    }

    val canonicalBase = ListBuffer[(collection.Set[M], collection.Set[M])]()
    val reader = new BufferedReader(new FileReader(new File(basepath + identifier + ".canbase-myCboObLinNoPruning")))
    //    reader.lines().forEach(println(_))
    reader.lines.forEach(line => {
      if (line.contains("=>")) {
        val s = line.split("=>")
        val premise = read(s(0))
        val conclusion = read(s(1))
        canonicalBase.addOne((premise, conclusion))
      }
    })
    reader.close()

    canonicalBase

  }

}
