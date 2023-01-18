package de.tu_dresden.inf.lat
package axiomatization

import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable
//import scala.jdk.CollectionConverters.*
import collection.parallel.CollectionConverters.*
import util.control.Breaks.*
import de.tu_dresden.inf.lat.axiomatization.Util.{printExecutionTime, writeExecutionTime}
import de.tu_dresden.inf.lat.parallel.NestedParallelComputations._

object FCbOPar {

  def computeAllClosures(n: Int,
                         closure: collection.BitSet => collection.BitSet,
                         inclusionIdeal: collection.BitSet => Boolean = _ => true)
                        (using logger: Logger)
  : mutable.Map[collection.BitSet, collection.BitSet] = {

    val closures = collection.concurrent.TrieMap[collection.BitSet, collection.BitSet]()

    def FCbOPar(xs: collection.BitSet, i: Int, ns: Array[collection.BitSet], nsMin: Int): Unit = {
      
      val n_i = n - i
      val queue = new scala.Array[collection.BitSet](n_i)
      val ms = new scala.Array[collection.BitSet](n_i)
      Array.copy(ns.array, ns.array.length - n_i, ms.array, 0, n_i)

      (i until n)
        .filterNot(xs.contains)
        .foreachPar { j =>
          val diff = ns(j - nsMin) diff xs
          if (diff.isEmpty || diff.min >= j) {
            val xs_j = xs + j
            if (inclusionIdeal(xs_j)) {
              val ys = closure(xs_j)
              if ((ys diff xs).min >= j)
                logger.tick()
                closures(ys) = xs_j
                queue(j - i) = ys
              else
                ms(j - i) = ys
            }
          }
        }

      (i until n).foreach { j =>
        val ys = queue(j - i)
        if (ys != null)
          FCbOPar(ys, j + 1, ms, i)
      }
    }

    FCbOPar(collection.BitSet.empty, 0, Array.fill(n)(collection.BitSet.empty), 0)

    logger.println()

    closures

  }

}
