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

    def FCbOPar2(xs: collection.BitSet, i: Int, ns: Array[collection.BitSet], nsMin: Int): Unit = {
      
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
          FCbOPar2(ys, j + 1, ms, i)
      }
    }

    FCbOPar2(collection.BitSet.empty, 0, Array.fill(n)(collection.BitSet.empty), 0)

    logger.println()

    closures

  }

}



object FCbO {

  def computeAllClosures(n: Int,
                         closure: collection.BitSet => collection.BitSet,
                         inclusionIdeal: collection.BitSet => Boolean = _ => true)
                        (using logger: Logger)
//                        (using valueLogger: ValueLogger)
  : mutable.HashMap[collection.BitSet, collection.BitSet] = {

    val closures = mutable.HashMap[collection.BitSet, collection.BitSet]()

    def subsetOfUpTo(xs: collection.BitSet, j: Int, ys: collection.BitSet): Boolean = {
      var result = true
      val it = xs.iterator
      breakable {
        while (it.hasNext) {
          val x = it.next()
          if (x < j) {
            if (!ys(x)) {
              result = false
              break
            }
          } else {
            break
          }
        }
      }
      result
    }

    //    @tailrec
        def FCbO(xs: collection.BitSet, i: Int, ns: Int => collection.BitSet): Unit = {
          val queue = mutable.Queue[(collection.BitSet, collection.BitSet, Int)]()
          val ms = mutable.HashMap[Int, collection.BitSet]()
          for (j <- i until n) {
            ms.put(j, ns(j))
            if (!xs(j)) {
              // if (subsetOfUpTo(ns(j), j, xs)) {
              val diff = ns(j) diff xs
              if (diff.isEmpty || diff.min >= j) {
                val xs_j = xs + j
                if (inclusionIdeal(xs_j)) {
                  // val ys = printExecutionTime({ closure(xs_j) }, "\rFCbO: " + closures.size + " closures computed so far.  Last closure computation took ")
                  // val ys = writeExecutionTime({ closure(xs_j) }, valueLogger.logValue)
                  val ys = closure(xs_j)
//                  Bug Fixing (wasn't here):
//                  if (!(xs_j subsetOf ys))
//                    throw RuntimeException("The closure operator is not extensive.")
                  // if (xs.forall(x => x >= j || ys(x)) && ys.forall(y => y >= j || xs(y)))
                  // Since xs ⊆ xs+j ⊆ closure(xs+j) = ys, the first test subsetOfUpTo(xs, j, ys) is always true
                  // if (subsetOfUpTo(xs, j, ys) && subsetOfUpTo(ys, j, xs))
                  // if (subsetOfUpTo(ys, j, xs))
//                  Bug Fixing:
//                  if ((ys diff xs).isEmpty)
//                    println("xs: " + xs)
//                    println("xs_j: " + xs_j)
//                    println("ys: " + ys)
//                    println("ys diff xs: " + (ys diff xs))
//                    println("ys filterNot xs: " + (ys filterNot xs))
//                    println("immutable diff: " + (scala.collection.immutable.BitSet.fromSpecific(ys) diff scala.collection.immutable.BitSet.fromSpecific(xs)))
//                    println("mutable diff: " + (scala.collection.mutable.BitSet.fromSpecific(ys) diff scala.collection.mutable.BitSet.fromSpecific(xs)))
//                    throw RuntimeException()
                  if ((ys diff xs).min >= j)
                    logger.tick()
                    queue.enqueue((ys, xs_j, j))
                  else ms.put(j, ys)
                }
              }
            }
          }
          while (queue.nonEmpty) {
            val (ys, gen, j) = queue.dequeue()
//            closures.addOne(ys)
//            if (closures.contains(ys))
//              println("Already there.")
//              if (closures.apply(ys).size > gen.size)
//                closures.update(ys, gen)
//            else
//              closures.update(ys, gen)
            closures.update(ys, gen)
            FCbO(ys, j+1, ms)
          }
        }

        FCbO(collection.BitSet.empty, 0, _ => collection.BitSet.empty)

//    val outerDeque = mutable.ArrayDeque[(collection.BitSet, Int, Int => collection.BitSet)]()
////    outerDeque.prepend((collection.BitSet.empty, 1, (_: Int) => collection.BitSet.empty))
//    outerDeque.prepend((collection.BitSet.empty, 0, (_: Int) => collection.BitSet.empty))
//
//    while (outerDeque.nonEmpty) {
//      //      print("\rqueue size: " + outerDeque.size)
//      val (xs, i, ns) = outerDeque.removeHead()
//      closures.addOne(xs)
//      val innerQueue = mutable.Queue[(collection.BitSet, Int)]()
//      val ms = mutable.HashMap[Int, collection.BitSet]()
////      for (j <- i to n) {
//      for (j <- i to (n - 1)) {
//        //        print("\rqueue size: " + outerDeque.size + "  ---  " + j)
//        ms.put(j, ns(j))
//        if (!xs(j)) {
//          if (subsetOfUpTo(ns(j), j, xs)) {
//            val xs_j = xs + j
//            if (inclusionIdeal(xs_j)) {
//              val ys = measureExecutionTime({
//                closure(xs_j)
//              }, "\router queue size: " + outerDeque.size + "  ---  inner queue position: " + j + "  ---  closure computation took ")
//              //            val ys = closure(xs + j)
//              if (subsetOfUpTo(xs, j, ys) && subsetOfUpTo(ys, j, xs))
//                innerQueue.enqueue((ys, j))
//              else ms.put(j, ys)
//            }
//          }
//        }
//      }
//      //      while (innerQueue.nonEmpty) {
//      //        val (ys, j) = innerQueue.dequeue()
//      //        outerDeque.prepend((ys, j + 1, ms))
//      //      }
//      outerDeque.prependAll(innerQueue.map((ys, j) => (ys, j + 1, ms)))
//    }

    logger.println()

    closures

  }

}
