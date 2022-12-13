package de.tu_dresden.inf.lat
package axiomatization

import scala.collection.mutable
//import scala.jdk.CollectionConverters.*
import collection.parallel.CollectionConverters.*
import util.control.Breaks.*
import de.tu_dresden.inf.lat.axiomatization.Util.{printExecutionTime, writeExecutionTime}


object FCbOPar {

  def computeAllClosures(n: Int,
                         closure: collection.BitSet => collection.BitSet,
                         inclusionIdeal: collection.BitSet => Boolean = _ => true)
                        (using logger: Logger)
  //                        (using valueLogger: ValueLogger)
  : mutable.HashMap[collection.BitSet, collection.BitSet] = {

    val closures = mutable.HashMap[collection.BitSet, collection.BitSet]()

    def FCbOPar(xs: collection.BitSet, i: Int, ns: Int => collection.BitSet): Unit = {
      // val queue = mutable.Queue[(collection.BitSet, collection.BitSet, Int)]()
      val queue = java.util.concurrent.ConcurrentLinkedQueue[(collection.BitSet, collection.BitSet, Int)]()//.asScala
      // val ms = mutable.HashMap[Int, collection.BitSet]()
      val ms = collection.concurrent.TrieMap[Int, collection.BitSet]()
      // for (j <- i until n) {
      (i until n).par.foreach { j =>
        ms.put(j, ns(j))
        if (!xs(j)) {
          val diff = ns(j) diff xs
          if (diff.isEmpty || diff.min >= j) {
            val xs_j = xs + j
            if (inclusionIdeal(xs_j)) {
              val ys = closure(xs_j)
              if ((ys diff xs).min >= j)
                logger.tick()
                // queue.enqueue((ys, xs_j, j))
                queue.add((ys, xs_j, j))
              else ms.put(j, ys)
            }
          }
        }
      }
      // while (queue.nonEmpty) {
      while (!queue.isEmpty) {
        // val (ys, gen, j) = queue.dequeue()
        val (ys, gen, j) = queue.poll()
        closures.update(ys, gen)
        FCbOPar(ys, j+1, ms)
      }
    }

    FCbOPar(collection.BitSet.empty, 0, _ => collection.BitSet.empty)

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
