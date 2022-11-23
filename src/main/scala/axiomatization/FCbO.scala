package de.tu_dresden.inf.lat
package axiomatization

import scala.collection.mutable
import util.control.Breaks.*
import de.tu_dresden.inf.lat.axiomatization.Util.{printExecutionTime, writeExecutionTime}

object FCbO {

  def computeAllClosures(n: Int,
                         closure: collection.BitSet => collection.BitSet,
                         inclusionIdeal: collection.BitSet => Boolean = _ => true)
                        (using logger: Logger)
//                        (using valueLogger: ValueLogger)
  : collection.Set[collection.BitSet] = {

    val closures = mutable.HashSet[collection.BitSet]()

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

    //    def equalsUpTo(xs: collection.BitSet, j: Int, ys: collection.BitSet): Boolean = {
    //      var result = true
    //      val xIt = xs.iterator
    //      val yIt = ys.iterator
    //      breakable {
    //
    //        val xHasNext = xIt.hasNext
    //        val yHasNext = yIt.hasNext
    //        if (xHasNext != yHasNext) {
    //          break
    //        }
    //      }
    //      result
    //    }

    //    @tailrec
        def FCbO(xs: collection.BitSet, i: Int, ns: Int => collection.BitSet): Unit = {
          val queue = mutable.Queue[(collection.BitSet, Int)]()
          val ms = mutable.HashMap[Int, collection.BitSet]()
          for (j <- i until n) {
            ms.put(j, ns(j))
            if (!xs(j)) {
              if (subsetOfUpTo(ns(j), j, xs)) {
                val xs_j = xs + j
                if (inclusionIdeal(xs_j)) {
//                  val ys = printExecutionTime({ closure(xs_j) }, "\rFCbO: " + closures.size + " closures computed so far.  Last closure computation took ")
//                  val ys = writeExecutionTime({ closure(xs_j) }, valueLogger.logValue)
                  val ys = closure(xs_j)
                  if (subsetOfUpTo(xs, j, ys) && subsetOfUpTo(ys, j, xs))
                    queue.enqueue((ys, j))
                  //            if (xs.forall(x => x >= j || ys(x)) && ys.forall(y => y >= j || xs(y)))
                  //              queue.enqueue((ys,j))
                  else ms.put(j, ys)
                }
              }
            }
          }
          while (queue.nonEmpty) {
    //        counter.tick()
            val (ys, j) = queue.dequeue()
            closures.addOne(ys)
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
