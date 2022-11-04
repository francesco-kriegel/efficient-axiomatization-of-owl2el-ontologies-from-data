package de.tu_dresden.inf.lat
package axiomatization

import scala.collection.immutable.BitSet
import scala.collection.mutable
import util.control.Breaks.*

import de.tu_dresden.inf.lat.axiomatization.Util.measureExecutionTime

object FCbO {

  def computeAllClosures(n: Int, closure: BitSet => BitSet): collection.Set[BitSet] = {

    val closures = mutable.HashSet[BitSet]()

    def subsetOfUpTo(xs: BitSet, j: Int, ys: BitSet): Boolean = {
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

    //    def equalsUpTo(xs: BitSet, j: Int, ys: BitSet): Boolean = {
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

    ////    @tailrec
    //    def FCbO(xs: BitSet, i: Int, ns: Int => BitSet): Unit = {
    //      val queue = mutable.Queue[(BitSet, Int)]()
    //      val ms = mutable.HashMap[Int, BitSet]()
    //      for (j <- i to n) {
    //        ms.put(j, ns(j))
    //        if (!xs(j)) {
    //          if (subsetOfUpTo(ns(j), j, xs)) {
    //            val ys = closure(xs + j)
    //            if (subsetOfUpTo(xs, j, ys) && subsetOfUpTo(ys, j, xs))
    //              queue.enqueue((ys, j))
    ////            if (xs.forall(x => x >= j || ys(x)) && ys.forall(y => y >= j || xs(y)))
    ////              queue.enqueue((ys,j))
    //            else ms.put(j, ys)
    //          }
    //        }
    //      }
    //      while (queue.nonEmpty) {
    ////        counter.tick()
    //        val (ys, j) = queue.dequeue()
    //        closures.addOne(ys)
    //        FCbO(ys, j+1, ms)
    //      }
    //    }
    //
    //    FCbO(BitSet.empty, 1, _ => BitSet.empty)

    val outerDeque = mutable.ArrayDeque[(BitSet, Int, Int => BitSet)]()
    outerDeque.prepend((BitSet.empty, 1, (_: Int) => BitSet.empty))

    while (outerDeque.nonEmpty) {
      //      print("\rqueue size: " + outerDeque.size)
      val (xs, i, ns) = outerDeque.removeHead()
      closures.addOne(xs)
      val innerQueue = mutable.Queue[(BitSet, Int)]()
      val ms = mutable.HashMap[Int, BitSet]()
      for (j <- i to n) {
        //        print("\rqueue size: " + outerDeque.size + "  ---  " + j)
        ms.put(j, ns(j))
        if (!xs(j)) {
          if (subsetOfUpTo(ns(j), j, xs)) {
            val ys = measureExecutionTime({
              closure(xs + j)
            }, "\router queue size: " + outerDeque.size + "  ---  inner queue position: " + j + "  ---  closure computation took ")
            //            val ys = closure(xs + j)
            if (subsetOfUpTo(xs, j, ys) && subsetOfUpTo(ys, j, xs))
              innerQueue.enqueue((ys, j))
            else ms.put(j, ys)
          }
        }
      }
      //      while (innerQueue.nonEmpty) {
      //        val (ys, j) = innerQueue.dequeue()
      //        outerDeque.prepend((ys, j + 1, ms))
      //      }
      outerDeque.prependAll(innerQueue.map((ys, j) => (ys, j + 1, ms)))
    }

    closures

  }

}
