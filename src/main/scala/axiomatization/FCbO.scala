package de.tu_dresden.inf.lat
package axiomatization

import axiomatization.NestedParallelComputations.*
import axiomatization.Util.{printExecutionTime, writeExecutionTime}

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.jdk.CollectionConverters.*
import scala.util.control.Breaks.*


/**
 * Assumption: the empty set is a closure and it should not be included in the result.
 *
 * TODO: If needed in the future, revise the code in order to support the general case.
 */
object FCbOPar {

  def computeAllClosures(n: Int,
                         closure: collection.BitSet => collection.BitSet,
                         inclusionIdeal: collection.BitSet => Boolean = _ => true)
                        (using logger: Logger)
  : mutable.Map[collection.BitSet, collection.BitSet] = {

    //val closures = collection.concurrent.TrieMap[collection.BitSet, collection.BitSet]()
    val closures = java.util.concurrent.ConcurrentHashMap[collection.BitSet, collection.BitSet]().asScala

    def Step(xs: collection.BitSet, i: Int, ns: Array[collection.BitSet], nsMin: Int): Unit = {
      
      val n_i = n - i
      val queue = new scala.Array[collection.BitSet](n_i)
      val ms = new scala.Array[collection.BitSet](n_i)
      Array.copy(ns.array, ns.array.length - n_i, ms.array, 0, n_i)

      (i until n)
        .filterNot(xs.contains)
        .foreachPar { j =>
          val diff = ns(j - nsMin) diff xs
          if (diff.isEmpty || diff.min >= j)
            val xs_j = xs + j
            val ys = closure(xs_j)
            if ((ys diff xs).min >= j)
              if (inclusionIdeal(ys))
                logger.tick()
                closures(ys) = xs_j
                queue(j - i) = ys
            else
              ms(j - i) = ys
        }

      (i until n).foreach { j =>
        val ys = queue(j - i)
        if (ys != null)
          Step(ys, j + 1, ms, i)
      }
    }

    Step(collection.BitSet.empty, 0, Array.fill(n)(collection.BitSet.empty), 0)
    /**
     * TODO: For the general case (without the above assumption), first compute the closure of the empty set,
     * TODO: which yields the first closure, and add it to `closures`.  Then call `Step` as above but with the
     * TODO: first closure as first argument (in place of `collection.BitSet.empty`).
     */

    logger.println()

    closures

  }

}


/**
 * This variant of FCbO can be used if the attribute set
 * `{0,...,n-1}` is partitioned into two subsets of which the first is  `{0,...,k}` for some `k < n`,
 * and the goal is to compute all closures that contain at least one attribute of the first subset.
 */
object FCbOPar_Mod {

  def computeAllClosures(n: Int,
                         closure: collection.BitSet => collection.BitSet,
                         maxInFirstIteration: Int)
                        (using logger: Logger)
  : mutable.Map[collection.BitSet, collection.BitSet] = {

    //val closures = collection.concurrent.TrieMap[collection.BitSet, collection.BitSet]()
    val closures = java.util.concurrent.ConcurrentHashMap[collection.BitSet, collection.BitSet]().asScala

    def FirstStep(xs: collection.BitSet, i: Int, ns: Array[collection.BitSet], nsMin: Int): Unit = {

      val n_i = n - i
      val queue = new scala.Array[collection.BitSet](n_i)
      val ms = new scala.Array[collection.BitSet](n_i)
      Array.copy(ns.array, ns.array.length - n_i, ms.array, 0, n_i)

      (i until n)
        .filterNot(xs.contains)
        .foreachPar { j =>
          val diff = ns(j - nsMin) diff xs
          if (diff.isEmpty || diff.min >= j)
            val xs_j = xs + j
            val ys = closure(xs_j)
            if ((ys diff xs).min >= j)
              if (j <= maxInFirstIteration)
                logger.tick()
                closures(ys) = xs_j
                queue(j - i) = ys
            else
              ms(j - i) = ys
        }

      (i until maxInFirstIteration).foreach { j =>
        val ys = queue(j - i)
        if (ys != null)
          Step(ys, j + 1, ms, i)
      }
    }

    def Step(xs: collection.BitSet, i: Int, ns: Array[collection.BitSet], nsMin: Int): Unit = {

      val n_i = n - i
      val queue = new scala.Array[collection.BitSet](n_i)
      val ms = new scala.Array[collection.BitSet](n_i)
      Array.copy(ns.array, ns.array.length - n_i, ms.array, 0, n_i)

      (i until n)
        .filterNot(xs.contains)
        .foreachPar { j =>
          val diff = ns(j - nsMin) diff xs
          if (diff.isEmpty || diff.min >= j)
            val xs_j = xs + j
            val ys = closure(xs_j)
            if ((ys diff xs).min >= j)
              logger.tick()
              closures(ys) = xs_j
              queue(j - i) = ys
            else
              ms(j - i) = ys
        }

      (i until n).foreach { j =>
        val ys = queue(j - i)
        if (ys != null)
          Step(ys, j + 1, ms, i)
      }
    }

//    Step(collection.BitSet.empty, 0, Array.fill(n)(collection.BitSet.empty), 0)
    FirstStep(collection.BitSet.empty, 0, Array.fill(n)(collection.BitSet.empty), 0)

    logger.println()

    closures

  }

}
