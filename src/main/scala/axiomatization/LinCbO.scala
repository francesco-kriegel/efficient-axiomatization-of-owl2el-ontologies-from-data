package de.tu_dresden.inf.lat
package axiomatization

import axiomatization.InducedFormalContext
import axiomatization.Util.measureExecutionTime

import org.checkerframework.checker.units.qual.A
import org.semanticweb.owlapi.model.{OWLClass, OWLObjectProperty, OWLObjectSomeValuesFrom}

import java.io.{BufferedReader, File, FileReader}
import scala.collection.immutable.BitSet
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.{break, breakable}


object LinCbO {

  def computeCanonicalBase(cxt: InducedFormalContext, inclusionIdeal: collection.BitSet => Boolean = _ => true)(using logger: Logger):
  mutable.ArrayBuffer[BitImplication] = {

    val n = cxt.activeAttributes.length

    val imps = new Array[mutable.BitSet](n)
    (0 until n).foreach(i => imps(i) = mutable.BitSet())

    val canonicalBase = mutable.ArrayBuffer[BitImplication]()

    def linClosure(set: mutable.BitSet, y: Int, update: mutable.BitSet, lastCounts: Array[Int]):
    Option[(mutable.BitSet, Array[Int])] = {
      val x = mutable.BitSet.fromSpecific(set)
      val setCounts = new Array[Int](canonicalBase.size)
      (0 until canonicalBase.length).foreach(imp => {
        setCounts(imp) = if (imp < lastCounts.length) lastCounts(imp) else (canonicalBase(imp)._1 diff set).size
      })
      var fail = false
      breakable {
        while (update.nonEmpty) {
          val m = update.min
          update.remove(m)
          imps(m).foreach(imp => {
            setCounts(imp) = setCounts(imp) - 1
            if (setCounts(imp) <= 0) {
              val diff = canonicalBase(imp)._2 diff x
              if (diff.nonEmpty && diff.min < y)
                fail = true
                break
              x.addAll(diff)
              update.addAll(diff)
            }
          })
        }
      }
      if (fail) None
      else if (x.size == n) Some((x, Array.empty))
      else Some((x, setCounts))
    }

    val counts = mutable.ArrayBuffer[Int]()

    def addPseudoIntent(ant: mutable.BitSet, cons: mutable.BitSet): Unit = {
      val imp = (ant, cons)
      val index = canonicalBase.length
      canonicalBase.addOne(imp)
      counts.append(ant.size)
      ant.foreach(j => imps(j).addOne(index))
      logger.print("\rLinCbO: " + canonicalBase.length + " implications computed so far.")
    }

    def Step(current: mutable.BitSet, y: Int, added: mutable.BitSet, lastCounts: Array[Int]): Unit = {
      if (inclusionIdeal(current)) {
        val linCl = linClosure(current, y, added, lastCounts)
        if (linCl.nonEmpty) {
          val (psClosure, count) = linCl.get
          val closure = cxt.closure(psClosure)
          if (inclusionIdeal(closure)) {
            val diff = closure diff psClosure
            if (diff.nonEmpty) {
              addPseudoIntent(psClosure, closure)
              if (diff.min > y) {
                Step(closure, y, diff, count)
              }
            } else {
              for (i <- (n - 1) to (y + 1) by -1) {
                if (!psClosure.contains(i)) {
                  val next = psClosure + i
                  val nDiff = mutable.BitSet(i)
                  Step(next, i, nDiff, if (next.size == 1) counts.toArray else count)
                }
              }
            }
          }
        }
      }
    }

    Step(mutable.BitSet.empty, -1, mutable.BitSet.empty, counts.toArray)

    logger.println()

    canonicalBase

  }

}

object LinCbO_WithBackgroundImplications {

  def computeCanonicalBase(cxt: InducedFormalContext,
                           backgroundImplications: collection.Iterable[BitImplication],
                           nAtts: Int = -1,
                           inclusionIdeal: collection.BitSet => Boolean = _ => true
                          )(using logger: Logger): mutable.HashSet[BitImplication] = {

    val m = cxt.activeAttributes.length
    val n = if (nAtts < m) m else nAtts
    val bitsAdditionalAttributes = BitSet.fromSpecific(m until n)

    val first = mutable.BitSet()
    backgroundImplications.foreach(imp => {
      if (imp._1.isEmpty)
        first.addAll(imp._2)
    })

    val allImplications = mutable.ArrayBuffer[BitImplication]()
    val canonicalBase = mutable.HashSet[BitImplication]()
    val counts = mutable.ArrayBuffer[Int]()
    val imps = new Array[mutable.BitSet](n)
    (0 until n).foreach(i => imps(i) = mutable.BitSet())
    var j = 0
    backgroundImplications.foreach(imp => {
      allImplications.addOne(imp)
      counts.append((imp._1 diff first).size)
      imp._1.foreach(i => {
        imps(i).addOne(j)
      })
      j += 1
    })

    def linClosure(set: mutable.BitSet, y: Int, update: mutable.BitSet, lastCounts: Array[Int]):
    Option[(mutable.BitSet, Array[Int])] = {
      val x = mutable.BitSet.fromSpecific(set)
      val setCounts = new Array[Int](allImplications.size)
      (0 until allImplications.length).foreach(imp => {
        setCounts(imp) = if (imp < lastCounts.length) lastCounts(imp) else (allImplications(imp)._1 diff set).size
      })
      var fail = false
      breakable {
        while (update.nonEmpty) {
          val m = update.min
          update.remove(m)
          imps(m).foreach(imp => {
            setCounts(imp) = setCounts(imp) - 1
            if (setCounts(imp) <= 0) {
              val diff = allImplications(imp)._2 diff x
              if (diff.nonEmpty && diff.min < y)
                fail = true
                break
              x.addAll(diff)
              update.addAll(diff)
            }
          })
        }
      }
      if (fail) None
      //      else if (x.size == n) Some((x, Array.empty))
      else Some((x, setCounts))
    }

    def addPseudoIntent(_ant: mutable.BitSet, _cons: mutable.BitSet): Unit = {
      val ant = _ant intersect cxt.bitsActiveAttributes
      val cons = _cons intersect cxt.bitsActiveAttributes
      val imp = ant -> cons
      val index = allImplications.length
      allImplications.addOne(imp)
      canonicalBase.addOne(imp)
      counts.append(ant.size)
      ant.foreach(j => imps(j).addOne(index))
      logger.print("\rLinCbO: " + canonicalBase.size + " implications computed so far.")
    }

    def Step(current: mutable.BitSet, y: Int, added: mutable.BitSet, lastCounts: Array[Int]): Unit = {
      if (inclusionIdeal(current)) {
        val linCl = linClosure(current, y, added, lastCounts)
        if (linCl.nonEmpty) {
          val (psClosure, count) = linCl.get

          val left = psClosure intersect cxt.bitsActiveAttributes
          val right = psClosure intersect bitsAdditionalAttributes
          val closure = cxt.closure(left) union right

          if (inclusionIdeal(closure)) {
            val diff = closure diff psClosure
            if (diff.nonEmpty) {
              // addPseudoIntent(psClosure, closure)
              addPseudoIntent(psClosure, diff)
              if (diff.min > y) {
                Step(closure, y, diff, count)
              }
            } else {
              for (i <- (m - 1) to (y + 1) by -1) {
                if (!psClosure.contains(i)) {
                  val next = psClosure + i
                  val nDiff = mutable.BitSet(i)
                  Step(next, i, nDiff, if (next.size == 1) counts.toArray else count)
                }
              }
            }
          }
        }
      }
    }

    Step(first, -1, first, counts.toArray)

    logger.println()

    canonicalBase

  }

}
/**
 * This version uses the LCM-inspired pruning.
 */
object LinCbO_WithPruning_WithBackgroundImplications {

  def computeCanonicalBase(cxt: InducedFormalContext,
                           backgroundImplications: collection.Iterable[BitImplication],
                           nAtts: Int = -1,
                           inclusionIdeal: collection.BitSet => Boolean = _ => true
                          )(using logger: Logger): mutable.HashSet[BitImplication] = {

    val m = cxt.activeAttributes.length
    val n = if (nAtts < m) m else nAtts
    val bitsAdditionalAttributes = BitSet.fromSpecific(m until n)

    val first = mutable.BitSet()
    backgroundImplications.foreach(imp => {
      if (imp._1.isEmpty)
        first.addAll(imp._2)
    })

    val allImplications = mutable.ArrayBuffer[BitImplication]()
    val canonicalBase = mutable.HashSet[BitImplication]()
    val counts = mutable.ArrayBuffer[Int]()
    val imps = new Array[mutable.BitSet](n)
    (0 until n).foreach(i => imps(i) = mutable.BitSet())
    var j = 0
    backgroundImplications.foreach(imp => {
      allImplications.addOne(imp)
      counts.append((imp._1 diff first).size)
      imp._1.foreach(i => {
        imps(i).addOne(j)
      })
      j += 1
    })
    val pruningData = new Array[Int](n)
    (0 until n).foreach(pruningData(_) = -1)
    val pruningStack = new mutable.Stack[Int]()

    def linClosure(set: mutable.BitSet, y: Int, update: mutable.BitSet, lastCounts: Array[Int]):
    (mutable.BitSet, Array[Int], Int) = {
      val x = mutable.BitSet.fromSpecific(set)
      val setCounts = new Array[Int](allImplications.size)
      (0 until allImplications.length).foreach(imp => {
        setCounts(imp) = if (imp < lastCounts.length) lastCounts(imp) else (allImplications(imp)._1 diff set).size
      })
      var fail = -1
      breakable {
        while (update.nonEmpty) {
          val m = update.min
          update.remove(m)
          imps(m).foreach(imp => {
            setCounts(imp) = setCounts(imp) - 1
            if (setCounts(imp) <= 0) {
              val diff = allImplications(imp)._2 diff x
              if (diff.nonEmpty)
                val firstBit = diff.min
                if (firstBit < y)
                  fail = firstBit
                  break
              x |= diff
              update |= diff
            }
          })
        }
      }
      (x, setCounts, fail)
    }

    def addPseudoIntent(_ant: mutable.BitSet, _cons: mutable.BitSet): Unit = {
      val ant = _ant intersect cxt.bitsActiveAttributes
      val cons = _cons intersect cxt.bitsActiveAttributes
      val imp = ant -> cons
      val index = allImplications.length
      allImplications.addOne(imp)
      canonicalBase.addOne(imp)
      counts.append(ant.size)
      ant.foreach(j => imps(j).addOne(index))
      logger.print("\rLinCbO: " + canonicalBase.size + " implications computed so far.")
    }

    def Step(current: mutable.BitSet, y: Int, added: mutable.BitSet, lastCounts: Array[Int]): Int = {
//      if (inclusionIdeal(current)) {
        val stackSize = pruningStack.size
        var result = -1
        val (psClosure, count, fail) = linClosure(current, y, added, lastCounts)
        if (fail > -1) {
          result = fail
        } else {
          val left = psClosure intersect cxt.bitsActiveAttributes
          val right = psClosure intersect bitsAdditionalAttributes
          val closure = cxt.closure(left) union right
          if (inclusionIdeal(closure)) {
            val diff = closure diff psClosure
            if (diff.nonEmpty) {
              // addPseudoIntent(psClosure, closure)
              addPseudoIntent(psClosure, diff)
              val firstBit = diff.min
              if (firstBit > y)
                result = Step(closure, y, diff, count)
              else
                result = firstBit
            } else {
              for (i <- (m - 1) to (y + 1) by -1) {
                if (!(psClosure contains i)) {
                  val next = psClosure + i
                  val nDiff = mutable.BitSet(i)
                  if (pruningData(i) == -1 || (next contains pruningData(i))) {
                    result = Step(next, i, nDiff, if next.size == 1 then counts.toArray else count)
                    if (result > -1 && result < i) {
                      pruningStack.push(i)
                      pruningData(i) = result
                    }
                  }
                }
              }
              result = -1
            }
          }
          while (pruningStack.size > stackSize) {
            val i = pruningStack.pop()
            pruningData(i) = -1
          }
        }
      result
//      }
    }

    Step(first, -1, first, counts.toArray)

    logger.println()

    canonicalBase

  }

}

/**
 * @deprecated The above version without explicit tail recursion is faster, it needs only half of the computation time.
 */
@Deprecated(forRemoval = true)
object LinCbO_WithBackgroundImplications_TailRec {

  def computeCanonicalBase(cxt: InducedFormalContext,
                           backgroundImplications: collection.Iterable[BitImplication],
                           nAtts: Int = -1,
                           inclusionIdeal: collection.BitSet => Boolean = _ => true
                          )(using logger: Logger): mutable.HashSet[BitImplication] = {

    val m = cxt.activeAttributes.length
    val n = if (nAtts < m) m else nAtts
    val bitsAdditionalAttributes = BitSet.fromSpecific(m until n)

    val first = mutable.BitSet()
    backgroundImplications.foreach(imp => {
      if (imp._1.isEmpty)
        first.addAll(imp._2)
    })

    val allImplications = mutable.ArrayBuffer[BitImplication]()
    val canonicalBase = mutable.HashSet[BitImplication]()
    val counts = mutable.ArrayBuffer[Int]()
    val imps = new Array[mutable.BitSet](n)
    (0 until n).foreach(i => imps(i) = mutable.BitSet())
    var j = 0
    backgroundImplications.foreach(imp => {
      allImplications.addOne(imp)
      counts.append((imp._1 diff first).size)
      imp._1.foreach(i => {
        imps(i).addOne(j)
      })
      j += 1
    })

    def linClosure(set: mutable.BitSet, y: Int, update: mutable.BitSet, lastCounts: Array[Int]):
    Option[(mutable.BitSet, Array[Int])] = {
      val x = mutable.BitSet.fromSpecific(set)
      val setCounts = new Array[Int](allImplications.size)
      (0 until allImplications.length).foreach(imp => {
        setCounts(imp) = if (imp < lastCounts.length) lastCounts(imp) else (allImplications(imp)._1 diff set).size
      })
      var fail = false
      breakable {
        while (update.nonEmpty) {
          val m = update.min
          update.remove(m)
          imps(m).foreach(imp => {
            setCounts(imp) = setCounts(imp) - 1
            if (setCounts(imp) <= 0) {
              val diff = allImplications(imp)._2 diff x
              if (diff.nonEmpty && diff.min < y)
                fail = true
                break
              x.addAll(diff)
              update.addAll(diff)
            }
          })
        }
      }
      if (fail) None
      //      else if (x.size == n) Some((x, Array.empty))
      else Some((x, setCounts))
    }

    def addPseudoIntent(_ant: mutable.BitSet, _cons: mutable.BitSet): Unit = {
      val ant = _ant intersect cxt.bitsActiveAttributes
      val cons = _cons intersect cxt.bitsActiveAttributes
      val imp = ant -> cons
      val index = allImplications.length
      allImplications.addOne(imp)
      canonicalBase.addOne(imp)
      counts.append(ant.size)
      ant.foreach(j => imps(j).addOne(index))
      logger.print("\rLinCbO: " + canonicalBase.size + " implications computed so far.")
    }

    import scala.util.control.TailCalls._

    def Step(current: mutable.BitSet, y: Int, added: mutable.BitSet, lastCounts: Array[Int]): TailRec[Unit] = {
      var tailRecursion: TailRec[Unit] = done(())
      if (inclusionIdeal(current)) {
        val linCl = linClosure(current, y, added, lastCounts)
        if (linCl.nonEmpty) {
          val (psClosure, count) = linCl.get

          val left = psClosure intersect cxt.bitsActiveAttributes
          val right = psClosure intersect bitsAdditionalAttributes
          val closure = cxt.closure(left) union right

          if (inclusionIdeal(closure)) {
            val diff = closure diff psClosure
            if (diff.nonEmpty) {
              // addPseudoIntent(psClosure, closure)
              addPseudoIntent(psClosure, diff)
              if (diff.min > y) {
                tailRecursion = tailcall(Step(closure, y, diff, count))
              }
            } else {
              var isFirst = true
              for (i <- (m - 1) to (y + 1) by -1) {
                if (!psClosure.contains(i)) {
                  val next = psClosure + i
                  val nDiff = mutable.BitSet(i)
                  if (isFirst)
                    tailRecursion = tailcall(Step(next, i, nDiff, if (next.size == 1) counts.toArray else count))
                    isFirst = false
                  else
                    tailRecursion = tailRecursion flatMap { _ => tailcall(Step(next, i, nDiff, if (next.size == 1) counts.toArray else count)) }
                }
              }
            }
          }
        }
      }
      tailRecursion
    }

    Step(first, -1, first, counts.toArray).result

    logger.println()

    canonicalBase

  }

}

/**
 * @deprecated This version computes an incomplete implication base.
 */
@Deprecated(forRemoval = true)
object LinCbO_WithBackgroundImplications_Par {

  def computeCanonicalBase(cxt: InducedFormalContext,
                           backgroundImplications: collection.Iterable[BitImplication],
                           nAtts: Int = -1,
                           inclusionIdeal: collection.BitSet => Boolean = _ => true
                          )(using logger: Logger): collection.Set[BitImplication] = {

    val m = cxt.activeAttributes.length
    val n = if (nAtts < m) m else nAtts
    val bitsAdditionalAttributes = BitSet.fromSpecific(m until n)

    val first = mutable.BitSet()
    backgroundImplications.foreach(imp => {
      if (imp._1.isEmpty)
        first.addAll(imp._2)
    })

    import scala.jdk.CollectionConverters._

    val allImplications = java.util.concurrent.CopyOnWriteArrayList[BitImplication]().asScala //mutable.ArrayBuffer[BitImplication]()
    val canonicalBase = java.util.concurrent.ConcurrentHashMap[BitImplication, Unit]().keySet(()).asScala //mutable.HashSet[BitImplication]()
    val counts = java.util.concurrent.CopyOnWriteArrayList[Int]().asScala //mutable.ArrayBuffer[Int]()
    val imps = new Array[mutable.Set[Int]](n) //new Array[mutable.BitSet](n)
    (0 until n).foreach(i => imps(i) = java.util.concurrent.ConcurrentHashMap[Int, Unit]().keySet(()).asScala) //(0 until n).foreach(i => imps(i) = mutable.BitSet())
    var j = 0
    backgroundImplications.foreach(imp => {
      allImplications.addOne(imp)
      counts.append((imp._1 diff first).size)
      imp._1.foreach(i => {
        imps(i).addOne(j)
      })
      j += 1
    })

    def linClosure(set: mutable.BitSet, y: Int, update: mutable.BitSet, lastCounts: Array[Int]):
    Option[(mutable.BitSet, Array[Int])] = {
      val x = mutable.BitSet.fromSpecific(set)
      val setCounts = new Array[Int](allImplications.size)
      (0 until allImplications.length).foreach(imp => {
        setCounts(imp) = if (imp < lastCounts.length) lastCounts(imp) else (allImplications(imp)._1 diff set).size
      })
      var fail = false
      breakable {
        while (update.nonEmpty) {
          val m = update.min
          update.remove(m)
          imps(m).foreach(imp => {
            setCounts(imp) = setCounts(imp) - 1
            if (setCounts(imp) <= 0) {
              val diff = allImplications(imp)._2 diff x
              if (diff.nonEmpty && diff.min < y)
                fail = true
                break
              x.addAll(diff)
              update.addAll(diff)
            }
          })
        }
      }
      if (fail) None
      //      else if (x.size == n) Some((x, Array.empty))
      else Some((x, setCounts))
    }

    def addPseudoIntent(_ant: mutable.BitSet, _cons: mutable.BitSet): Unit = {
      val ant = _ant intersect cxt.bitsActiveAttributes
      val cons = _cons intersect cxt.bitsActiveAttributes
      val imp = ant -> cons
      canonicalBase.addOne(imp)
      canonicalBase.synchronized {
        val index = allImplications.length
        allImplications.addOne(imp)
        counts.append(ant.size)
        ant.foreach(j => imps(j).addOne(index))
      }
      logger.print("\rLinCbO: " + canonicalBase.size + " implications computed so far.")
    }

    import de.tu_dresden.inf.lat.axiomatization.NestedParallelComputations._

    type SingleStep = (mutable.BitSet, Int, mutable.BitSet, Array[Int])
    val nextSteps = java.util.concurrent.ConcurrentLinkedQueue[SingleStep]()

    def Step(current: mutable.BitSet, y: Int, added: mutable.BitSet, lastCounts: Array[Int]): Unit = {
      if (inclusionIdeal(current)) {
        val linCl = linClosure(current, y, added, lastCounts)
        if (linCl.nonEmpty) {
          val (psClosure, count) = linCl.get

          val left = psClosure intersect cxt.bitsActiveAttributes
          val right = psClosure intersect bitsAdditionalAttributes
          val closure = cxt.closure(left) union right

          if (inclusionIdeal(closure)) {
            val diff = closure diff psClosure
            if (diff.nonEmpty) {
              // addPseudoIntent(psClosure, closure)
              addPseudoIntent(psClosure, diff)
              if (diff.min > y) {
                nextSteps add (closure, y, diff, count)
              }
            } else {
              ((m - 1) to (y + 1) by -1) foreach { i =>
                if (!psClosure.contains(i)) {
                  val next = psClosure + i
                  val nDiff = mutable.BitSet(i)
                  nextSteps add (next, i, nDiff, if (next.size == 1) counts.toArray else count)
                }
              }
            }
          }
        }
      }
    }

    nextSteps add (first, -1, first, counts.toArray)

    while (!nextSteps.isEmpty) {
      val futures = mutable.Queue[java.util.concurrent.RecursiveAction]()
      while (!nextSteps.isEmpty) {
        val nextStep = nextSteps.poll()
        futures addOne { () => Step.tupled(nextStep) }
      }
      futures foreach { FORK_JOIN_POOL.execute }
      futures foreach { _.quietlyJoin }
    }

    logger.println()

    canonicalBase

  }

}
