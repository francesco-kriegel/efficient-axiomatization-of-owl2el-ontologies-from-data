
package de.tu_dresden.inf.lat
package axiomatization

import scala.collection.mutable
import scala.collection.parallel.CollectionConverters.*

object Util {

  def fixedPoint[A](monotoneFunction: A ⇒ A, equivalencePredicate: (A, A) ⇒ Boolean = (a1: A, a2: A) ⇒ a1 equals a2) =
    (a: A) ⇒ {
      var result = a
      var continue = true
      while (continue) {
        val next = monotoneFunction(result)
        continue = !equivalencePredicate(result, next)
        result = next
      }
      result
    }

  def cartesianProduct[T](sets: collection.Set[collection.Set[T]]): collection.Set[collection.Set[T]] = {
    if (sets.isEmpty)
      Set.empty[collection.Set[T]]
    else {
      val seed: collection.Set[collection.Set[T]] = Set(Set())
      val foldFunction: ((collection.Set[collection.Set[T]], collection.Set[T]) ⇒ collection.Set[collection.Set[T]]) = {
        (xs: collection.Set[collection.Set[T]], ys: collection.Set[T]) ⇒ xs.flatMap((x: collection.Set[T]) ⇒ ys.map((y: T) ⇒ x + y))
      }
      sets.foldLeft(seed)(foldFunction)
    }
  }

//  implicit class LocalBitSet(bs: collection.BitSet) {
//    def diffOrReportBug(other: collection.BitSet): collection.BitSet = {
//      val diff = bs diff other
//      val expected = collection.mutable.BitSet.fromSpecific(bs).subtractAll(other)
//      if (diff equals expected)
//        diff
//      else
//        println("BitSet 1: " + bs)
//        println("BitSet 2: " + other)
//        println("Computed Difference: " + diff)
//        println("Expected Difference: " + expected)
//        throw RuntimeException()
//    }
//  }

  implicit class LocalSet[A](set: collection.Set[A]) {
    def strictSubsetOf(other: collection.Set[A]): Boolean = {
      (set subsetOf other) && (set.size < other.size)
    }
  }

  // TODO: Compute in parallel
//  def intersectionOfBitSets(it: Iterator[mutable.BitSet], size: Int, domain: collection.BitSet): mutable.BitSet = {
//    if (it.isEmpty)
////      throw new IllegalArgumentException("Cannot compute empty intersection.")
//      mutable.BitSet.fromSpecific(domain)
//    else {
//      val elems = new Array[Array[Long]](size)
//      var i = 0
//      it.foreach(set => {
//        elems(i) = set.toBitMask
//        i += 1
//      })
//      val nwords = elems.map(_.length).min
//      val newElems = new Array[Long](nwords)
//      (0 until nwords).par.foreach(i => {
//        newElems(i) = elems(0)(i)
//        (1 until elems.length).foreach(n => newElems(i) &= elems(n)(i))
//      })
//      new mutable.BitSet(newElems)
//    }
//  }

  def intersectionOfBitSets(it: Iterator[mutable.BitSet], domain: collection.BitSet): mutable.BitSet = {
    if (it.isEmpty)
      mutable.BitSet.fromSpecific(domain)
    else
      it.reduceLeft(_ & _)
  }

  implicit class LocalMultiMap[A, B](map: collection.Map[A, collection.mutable.Set[B]] with collection.mutable.MultiMap[A, B]) {
    def foreachBinding(f: (A, B) ⇒ Unit): Unit = {
      map.foreach({ case (key, values) ⇒ values.foreach(value ⇒ f(key, value)) })
    }
  }

  implicit class LocalMultiMapBits[A](map: mutable.Map[A, mutable.BitSet]) {
    def addBinding(key: A, value: Int): Unit = {
      map.getOrElseUpdate(key, mutable.BitSet()).addOne(value)
    }
    def removeBinding(key: A, value: Int): Unit = {
      map.get(key).foreach(_.remove(value))
    }
    def foreachBinding(f: (A, Int) ⇒ Unit): Unit = {
      map.foreach({ case (key, values) ⇒ values.foreach(value ⇒ f(key, value)) })
    }
  }

//  @inline
  def measureExecutionTime[T](code: => T): (T, Long) = {
    val start = System.currentTimeMillis()
    val result = code
    val duration = System.currentTimeMillis() - start
    (result, duration)
  }

  @inline
  def writeExecutionTime[T](code: => T, consume: Long => _): T = {
    val start = System.currentTimeMillis()
    val result = code
    val duration = System.currentTimeMillis() - start
    consume(duration)
    result
  }

  @inline
  def printExecutionTime[T](code: => T, text: String)(using logger: Logger): T = {
    val start = System.currentTimeMillis()
    val result = code
    val duration = System.currentTimeMillis() - start
    logger.print(text + formatTime(duration))
    result
  }

  def formatTime(t: Long): String = {
    var x = t
    val SSS = x % 1000
    x /= 1000
    val ss = x % 60
    x /= 60
    val mm = x % 60
    x /= 60
    val HH = x
    HH + "h " + (if (mm < 10) "0" + mm else mm) + "min " + (if (ss < 10) "0" + ss else ss) + "s " + (if (SSS < 100) "0" + (if (SSS < 10) "0" + SSS else SSS) else SSS) + "ms"
  }

//  class DisposableVariable[T](var value: T) {
//    def dispose(): Unit = {
//      value = null
//    }
//  }

//  class Counter() {
//    var n = 0
//
//    def tick(): Unit = {
//      n = n + 1
//      print("\r" + n)
//    }
//
//    def reset(): Unit = {
//      println()
//      n = 0
//    }
//  }
//
//  val GLOBAL_COUNTER = Counter()

}
