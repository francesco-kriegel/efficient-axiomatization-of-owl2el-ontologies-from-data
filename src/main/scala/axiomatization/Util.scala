
package de.tu_dresden.inf.lat
package axiomatization

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

  implicit class LocalSet[A](set: collection.Set[A]) {
    def strictSubsetOf(other: collection.Set[A]): Boolean = {
      (set subsetOf other) && (set.size < other.size)
    }
  }

  implicit class LocalMultiMap[A, B](map: collection.Map[A, collection.mutable.Set[B]] with collection.mutable.MultiMap[A, B]) {
    def foreachBinding(f: (A, B) ⇒ Unit): Unit = {
      map.foreach({ case (key, values) ⇒ values.foreach(value ⇒ f(key, value)) })
    }
  }

  def measureExecutionTime[T](code: => T, text: String): T = {
    val start = System.currentTimeMillis()
    val result = code
    val duration = System.currentTimeMillis() - start
    print(text + formatTime(duration))
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

  class Counter() {
    var n = 0

    def tick(): Unit = {
      n = n + 1
      print("\r" + n)
    }

    def reset(): Unit = {
      println()
      n = 0
    }
  }

  val GLOBAL_COUNTER = Counter()

}
