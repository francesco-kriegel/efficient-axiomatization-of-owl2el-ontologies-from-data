package de.tu_dresden.inf.lat
package axiomatization

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}


trait Logger {
  @inline def print(obj: Any): Unit
  @inline def println(obj: Any): Unit
  @inline def println(): Unit

  @inline def errPrint(obj: Any): Unit
  @inline def errPrintln(obj: Any): Unit
  @inline def errPrintln(): Unit

  @inline def tick(): Unit
  @inline def reset(): Unit
}

final class ConsoleLogger() extends Logger {
  override def print(obj: Any): Unit = Console.print(obj)
  override def println(obj: Any): Unit = Console.println(obj)
  override def println(): Unit = Console.println()

  override def errPrint(obj: Any): Unit = Console.err.print(obj)
  override def errPrintln(obj: Any): Unit = Console.err.println(obj)
  override def errPrintln(): Unit = Console.err.println()

  private val n = AtomicInteger(0)
  private val start = AtomicLong(0L)
  private val time = AtomicLong(0L)
  private val cRate = AtomicInteger(0)
  private val tRate = AtomicInteger(0)

  private val updateRate = 100

  override def tick(): Unit = {
    //print("\r" + n.incrementAndGet())
    val i = n.incrementAndGet()
    if (i % updateRate == 0) {
      val current = System.currentTimeMillis()
      val last = time.getAndSet(current)
      val j = ((1000L * updateRate.toLong) / Math.max(1, current - last)).toInt
      val k = ((1000L * i.toLong) / Math.max(1, current - start.get())).toInt
      cRate.set(j)
      tRate.set(k)
      print(s"\r$i (current: $j/s, total: $k/s)        ")
    } else {
      val j = cRate.get()
      val k = tRate.get()
      print(s"\r$i (current: $j/s, total: $k/s)        ")
    }
  }

  override def reset(): Unit = {
    println()
    n.set(0)
    start.set(System.currentTimeMillis())
    time.set(System.currentTimeMillis())
    cRate.set(0)
    tRate.set(0)
  }
}

final class NoLogger() extends Logger {
  override def print(obj: Any): Unit = {}
  override def println(obj: Any): Unit = {}
  override def println(): Unit = {}

  override def errPrint(obj: Any): Unit = {}
  override def errPrintln(obj: Any): Unit = {}
  override def errPrintln(): Unit = {}

  override def tick(): Unit = {}
  override def reset(): Unit = {}
}
