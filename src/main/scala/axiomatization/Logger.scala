package de.tu_dresden.inf.lat
package axiomatization

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

  var n = 0

  def tick(): Unit = {
    n += 1
    print("\r" + n)
  }

  def reset(): Unit = {
    println()
    n = 0
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
