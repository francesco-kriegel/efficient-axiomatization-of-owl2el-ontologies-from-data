package de.tu_dresden.inf.lat
package axiomatization

import java.io.{BufferedWriter, FileWriter}
import java.util.concurrent.ConcurrentLinkedQueue

trait ValueLogger() {
  def logValue(obj: Any): Unit
  def close(): Unit
}

class FileValueLogger(val filename: String, val prefix: String) extends ValueLogger() {

  val queue = new ConcurrentLinkedQueue[Any]()
  var isOpen = true

  def logValue(obj: Any): Unit = {
    if (isOpen)
      queue.offer(obj)
    else
      throw new IllegalStateException("The value logger has been closed.")
  }

  def close(): Unit = {
    isOpen = false
  }

  new Thread(() => {
    val writer = new BufferedWriter(new FileWriter(filename, true))
    writer.newLine()
    writer.write(prefix)
    while (isOpen) {
      while (!queue.isEmpty) {
        writer.write(";" + queue.poll())
      }
      writer.flush()
      Thread.sleep(1000)
    }
    while (!queue.isEmpty) {
      writer.write(";" + queue.poll())
    }
    writer.flush()
    writer.close()
  }).start()

}

class LoggerValueLogger()(using logger: Logger) extends ValueLogger() {
  logger.println()
  override def logValue(obj: Any): Unit = logger.print("\r" + obj)
  override def close(): Unit = {}
}
