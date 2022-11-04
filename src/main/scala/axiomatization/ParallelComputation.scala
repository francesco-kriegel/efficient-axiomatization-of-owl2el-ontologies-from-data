package de.tu_dresden.inf.lat
package axiomatization

import java.util.concurrent.ConcurrentLinkedQueue
import util.control.Breaks.*
import scala.jdk.CollectionConverters.*

class ParallelComputation[T](private val n: Int) {

  private val resultQueue = new ConcurrentLinkedQueue[T]()
  private val threads = new Array[Thread](n)
  private var acceptsNewCodeForExecution = true

  private val t = new Thread(() => {})
  for (i <- 0 to n - 1) {
    threads(i) = t
  }
  t.start()

  def execute(code: => T): Unit = {
    if (!acceptsNewCodeForExecution) throw new IllegalStateException()
    breakable {
      while (true) {
        for (i <- 0 to n - 1) {
          if (!threads(i).isAlive) {
            val thread = new Thread(() => {
              resultQueue.offer(code);
              ()
            })
            threads(i) = thread
            thread.start()
            break
          }
        }
        Thread.sleep(100)
      }
    }
  }

  def canExecuteNewCode(): Boolean = {
    acceptsNewCodeForExecution
  }

  def hasAliveThread(): Boolean = {
    threads.exists(_.isAlive)
  }

  def results(): Iterable[T] = {
    acceptsNewCodeForExecution = false
    while (hasAliveThread()) {
      Thread.sleep(100)
    }
    resultQueue.asScala
  }

}
