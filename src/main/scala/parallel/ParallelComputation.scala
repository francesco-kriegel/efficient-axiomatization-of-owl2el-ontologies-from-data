package de.tu_dresden.inf.lat
package parallel

import java.util.concurrent.ThreadPoolExecutor

object ParallelComputation {

  private val threadPoolExecutor: ThreadPoolExecutor =
    java.util.concurrent.ThreadPoolExecutor(
      Runtime.getRuntime.availableProcessors(),
      Runtime.getRuntime.availableProcessors(),
      60L, java.util.concurrent.TimeUnit.SECONDS,
      java.util.concurrent.LinkedBlockingQueue[Runnable]()
    )

  implicit val executionContext: scala.concurrent.ExecutionContextExecutor =
    scala.concurrent.ExecutionContext.fromExecutor(
      threadPoolExecutor
    )

  implicit class IterableWithForeachPar[T](iterable: scala.collection.Iterable[T]) {
    inline def foreachPar[U](f: T => U): Unit = {
      iterable.map(t => scala.concurrent.Future(f(t)))
        .foreach(scala.concurrent.Await.ready(_, scala.concurrent.duration.Duration.Inf))
    }
//    inline def pareach[U](f: T => U): Unit = foreachPar(f)
  }

  def shutdownThreadPool(): Unit = {
    threadPoolExecutor.shutdown()
  }

  implicit class IterableOfFutures(futures: scala.collection.Iterable[scala.concurrent.Future[_]]) {
    inline def awaitEach(): Unit = {
      futures.foreach(scala.concurrent.Await.ready(_, scala.concurrent.duration.Duration.Inf))
    }
  }

//  implicit class IterableOfCodeBlocks(tasks: scala.collection.Iterable[CodeBlock]) {
//    def executeInParallel(): Unit = {
//      tasks.map(computation => scala.concurrent.Future { computation.execute() })
//        .foreach(scala.concurrent.Await.ready(_, scala.concurrent.duration.Duration.Inf))
//    }
//  }

//  private[ParallelComputation]
//  class CodeBlock(code: => Unit) {
//    inline def execute(): Unit = code
//  }

}
