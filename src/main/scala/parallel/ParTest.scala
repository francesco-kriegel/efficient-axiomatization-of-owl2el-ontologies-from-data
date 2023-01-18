package de.tu_dresden.inf.lat
package parallel

import de.tu_dresden.inf.lat.parallel.NestedParallelComputations._

@Deprecated(forRemoval = true)
object ParTest {

  def main(args: Array[String]): Unit = {
    val counter = java.util.concurrent.atomic.AtomicLong(0)
    (0 until 100).foreachPar(i => {
      ('a' to 'z').foreachPar(x => {
        println(i + ":" + x)
        counter.incrementAndGet()
      })
    })
    println()
    println(counter.get())

    def f(x: Int): Double = {
      Thread.sleep(1)
      Math.sqrt(x)
    }

    val startSeq = System.currentTimeMillis()
    val sqrtsSeq = (0 until 10000).map(f)
    val durationSeq = System.currentTimeMillis() - startSeq

    val startPar = System.currentTimeMillis()
    val sqrtsPar = (0 until 10000).mapPar(f)
    val durationPar = System.currentTimeMillis() - startPar

    println(sqrtsSeq equals sqrtsPar)
    println("Sequential computation: " + de.tu_dresden.inf.lat.axiomatization.Util.formatTime(durationSeq))
    println("Parallel computation:   " + de.tu_dresden.inf.lat.axiomatization.Util.formatTime(durationPar))

//    (0 until 10000).tapEachPar(println)

  }

}
