package de.tu_dresden.inf.lat
package axiomatization

object BitSetTest {

//  var int = 0

  def main(args: Array[String]): Unit = {
//    val us = scala.collection.immutable.BitSet(39, 41, 44, 46, 256)
//    val vs = scala.collection.immutable.BitSet(39, 41, 44, 46, 64, 256)
//    val xs = scala.collection.immutable.BitSet.fromBitMask(us.toBitMask.take(3))
//    val ys = scala.collection.immutable.BitSet.fromBitMask(vs.toBitMask.take(3))
//    val diff = ys diff xs
//    println("xs: " + xs)
//    println("ys: " + ys)
//    println("diff: " + diff)

    import scala.concurrent.ExecutionContext.Implicits.global

    val bs = concurrent.ConcurrentBitSet()

    (0 until 16384 by 1).map(i => {
      scala.concurrent.Future {
        bs.add(i)
      }
    }).foreach(scala.concurrent.Await.ready(_, scala.concurrent.duration.Duration.Inf))

    bs.disallowResize()

    println(bs.viewAsImmutableBitSet().size)
    println((0 until 16384 by 1).forall(bs.contains))


//    val handle = java.lang.invoke.MethodHandles.lookup().findStaticVarHandle(this.getClass, "int", int.getClass)
//
//    println(handle.varType())
//    handle.getVolatile(Array[Object](this))

  }

}
