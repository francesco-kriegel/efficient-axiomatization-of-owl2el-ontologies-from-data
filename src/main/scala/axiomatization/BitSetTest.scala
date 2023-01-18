package de.tu_dresden.inf.lat
package axiomatization

@Deprecated(forRemoval = true)
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

    val start1 = System.currentTimeMillis()
    val bs = concurrent.ConcurrentBitSet()
    println("Instatiation: " + Util.formatTime(System.currentTimeMillis() - start1))

    val range = 0 until (1 << 20) by 1
    //scala.collection.immutable.List.from(range)
    val rangeInRandomOrder = scala.util.Random.shuffle(range)
    val start2 = System.currentTimeMillis()
    rangeInRandomOrder.map(i => {
      scala.concurrent.Future {
        bs.add(i)
      }
    }).foreach(scala.concurrent.Await.ready(_, scala.concurrent.duration.Duration.Inf))
    //rangeInRandomOrder.foreach(bs.add)
    println("Adding elements: " + Util.formatTime(System.currentTimeMillis() - start2))

    val start3 = System.currentTimeMillis()
    bs.disallowResize()
    println("Consolidation: " + Util.formatTime(System.currentTimeMillis() - start3))

    val start4 = System.currentTimeMillis()
    println(bs.viewAsImmutableBitSet().size)
    println("Determination of size: " + Util.formatTime(System.currentTimeMillis() - start3))

    val start5 = System.currentTimeMillis()
    println(range.forall(bs.contains))
    println("Checking elements: " + Util.formatTime(System.currentTimeMillis() - start3))


//    val handle = java.lang.invoke.MethodHandles.lookup().findStaticVarHandle(this.getClass, "int", int.getClass)
//
//    println(handle.varType())
//    handle.getVolatile(Array[Object](this))

  }

}
