package de.tu_dresden.inf.lat
package concurrent

class ConcurrentBoundedBitSet3(maxElem: Int) {

  @volatile var array = new Array[Long]((maxElem >> 6) + 1)
  val handle: java.lang.invoke.VarHandle = java.lang.invoke.MethodHandles.arrayElementVarHandle(array.getClass)//(classOf[Array[Long]])

  def asImmutableBitSet(): scala.collection.immutable.BitSet = {
    collection.immutable.BitSet.fromBitMaskNoCopy(array)
  }

  def toMutableBitSet(): scala.collection.mutable.BitSet = {
    collection.mutable.BitSet.fromBitMask(array)
  }

  def add(elem: Int): Unit = {
    require((0 <= elem) && (elem <= maxElem))
    val idx = elem >> 6
    val mask = 1L << elem
    handle.getAndBitwiseOr(array, idx, mask)
  }

  def remove(elem: Int): Unit = {
    require((0 <= elem) && (elem <= maxElem))
    val idx = elem >> 6
    val mask = 1L << elem
    val notMask = ~mask
    handle.getAndBitwiseAnd(array, idx, notMask)
  }

  def contains(elem: Int): Boolean = {
    (0 <= elem) && (elem <= maxElem) && (handle.getVolatile(array, elem >> 6).asInstanceOf[Long] & (1L << elem)) != 0L
  }

}
