package de.tu_dresden.inf.lat
package concurrent

//* A bitset for elements that are integers between 0 and maxElem */
@Deprecated(forRemoval = true)
class ConcurrentBoundedBitSet(maxElem: Int) {

  private val atomicArray = AtomicLongArray((maxElem >> 6) + 1)

  def asImmutableBitSet(): scala.collection.immutable.BitSet = {
    collection.immutable.BitSet.fromBitMaskNoCopy(atomicArray.array)
  }

  def toMutableBitSet(): scala.collection.mutable.BitSet = {
    collection.mutable.BitSet.fromBitMask(atomicArray.array)
  }

  def add(elem: Int): Unit = {
    require((0 <= elem) && (elem <= maxElem))
    val idx = elem >> 6
    val mask = 1L << elem
    var successfullyAdded = false
    while (!successfullyAdded) {
      val word = atomicArray.get(idx)
      successfullyAdded = ((word & mask) != 0L) || atomicArray.compareAndSet(idx, word, word | mask)
    }
  }

  def remove(elem: Int): Unit = {
    require((0 <= elem) && (elem <= maxElem))
    val idx = elem >> 6
    val mask = 1L << elem
    val notMask = ~mask
    var successfullyRemoved = false
    while (!successfullyRemoved) {
      val word = atomicArray.get(idx)
      successfullyRemoved = ((word & mask) == 0L) || atomicArray.compareAndSet(idx, word, word & notMask)
    }
  }

  def contains(elem: Int): Boolean = {
    (0 <= elem) && (elem <= maxElem) && (atomicArray.get(elem >> 6) & (1L << elem)) != 0L
  }

}
