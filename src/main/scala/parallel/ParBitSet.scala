package scala.collection.parallel

class ParBitSet(bs: collection.BitSet) extends scala.collection.parallel.ParSet[Int] {

  def seq: collection.BitSet = bs

  def +(elem: Int): scala.collection.parallel.ParSet[Int] = ParBitSet(bs + elem)
  def -(elem: Int): scala.collection.parallel.ParSet[Int] = ParBitSet(bs - elem)
  def contains(elem: Int): Boolean = bs contains elem
  def size: Int = bs.size

  protected[parallel] def splitter: scala.collection.parallel.IterableSplitter[Int] = BitSetSplitter(0, 64 * bs.nwords, bs.size)

  private class BitSetSplitter(private[this] val minInclusive: Int,
                               private[this] val maxExclusive: Int,
                               private[this] val totalElements: Int) extends scala.collection.parallel.IterableSplitter[Int]() {

    private[this] val it = bs.iterator
    private[this] var remainingElements: Int = totalElements
    private[this] var nextElement: Int = -1
    final var hasNext: Boolean = false

    private[this] def advanceIterator(): Unit = {
      hasNext = it.hasNext
      if (hasNext)
        nextElement = it.next()
        hasNext &= nextElement < maxExclusive
    }
    advanceIterator()

    final override def next(): Int = {
      if (hasNext) {
        remainingElements -= 1
        val element = nextElement
        advanceIterator()
        element
      } else Iterator.empty.next()
    }

    def dup: scala.collection.parallel.IterableSplitter[Int] = BitSetSplitter(nextElement, maxExclusive, remainingElements)

    def remaining: Int = remainingElements

    def split: Seq[scala.collection.parallel.IterableSplitter[Int]] = {
      if (remainingElements < 2)
        Seq(this)
      else
        val halfRemainingElements = remainingElements / 2
        // TODO: The following should be done in a more efficient way.
        val jt = bs.iteratorFrom(nextElement)
        var j = 1
        while (j < halfRemainingElements)
          jt.next()
          j += 1
        val middle = jt.next()
        Seq(BitSetSplitter(nextElement, middle, halfRemainingElements), BitSetSplitter(middle, maxExclusive, remainingElements - halfRemainingElements))
    }


  }

}
