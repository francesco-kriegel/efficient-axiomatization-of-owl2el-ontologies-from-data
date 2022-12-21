package de.tu_dresden.inf.lat
package axiomatization

import scala.collection.concurrent.TrieMap
import scala.collection.immutable.BitSet
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.*

class BitSetToIntRelationThatExtendsInverseElementhood {

  private val bits = collection.concurrent.TrieMap[collection.BitSet, mutable.BitSet]()

  def add(xs: collection.BitSet, y: Int): Unit =
    if (!xs(y)) bits.getOrElseUpdate(xs, mutable.BitSet()).addOne(y)

  def remove(xs: collection.BitSet, y: Int): Unit =
    if (xs(y)) throw new IllegalArgumentException()
    else if (bits.contains(xs)) bits(xs).remove(y)

  def contains(xs: collection.BitSet, y: Int): Boolean =
    xs(y) || (bits.contains(xs) && bits(xs)(y))

  def apply(xs: collection.BitSet, y: Int): Boolean =
    contains(xs, y)

  def row(xs: collection.BitSet): collection.BitSet =
    if (bits.contains(xs)) xs ++ bits(xs)
    else xs

  def rawRow(xs: collection.BitSet): collection.BitSet =
    bits.getOrElse(xs, BitSet.empty)

  def rows = bits.keySet

  def clear(): Unit =
    bits.clear()

}

class BitSetToIntRelation {

  val bits = collection.concurrent.TrieMap[collection.BitSet, mutable.BitSet]()

  def add(xs: collection.BitSet, y: Int): Unit =
    bits.getOrElseUpdate(xs, mutable.BitSet()).addOne(y)

  def remove(xs: collection.BitSet, y: Int): Unit =
    if (bits.contains(xs)) bits(xs).remove(y)

  def contains(xs: collection.BitSet, y: Int): Boolean =
    bits.contains(xs) && bits(xs)(y)

  def apply(xs: collection.BitSet, y: Int): Boolean =
    contains(xs, y)

  def row(xs: collection.BitSet): collection.BitSet =
    bits.getOrElse(xs, BitSet.empty)

  def rows = bits.keySet

  def clear(): Unit =
    bits.clear()

}

class ReflexiveBitRelation[T] extends BitRelation[T, T] {

  override def add(x: T, y: T): Unit =
    if (!(x equals y)) super.add(x, y)

  override def remove(x: T, y: T): Unit =
    if (x equals y) throw new IllegalArgumentException()
    else super.remove(x, y)

  override def contains(x: T, y: T): Boolean =
    (x equals y) || super.contains(x, y)

  override def rowAsArrayBuffer(x: T): ArrayBuffer[T] =
    super.rowAsArrayBuffer(x).addOne(x)

  override def colAsArrayBuffer(y: T): ArrayBuffer[T] =
    super.colAsArrayBuffer(y).addOne(y)

  override def rowAsSet(x: T): collection.Set[T] =
    super.rowAsSet(x) + x

  override def colAsSet(y: T): collection.Set[T] =
    super.colAsSet(y) + y

}

class BitRelation[R, C] {

  private class Metadata(private val rowsOnCreation: Int, private val colsOnCreation: Int) {
    lazy val globalIndex: Int = rowsOnCreation + colsOnCreation
    lazy val sizeOfFilledRectangle: Int = rowsOnCreation * colsOnCreation
  }
  private class RowMetadata(private val rowsOnCreation: Int, private val colsOnCreation: Int) extends Metadata(rowsOnCreation, colsOnCreation) {
    def rowIndex: Int = rowsOnCreation
  }
  private class ColMetadata(private val rowsOnCreation: Int, private val colsOnCreation: Int) extends Metadata(rowsOnCreation, colsOnCreation) {
    def colIndex: Int = colsOnCreation
  }

  private[this] val bits = mutable.BitSet()
  private[this] val rows = mutable.ArrayBuffer[R]()
  private[this] val cols = mutable.ArrayBuffer[C]()
  private[this] val rowsMetadata = mutable.HashMap[R, RowMetadata]()
  private[this] val colsMetadata = mutable.HashMap[C, ColMetadata]()

  def addRow(x: R): Unit = {
    this.synchronized {
      if (!rowsMetadata.contains(x)) {
        rowsMetadata.put(x, RowMetadata(rows.size, cols.size))
        rows.addOne(x)
      }
    }
  }

  def addCol(y: C): Unit = {
    this.synchronized {
      if (!colsMetadata.contains(y)) {
        colsMetadata.put(y, ColMetadata(rows.size, cols.size))
        cols.addOne(y)
      }
    }
  }

  private def indexInBitSet(x: R, y: C): Int = {
    val rowMetadata = rowsMetadata.get(x)
    val colMetadata = colsMetadata.get(y)
    if (rowMetadata.isDefined) {
      if (colMetadata.isDefined) {
        if (rowMetadata.get.globalIndex < colMetadata.get.globalIndex)
          colMetadata.get.sizeOfFilledRectangle + rowMetadata.get.rowIndex
        else
          rowMetadata.get.sizeOfFilledRectangle + colMetadata.get.colIndex
      } else -2
    } else if (colMetadata.isDefined) -1
    else -3
  }

//  def shrink = ???

  def add(x: R, y: C): Unit = {
    val i = indexInBitSet(x, y)
    if (i >= 0) bits.addOne(i)
    else {
      if (-i % 2 == 1) addRow(x)
      if ((-i >> 1) % 2 == 1) addCol(y)
      val j = indexInBitSet(x, y)
      if (j >= 0) bits.addOne(j)
      else throw new IndexOutOfBoundsException("There are no free bit positions in the underlying BitSet.")
    }
  }

  def contains(x: R, y: C): Boolean = {
    val i = indexInBitSet(x, y)
//    if (i < 0) false
//    else bits.contains(i)
    (i >= 0) && bits.contains(i)
  }

  def remove(x: R, y: C) = {
    val i = indexInBitSet(x, y)
    if (i >= 0) bits.remove(i)
  }

  def apply(x: R, y: C): Boolean = {
    contains(x, y)
  }

  def rowAsArrayBuffer(x: R): mutable.ArrayBuffer[C] = {
    cols.filter(contains(x, _))
  }

  def colAsArrayBuffer(y: C): mutable.ArrayBuffer[R] = {
    rows.filter(contains(_, y))
  }

  def rowAsSet(x: R): collection.Set[C] = {
    colsMetadata.keySet.filter(contains(x, _))
  }

  def colAsSet(y: C): collection.Set[R] = {
    rowsMetadata.keySet.filter(contains(_, y))
  }

}

class ReflexiveLongBitRelation[T] extends LongBitRelation[T, T] {

  override def add(x: T, y: T): Unit =
    if (!(x equals y)) super.add(x, y)

  override def remove(x: T, y: T): Unit =
    if (x equals y) throw new IllegalArgumentException()
    else super.remove(x, y)

  override def contains(x: T, y: T): Boolean =
    (x equals y) || super.contains(x, y)

  override def rowAsArrayBuffer(x: T): ArrayBuffer[T] =
    super.rowAsArrayBuffer(x).addOne(x)

  override def colAsArrayBuffer(y: T): ArrayBuffer[T] =
    super.colAsArrayBuffer(y).addOne(y)

  override def rowAsSet(x: T): collection.Set[T] =
    super.rowAsSet(x) + x

  override def colAsSet(y: T): collection.Set[T] =
    super.colAsSet(y) + y

}

class LongBitRelation[R, C] {

  private class Metadata(private val rowsOnCreation: Int, private val colsOnCreation: Int) {
    lazy val globalIndex: Long = rowsOnCreation.longValue() + colsOnCreation.longValue()
    lazy val sizeOfFilledRectangle: Long = rowsOnCreation.longValue() * colsOnCreation.longValue()
  }
  private class RowMetadata(private val rowsOnCreation: Int, private val colsOnCreation: Int) extends Metadata(rowsOnCreation, colsOnCreation) {
    def rowIndex: Int = rowsOnCreation
    var isEmpty: Boolean = false
  }
  private class ColMetadata(private val rowsOnCreation: Int, private val colsOnCreation: Int) extends Metadata(rowsOnCreation, colsOnCreation) {
    def colIndex: Int = colsOnCreation
  }

  private[this] val bits = mutable.ArrayBuffer[mutable.BitSet]()
  private[this] val rows = mutable.ArrayBuffer[R]()
  private[this] val cols = mutable.ArrayBuffer[C]()
  private[this] val rowsMetadata = mutable.HashMap[R, RowMetadata]()
  private[this] val colsMetadata = mutable.HashMap[C, ColMetadata]()

  def addRow(x: R): Unit = {
    this.synchronized {
      if (!rowsMetadata.contains(x)) {
        rowsMetadata.put(x, RowMetadata(rows.size, cols.size))
        rows.addOne(x)
      }
    }
  }

  def addCol(y: C): Unit = {
    this.synchronized {
      if (!colsMetadata.contains(y)) {
        colsMetadata.put(y, ColMetadata(rows.size, cols.size))
        cols.addOne(y)
      }
    }
  }

  def nonEmptyRow(): Option[R] = {
    val it = rowsMetadata.iterator
    while (it.hasNext) {
      val (x, rowMetadata) = it.next()
      if (!rowMetadata.isEmpty)
        return Some(x)
    }
    None
  }

  def clearRow(x: R): Unit = {
    val rowMetadata = rowsMetadata.get(x)
    if (rowMetadata.isDefined) {
      rowMetadata.get.isEmpty = true
      for (y <- cols) {
//        remove(x, y)
        val (p, q) = split(_index(rowMetadata.get, colsMetadata(y)))
        if (bits.size > p) bits(p).remove(q)
      }
    }
  }

  private def indexInBitSet(x: R, y: C): Long = {
    val rowMetadata = rowsMetadata.get(x)
    val colMetadata = colsMetadata.get(y)
    if (rowMetadata.isDefined) {
      if (colMetadata.isDefined)
        _index(rowMetadata.get, colMetadata.get)
      else -2
    } else if (colMetadata.isDefined) -1
    else -3
  }

  private def _index(rowMetadata: RowMetadata, colMetadata: ColMetadata): Long = {
    if (rowMetadata.globalIndex < colMetadata.globalIndex)
      colMetadata.sizeOfFilledRectangle + rowMetadata.rowIndex.longValue()
    else
      rowMetadata.sizeOfFilledRectangle + colMetadata.colIndex.longValue()
  }

  private def split(i: Long): (Int, Int) = {
    if (i < 0) throw new IllegalArgumentException("cannot split negative number:  i = " + i)
    val p = i / Int.MaxValue.longValue()
    val q = i % Int.MaxValue.longValue()
    if ((p < 0) || (q < 0)) {
      throw new IllegalArgumentException("i = " + i + " | " + "p = " + p + " | " + "q = " + q)
    }
    (p.intValue(), q.intValue())
  }

  //  def shrink = ???

  def add(x: R, y: C): Unit = {
    val i = indexInBitSet(x, y)
    if (i >= 0) _add(i)
    else {
      if (-i % 2 == 1) addRow(x)
      if ((-i >> 1) % 2 == 1) addCol(y)
      _add(indexInBitSet(x, y))
    }
    rowsMetadata(x).isEmpty = false
  }

  private def _add(i: Long): Unit = {
    val (p, q) = split(i)
    for (_ <- bits.size to p)
      bits.addOne(new mutable.BitSet())
    bits(p).addOne(q)
  }

  def contains(x: R, y: C): Boolean = {
    val i = indexInBitSet(x, y)
    if (i < 0) false
    else {
      val (p, q) = split(i)
      if (bits.size > p) bits(p)(q)
      else false
    }
  }

  def remove(x: R, y: C): Unit = {
    val i = indexInBitSet(x, y)
    if (i >= 0) {
      val (p, q) = split(i)
      if (bits.size > p) bits(p).remove(q)
    }
  }

  def apply(x: R, y: C): Boolean = {
    contains(x, y)
  }

  def rowAsArrayBuffer(x: R): mutable.ArrayBuffer[C] = {
    cols.filter(contains(x, _))
  }

  def colAsArrayBuffer(y: C): mutable.ArrayBuffer[R] = {
    rows.filter(contains(_, y))
  }

  def rowAsSet(x: R): collection.Set[C] = {
    colsMetadata.keySet.filter(contains(x, _))
  }

  def rowAsIterable(x: R): Iterable[C] = {
    val optRowMetadata = rowsMetadata.get(x)
    if (optRowMetadata.isDefined) {
      val rowMetadata = optRowMetadata.get
      colsMetadata.collect({
        case (y, colMetadata) if {
          val (p, q) = split(_index(rowMetadata, colMetadata))
          (bits.size > p) && bits(p)(q)
        } => y
      })
    } else {
      Iterable.empty
    }
  }

  def colAsSet(y: C): collection.Set[R] = {
    rowsMetadata.keySet.filter(contains(_, y))
  }

}

class BitMap[T] {

  val rowMap = mutable.HashMap[T, mutable.BitSet]()
  //val rowMap = java.util.HashMap[T, mutable.BitSet]().asScala

  def add(x: T, y: Int): Unit = {
    rowMap.getOrElseUpdate(x, mutable.BitSet()).addOne(y)
  }

  def apply(x: T, y: Int): Boolean = {
    contains(x, y)
  }

  def contains(x: T, y: Int): Boolean = {
    rowMap.get(x).map(_.contains(y)).getOrElse(false)
  }

  def remove(x: T, y: Int): Unit = {
    rowMap.get(x).foreach(_.remove(y))
  }

  def rows(): collection.Set[T] = {
    rowMap.keySet
  }

  def row(x: T): mutable.BitSet = {
    rowMap.getOrElse(x, mutable.BitSet.empty)
  }

  def clearRow(x: T): Option[mutable.BitSet] = {
    rowMap.remove(x)
  }

}

class BitBiMap extends BitMap[Int] {

  // TODO: Replace HashMap[Int, T] with Array[T] if upper bound for keys is known, otherwise use ArrayBuffer[T]
  val colMap = mutable.HashMap[Int, mutable.BitSet]()

  override def add(x: Int, y: Int): Unit = {
    super.add(x, y)
    colMap.getOrElseUpdate(y, mutable.BitSet()).addOne(x)
  }

  override def remove(x: Int, y: Int): Unit = {
    super.remove(x, y)
    colMap.get(y).foreach(_.remove(x))
  }

  def cols(): collection.Set[Int] = {
    colMap.keySet
  }

  def col(y: Int): mutable.BitSet = {
    colMap.getOrElse(y, mutable.BitSet.empty)
  }

  def clearCol(y: Int): Option[mutable.BitSet] = {
    colMap.remove(y)
  }

}

class ArrayBitBiMap(rows: Int, cols: Int) {

  private val rowArray = new scala.Array[mutable.BitSet](rows)
  private val colArray = new scala.Array[mutable.BitSet](cols)
  (0 until rows).foreach(i => rowArray(i) = mutable.BitSet())
  (0 until cols).foreach(j => colArray(j) = mutable.BitSet())

  def add(x: Int, y: Int): Unit = {
    rowArray(x).addOne(y)
    colArray(y).addOne(x)
  }

  def apply(x: Int, y: Int): Boolean = {
    contains(x, y)
  }

  def contains(x: Int, y: Int): Boolean = {
    rowArray(x)(y)
  }

  def remove(x: Int, y: Int): Unit = {
    rowArray(x).remove(y)
    colArray(y).remove(x)
  }

  def row(x: Int): mutable.BitSet = {
    rowArray(x)
  }

  def col(y: Int): mutable.BitSet = {
    colArray(y)
  }

}

class ConcurrentArrayBitBiMap(rows: Int, cols: Int) {

  private val rowArray = new scala.Array[concurrent.ConcurrentBoundedBitSet2](rows)
  private val colArray = new scala.Array[concurrent.ConcurrentBoundedBitSet2](cols)
  (0 until rows).foreach(i => rowArray(i) = concurrent.ConcurrentBoundedBitSet2(cols - 1))
  (0 until cols).foreach(j => colArray(j) = concurrent.ConcurrentBoundedBitSet2(rows - 1))

  def add(x: Int, y: Int): Unit = {
    rowArray(x).add(y)
    colArray(y).add(x)
  }

  def apply(x: Int, y: Int): Boolean = {
    contains(x, y)
  }

  def contains(x: Int, y: Int): Boolean = {
    rowArray(x).contains(y)
  }

  def remove(x: Int, y: Int): Unit = {
    rowArray(x).remove(y)
    colArray(y).remove(x)
  }

  def row(x: Int): concurrent.ConcurrentBoundedBitSet2 = {
    rowArray(x)
  }

  def col(y: Int): concurrent.ConcurrentBoundedBitSet2 = {
    colArray(y)
  }

}

class SynchronizedArrayBitBiMap(rows: Int, cols: Int) {

  private val rowArray = new scala.Array[mutable.BitSet](rows)
  private val colArray = new scala.Array[mutable.BitSet](cols)
  (0 until rows).foreach(i => rowArray(i) = mutable.BitSet())
  (0 until cols).foreach(j => colArray(j) = mutable.BitSet())

  def add(x: Int, y: Int): Unit = {
    val row = rowArray(x)
    row.synchronized { row.add(y) }
    val col = colArray(y)
    col.synchronized { col.add(x) }
  }

  def apply(x: Int, y: Int): Boolean = {
    contains(x, y)
  }

  def contains(x: Int, y: Int): Boolean = {
    val row = rowArray(x)
    row.synchronized { row.contains(y) }
  }

  def remove(x: Int, y: Int): Unit = {
    val row = rowArray(x)
    row.synchronized { row.remove(y) }
    val col = colArray(y)
    col.synchronized { col.remove(x) }
  }

  def row(x: Int): mutable.BitSet = {
    rowArray(x)
  }

  def col(y: Int): mutable.BitSet = {
    colArray(y)
  }

}

class ConcurrentBitMap[T](maxY: Int) {

//  private val rowMap = scala.collection.concurrent.TrieMap[T, concurrent.ConcurrentBoundedBitSet2]()
  val rowMap = java.util.concurrent.ConcurrentHashMap[T, concurrent.ConcurrentBoundedBitSet2]().asScala

  def add(x: T, y: Int): Unit = {
    rowMap.getOrElseUpdate(x, concurrent.ConcurrentBoundedBitSet2(maxY)).add(y)
//    rowMap.computeIfAbsent(x, _ => concurrent.ConcurrentBoundedBitSet2(maxY)).add(y)
  }

  def apply(x: T, y: Int): Boolean = {
    contains(x, y)
  }

  def contains(x: T, y: Int): Boolean = {
    rowMap.get(x).exists(_.contains(y))
//    rowMap.contains(x) && rowMap.get(x).contains(y)
  }

  def remove(x: T, y: Int): Unit = {
    rowMap.get(x).foreach(_.remove(y))
//    if (rowMap.contains(x))
//      rowMap.get(x).remove(y)
  }

  def rows(): collection.Set[T] = {
    rowMap.keySet
  }

  def rowImmutable(x: T): scala.collection.immutable.BitSet = {
    if rowMap contains x
    then rowMap(x).viewAsImmutableBitSet()
    else scala.collection.immutable.BitSet.empty
  }

  def row(x: T): Option[concurrent.ConcurrentBoundedBitSet2] = {
    rowMap.get(x)
  }

  def clearRow(x: T): Option[concurrent.ConcurrentBoundedBitSet2] = {
    rowMap.remove(x)
  }

}

class ConcurrentBitBiMap(maxX: Int, maxY: Int) extends ConcurrentBitMap[Int](maxY) {

//  private val colMap = scala.collection.concurrent.TrieMap[Int, concurrent.ConcurrentBoundedBitSet2]()
  val colMap = java.util.concurrent.ConcurrentHashMap[Int, concurrent.ConcurrentBoundedBitSet2]().asScala

  override def add(x: Int, y: Int): Unit = {
    super.add(x, y)
    colMap.getOrElseUpdate(y, concurrent.ConcurrentBoundedBitSet2(maxX)).add(x)
  }

  override def remove(x: Int, y: Int): Unit = {
    super.remove(x, y)
    colMap.get(y).foreach(_.remove(x))
  }

  def cols(): collection.Set[Int] = {
    colMap.keySet
  }

  def colImmutable(y: Int): scala.collection.immutable.BitSet = {
    if colMap contains y
    then colMap(y).viewAsImmutableBitSet()
    else scala.collection.immutable.BitSet.empty
  }

  def col(y: Int): Option[concurrent.ConcurrentBoundedBitSet2] = {
    colMap.get(y)
  }

  def clearCol(y: Int): Option[concurrent.ConcurrentBoundedBitSet2] = {
    colMap.remove(y)
  }

}
