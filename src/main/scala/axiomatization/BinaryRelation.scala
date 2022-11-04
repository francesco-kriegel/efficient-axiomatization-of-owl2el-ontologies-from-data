package de.tu_dresden.inf.lat
package axiomatization

import scala.collection.concurrent.TrieMap
import scala.collection.immutable.BitSet
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

//trait BinaryRelation[It[_] <: Iterable[_], R, C] {
//
//  def apply(x: R, y: C): Boolean = contains(x,y)
//
////  def pairs(): It[(R,C)]
//
////  def rows(): It[R]
////  def columns(): It[C]
//
//  def row(x: R): It[C]
//  def col(y: C): It[R]
//
//  def add(x: R, y: C): Unit
//  def remove(x: R, y: C): Unit
//  def contains(x: R, y: C): Boolean
//
//}

//class HashRelation[R, C] extends BinaryRelation[mutable.Set, R, C] {
//
//  var _pairs = mutable.Set[(R,C)]()
//
////  def pairs(): mutable.Set[(R,C)] = _pairs
//
//  def rows(): mutable.Set[R] = { _pairs.map({ case (x, _) => x }) }
//
//  def columns(): mutable.Set[C] = { _pairs.map({ case (_, y) => y }) }
//
//  def row(x: R): mutable.Set[C] = { _pairs.collect({ case (xx, y) if xx equals x => y }) }
//
//  def col(y: C): mutable.Set[R] = { _pairs.collect({ case (x, yy) if yy equals y => x }) }
//
//  def add(x: R, y: C): Unit = { _pairs += (x,y) }
//
//  def remove(x: R, y: C): Unit = { _pairs -= (x,y) }
//
//  def contains(x: R, y: C): Boolean = { _pairs(x,y) }
//
//}

//class ConcurrentRelation[R, C] extends BinaryRelation[Set, R, C] {
//
//  private val _pairs = collection.concurrent.TrieMap[(R,C),Unit]()
//  private def pairs(): Set[(R,C)] = _pairs.keySet.toSet
//
////  def rows(): Set[R] = { pairs().map({ case (x, _) => x }) }
////  def columns(): Set[C] = { pairs().map({ case (_, y) => y }) }
//
//  def row(x: R): Set[C] = { pairs().collect({ case (xx, y) if xx equals x => y }) }
//
//  def col(y: C): Set[R] = { pairs().collect({ case (x, yy) if yy equals y => x }) }
//
//  def add(x: R, y: C): Unit = { _pairs.put((x,y), ()) }
//
//  def remove(x: R, y: C): Unit = { _pairs.remove((x,y)) }
//
//  def contains(x: R, y: C): Boolean = { _pairs.contains((x,y)) }
//
//}

//class ConcurrentRelationExtendingSetInclusion[T] extends ConcurrentRelation[collection.Set[T], T] {
//  override def add(xs: collection.Set[T], y: T): Unit = if (!xs(y)) super.add(xs, y)
//
//  override def remove(xs: collection.Set[T], y: T): Unit =
//    if (xs(y)) throw new IllegalArgumentException()
//    else super.remove(xs, y)
//
//  override def contains(xs: collection.Set[T], y: T): Boolean = xs(y) || super.contains(xs, y)
//
//  override def row(xs: collection.Set[T]): Set[T] = super.row(xs) ++ xs
//
//  override def col(y: T): Set[collection.Set[T]] = ???
//}

class BitSetToIntRelation {

  private val bits = collection.concurrent.TrieMap[BitSet, mutable.BitSet]()

  def add(xs: BitSet, y: Int): Unit =
    if (!xs(y)) bits.getOrElseUpdate(xs, mutable.BitSet()).addOne(y)

  def remove(xs: BitSet, y: Int): Unit =
    if (xs(y)) throw new IllegalArgumentException()
    else if (bits.contains(xs)) bits(xs).remove(y)

  def contains(xs: BitSet, y: Int): Boolean =
    xs(y) || (bits.contains(xs) && bits(xs)(y))

  def apply(xs: BitSet, y: Int): Boolean =
    contains(xs, y)

  def row(xs: BitSet): BitSet =
    if (bits.contains(xs)) xs ++ bits(xs)
    else xs

  def rawRow(xs: BitSet): collection.BitSet =
    bits.getOrElse(xs, BitSet.empty)

  def rows = bits.keySet

}

//class ConcurrentReflexiveRelation[T] extends ConcurrentRelation[T, T] {
//
//  override def add(x: T, y: T): Unit = if (!(x equals y)) super.add(x, y)
//
//  override def remove(x: T, y: T): Unit =
//    if (x equals y) throw new IllegalAccessException()
//    else super.remove(x, y)
//
//  override def contains(x: T, y: T): Boolean = (x equals y) || super.contains(x, y)
//
//  override def row(x: T): Set[T] = super.row(x) + x
//
//  override def col(y: T): Set[T] = super.col(y) + y
//
//}

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

  private val rowMap = mutable.HashMap[T, mutable.BitSet]()

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

  def clearRow(x: T): Unit = {
    rowMap.remove(x)
  }

}

class BitBiMap extends BitMap[Int] {

  private val colMap = mutable.HashMap[Int, mutable.BitSet]()

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

  def clearCol(y: Int): Unit = {
    colMap.remove(y)
  }

}
