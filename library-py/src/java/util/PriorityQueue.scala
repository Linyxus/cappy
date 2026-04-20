/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package java.util

import scala.language.unsafeNulls

import scala.language.higherKinds

import scala.annotation.tailrec

import java.lang.Utils.roundUpToPowerOfTwo

import java.util.Objects.requireNonNull

class PriorityQueue[E] private (
    private val comp: Comparator[_ >: E], internal: Boolean, initialCapacity: Int)
    extends AbstractQueue[E] with Serializable {

  import PriorityQueue._

  def this() =
    this(NaturalComparator, internal = true, initialCapacity = 16)

  def this(initialCapacity: Int) = {
    this(
      NaturalComparator,
      internal = true, {
        if (initialCapacity < 1)
          throw new IllegalArgumentException
        initialCapacity + 1 // index 0 is unused
      }
    )
  }

  def this(comparator: Comparator[_ >: E]) = {
    this(NaturalComparator.select(comparator), internal = true, initialCapacity = 16)
  }

  def this(initialCapacity: Int, comparator: Comparator[_ >: E]) = {
    this(
      NaturalComparator.select(comparator),
      internal = true, {
        if (initialCapacity < 1)
          throw new IllegalArgumentException()
        initialCapacity + 1 // index 0 is unused
      }
    )
  }

  def this(c: Collection[_ <: E]) = {
    this(c match {
      case c: PriorityQueue[_] =>
        c.comp.asInstanceOf[Comparator[_ >: E]]
      case c: SortedSet[_] =>
        NaturalComparator.select(c.comparator().asInstanceOf[Comparator[_ >: E]])
      case _ =>
        NaturalComparator
    }, internal = true, roundUpToPowerOfTwo(c.size() + 1)) // index 0 is unused
    addAll(c)
  }

  def this(c: PriorityQueue[_ <: E]) = {
    this(c.comp.asInstanceOf[Comparator[_ >: E]], internal = true,
        roundUpToPowerOfTwo(c.size() + 1)) // index 0 is unused
    addAll(c)
  }

  def this(sortedSet: SortedSet[_ <: E]) = {
    this(
        NaturalComparator.select(
            sortedSet.comparator().asInstanceOf[Comparator[_ >: E]]),
        internal = true,
        roundUpToPowerOfTwo(sortedSet.size() + 1)) // index 0 is unused
    addAll(sortedSet)
  }

  // The index 0 is not used; the root is at index 1.
  // This is standard practice in binary heaps, to simplify arithmetics.
  private var inner: Array[AnyRef] = makeArray(initialCapacity)

  override def add(e: E): Boolean = {
    val newInner = arrayPush(inner, requireNonNull(e))
    inner = newInner
    fixUp(arrayLength(inner) - 1)
    true
  }

  def offer(e: E): Boolean = add(e)

  def peek(): E =
    if (arrayLength(inner) > 1) arrayGet(inner, 1)
    else null.asInstanceOf[E]

  override def remove(o: Any): Boolean = {
    if (o == null) {
      false
    } else {
      val len = arrayLength(inner)
      var i = 1
      while (i != len && !o.equals(arrayGet(inner, i))) {
        i += 1
      }

      if (i != len) {
        removeAt(i)
        true
      } else {
        false
      }
    }
  }

  private def removeExact(o: Any): Unit = {
    val len = arrayLength(inner)
    var i = 1
    while (i != len && (o.asInstanceOf[AnyRef] ne arrayGet(inner, i).asInstanceOf[AnyRef])) {
      i += 1
    }
    if (i == len)
      throw new ConcurrentModificationException()
    removeAt(i)
  }

  private def removeAt(i: Int): Unit = {
    val newLength = arrayLength(inner) - 1
    if (i == newLength) {
      arrayDecLength(inner)
    } else {
      arraySet(inner, i, arrayGet(inner, newLength))
      arrayDecLength(inner)
      fixUpOrDown(i)
    }
  }

  override def contains(o: Any): Boolean = {
    if (o == null) {
      false
    } else {
      val len = arrayLength(inner)
      var i = 1
      while (i != len && !o.equals(arrayGet(inner, i))) {
        i += 1
      }
      i != len
    }
  }

  def iterator(): Iterator[E] = {
    new Iterator[E] {
      private[this] var inner: Array[AnyRef] = PriorityQueue.this.inner
      private[this] var nextIdx: Int = 1
      private[this] var last: E = _ // null

      def hasNext(): Boolean = nextIdx < arrayLength(inner)

      def next(): E = {
        if (!hasNext())
          throw new NoSuchElementException("empty iterator")
        last = arrayGet(inner, nextIdx)
        nextIdx += 1
        last
      }

      override def remove(): Unit = {
        /* Once we start removing elements, the inner array of the enclosing
         * PriorityQueue will be modified in arbitrary ways. In particular,
         * entries yet to be iterated can be moved before `nextIdx` if the
         * removal requires a `fixUp()`.
         *
         * Therefore, at the first removal, we take a snapshot of the remainder
         * of the inner array yet to be iterated, and continue iterating over
         * the snapshot.
         *
         * We use a linear lookup based on reference equality to precisely
         * remove the entries that we are still iterating over (in
         * `removeExact()`).
         *
         * This means that this method is O(n), contrary to typical
         * expectations for `Iterator.remove()`. I could not come up with a
         * better algorithm.
         */

        if (last == null)
          throw new IllegalStateException()
        if (inner eq PriorityQueue.this.inner) {
          inner = arrayCopyFrom(inner, nextIdx)
          nextIdx = 1
        }
        removeExact(last)
        last = null.asInstanceOf[E]
      }
    }
  }

  def size(): Int = arrayLength(inner) - 1

  override def clear(): Unit =
    arrayClear(inner)

  def poll(): E = {
    val inner = this.inner // local copy
    if (arrayLength(inner) > 1) {
      val newSize = arrayLength(inner) - 1
      val result = arrayGet(inner, 1)
      arraySet(inner, 1, arrayGet(inner, newSize))
      arrayDecLength(inner)
      fixDown(1)
      result
    } else {
      null.asInstanceOf[E]
    }
  }

  def comparator(): Comparator[_ >: E] =
    NaturalComparator.unselect(comp)

  // Heavy lifting: heap fixup

  /** Fixes the heap property around the child at index `m`, either up the
   *  tree or down the tree, depending on which side is found to violate the
   *  heap property.
   */
  private[this] def fixUpOrDown(m: Int): Unit = {
    val inner = this.inner // local copy
    if (m > 1 && comp.compare(arrayGet(inner, m >> 1), arrayGet(inner, m)) > 0)
      fixUp(m)
    else
      fixDown(m)
  }

  /** Fixes the heap property from the child at index `m` up the tree, towards
   *  the root.
   */
  private[this] def fixUp(m: Int): Unit = {
    val inner = this.inner // local copy

    /* At each step, even though `m` changes, the element moves with it, and
     * hence inner(m) is always the same initial `innerAtM`.
     */
    val innerAtM = arrayGet(inner, m)

    var current = m
    var done = false
    while !done && current > 1 do
      val parent = current >> 1
      val innerAtParent = arrayGet(inner, parent)
      if comp.compare(innerAtParent, innerAtM) > 0 then
        arraySet(inner, parent, innerAtM)
        arraySet(inner, current, innerAtParent)
        current = parent
      else
        done = true
  }

  /** Fixes the heap property from the child at index `m` down the tree,
   *  towards the leaves.
   */
  private[this] def fixDown(m: Int): Unit = {
    val inner = this.inner // local copy
    val size = arrayLength(inner) - 1

    /* At each step, even though `m` changes, the element moves with it, and
     * hence inner(m) is always the same initial `innerAtM`.
     */
    val innerAtM = arrayGet(inner, m)

    var current = m
    var done = false
    while !done do
      var child = 2 * current
      if child <= size then
        var childValue = arrayGet(inner, child)

        if child < size then
          val rightValue = arrayGet(inner, child + 1)
          if comp.compare(childValue, rightValue) > 0 then
            child += 1
            childValue = rightValue

        if comp.compare(innerAtM, childValue) > 0 then
          arraySet(inner, current, childValue)
          arraySet(inner, child, innerAtM)
          current = child
        else
          done = true
      else
        done = true
  }

}

object PriorityQueue {
  /* We store the effective length in the index 0 of the array,
   * which is unused by the heap itself.
   */
  @inline private def makeArray(initialCapacity: Int): Array[AnyRef] = {
    val v = new Array[AnyRef](initialCapacity)
    v(0) = 1.asInstanceOf[AnyRef]
    v
  }

  @inline private def arrayLength(v: Array[AnyRef]): Int =
    v(0).asInstanceOf[Int]

  @inline private def arrayDecLength(v: Array[AnyRef]): Unit = {
    val newLength = arrayLength(v) - 1
    v(0) = newLength.asInstanceOf[AnyRef]
    v(newLength) = null // free reference for GC
  }

  @inline private def arrayGet[E](v: Array[AnyRef], index: Int): E =
    v(index).asInstanceOf[E]

  @inline private def arraySet[E](v: Array[AnyRef], index: Int, e: E): Unit =
    v(index) = e.asInstanceOf[AnyRef]

  @inline private def arrayPush[E](v: Array[AnyRef], e: E): Array[AnyRef] = {
    val l = arrayLength(v)
    val minCapacity = l + 1
    val newArr = {
      if (v.length < minCapacity)
        Arrays.copyOf(v, roundUpToPowerOfTwo(minCapacity))
      else v
    }
    newArr(l) = e.asInstanceOf[AnyRef]
    newArr(0) = (l + 1).asInstanceOf[AnyRef]
    newArr
  }

  @inline private def arrayCopyFrom(v: Array[AnyRef], from: Int): Array[AnyRef] = {
    val elemLength = arrayLength(v) - from
    val newArr = new Array[AnyRef](elemLength + 1)
    newArr(0) = (elemLength + 1).asInstanceOf[AnyRef]
    System.arraycopy(v, from, newArr, 1, elemLength)
    newArr
  }

  @inline private def arrayClear(v: Array[AnyRef]): Unit = {
    Arrays.fill(v, null)
    v(0) = 1.asInstanceOf[AnyRef]
  }
}
