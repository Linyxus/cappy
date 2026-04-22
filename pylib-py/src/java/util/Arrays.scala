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

import scala.annotation.tailrec

import java.util.internal.GenericArrayOps._
import java.util.internal.GenericArrayOps.given

import ScalaOps._

object Arrays {

  private object NaturalComparator extends Comparator[AnyRef] {
    @inline
    def compare(o1: AnyRef, o2: AnyRef): Int =
      o1.asInstanceOf[Comparable[AnyRef]].compareTo(o2)
  }

  @inline def ifNullUseNaturalComparator[T <: AnyRef](
      comparator: Comparator[_ >: T]): Comparator[_ >: T] = {
    if (comparator == null) NaturalComparator
    else comparator
  }

  // Implementation of the API

  @noinline def sort(a: Array[Int]): Unit =
    sortImpl[Int](a)(IntArrayOps)(using IntArrayOps, IntArrayOps)

  @noinline def sort(a: Array[Int], fromIndex: Int, toIndex: Int): Unit =
    sortRangeImpl[Int](a, fromIndex, toIndex)(IntArrayOps)(using IntArrayOps, IntArrayOps)

  @noinline def sort(a: Array[Long]): Unit =
    sortImpl[Long](a)(LongArrayOps)(using LongArrayOps, LongArrayOps)

  @noinline def sort(a: Array[Long], fromIndex: Int, toIndex: Int): Unit =
    sortRangeImpl[Long](a, fromIndex, toIndex)(LongArrayOps)(using LongArrayOps, LongArrayOps)

  @noinline def sort(a: Array[Short]): Unit =
    sortImpl[Short](a)(ShortArrayOps)(using ShortArrayOps, ShortArrayOps)

  @noinline def sort(a: Array[Short], fromIndex: Int, toIndex: Int): Unit =
    sortRangeImpl[Short](a, fromIndex, toIndex)(ShortArrayOps)(using ShortArrayOps, ShortArrayOps)

  @noinline def sort(a: Array[Char]): Unit =
    sortImpl[Char](a)(CharArrayOps)(using CharArrayOps, CharArrayOps)

  @noinline def sort(a: Array[Char], fromIndex: Int, toIndex: Int): Unit =
    sortRangeImpl[Char](a, fromIndex, toIndex)(CharArrayOps)(using CharArrayOps, CharArrayOps)

  @noinline def sort(a: Array[Byte]): Unit =
    sortImpl[Byte](a)(ByteArrayOps)(using ByteArrayOps, ByteArrayOps)

  @noinline def sort(a: Array[Byte], fromIndex: Int, toIndex: Int): Unit =
    sortRangeImpl[Byte](a, fromIndex, toIndex)(ByteArrayOps)(using ByteArrayOps, ByteArrayOps)

  @noinline def sort(a: Array[Float]): Unit =
    sortImpl[Float](a)(FloatArrayOps)(using FloatArrayOps, FloatArrayOps)

  @noinline def sort(a: Array[Float], fromIndex: Int, toIndex: Int): Unit =
    sortRangeImpl[Float](a, fromIndex, toIndex)(FloatArrayOps)(using FloatArrayOps, FloatArrayOps)

  @noinline def sort(a: Array[Double]): Unit =
    sortImpl[Double](a)(DoubleArrayOps)(using DoubleArrayOps, DoubleArrayOps)

  @noinline def sort(a: Array[Double], fromIndex: Int, toIndex: Int): Unit =
    sortRangeImpl[Double](a, fromIndex, toIndex)(DoubleArrayOps)(using DoubleArrayOps, DoubleArrayOps)

  @noinline def sort(a: Array[AnyRef]): Unit =
    sortImpl(a)(NaturalComparator)

  @noinline def sort(a: Array[AnyRef], fromIndex: Int, toIndex: Int): Unit =
    sortRangeImpl(a, fromIndex, toIndex)(NaturalComparator)

  @noinline def sort[T <: AnyRef](array: Array[T], comparator: Comparator[_ >: T]): Unit = {
    implicit val createOps = new TemplateArrayOps(array)
    sortImpl(array)(ifNullUseNaturalComparator(comparator))
  }

  @noinline def sort[T <: AnyRef](array: Array[T], fromIndex: Int, toIndex: Int,
      comparator: Comparator[_ >: T]): Unit = {
    implicit val createOps = new TemplateArrayOps(array)
    sortRangeImpl(array, fromIndex, toIndex)(ifNullUseNaturalComparator(comparator))
  }

  @inline
  private def sortRangeImpl[T](a: Array[T], fromIndex: Int, toIndex: Int)(
      comparator: Comparator[_ >: T])(
      implicit ops: ArrayOps[T], createOps: ArrayCreateOps[T]): Unit = {
    checkRangeIndices(a, fromIndex, toIndex)(ops)
    stableMergeSort[T](a, fromIndex, toIndex)(comparator)
  }

  @inline
  private def sortImpl[T](a: Array[T])(comparator: Comparator[_ >: T])(
      implicit ops: ArrayOps[T], createOps: ArrayCreateOps[T]): Unit = {
    stableMergeSort[T](a, 0, ops.length(a))(comparator)
  }

  private final val inPlaceSortThreshold = 16

  /** Sort array `a` with merge sort and insertion sort. */
  @inline
  private def stableMergeSort[T](a: Array[T], start: Int, end: Int)(
      comparator: Comparator[_ >: T])(
      implicit ops: ArrayOps[T], createOps: ArrayCreateOps[T]): Unit = {
    if (end - start > inPlaceSortThreshold)
      stableSplitMerge(a, createOps.create(ops.length(a)), start, end)(comparator)
    else
      insertionSort(a, start, end)(comparator)
  }

  @noinline
  private def stableSplitMerge[T](a: Array[T], temp: Array[T], start: Int,
      end: Int)(
      comparator: Comparator[_ >: T])(
      implicit ops: ArrayOps[T]): Unit = {
    val length = end - start
    if (length > inPlaceSortThreshold) {
      val middle = start + (length / 2)
      stableSplitMerge(a, temp, start, middle)(comparator)
      stableSplitMerge(a, temp, middle, end)(comparator)
      stableMerge(a, temp, start, middle, end)(comparator)
      System.arraycopy(temp, start, a, start, length)
    } else {
      insertionSort(a, start, end)(comparator)
    }
  }

  @inline
  private def stableMerge[T](a: Array[T], temp: Array[T], start: Int,
      middle: Int, end: Int)(
      comparator: Comparator[_ >: T])(
      implicit ops: ArrayOps[T]): Unit = {
    var outIndex = start
    var leftInIndex = start
    var rightInIndex = middle
    while (outIndex < end) {
      if (leftInIndex < middle &&
          (rightInIndex >= end || comparator.compare(
              ops.get(a, leftInIndex), ops.get(a, rightInIndex)) <= 0)) {
        ops.set(temp, outIndex, ops.get(a, leftInIndex))
        leftInIndex += 1
      } else {
        ops.set(temp, outIndex, ops.get(a, rightInIndex))
        rightInIndex += 1
      }
      outIndex += 1
    }
  }

  /* ArrayOps[T] and Comparator[T] might be slow especially for boxed
   * primitives, so use a binary search variant of insertion sort.
   * The caller must pass end >= start or math will fail. Also, start >= 0.
   */
  @noinline
  private final def insertionSort[T](a: Array[T], start: Int, end: Int)(
      comparator: Comparator[_ >: T])(
      implicit ops: ArrayOps[T]): Unit = {
    val n = end - start
    if (n >= 2) {
      val aStart = ops.get(a, start)
      val aStartPlusOne = ops.get(a, start + 1)
      if (comparator.compare(aStart, aStartPlusOne) > 0) {
        ops.set(a, start, aStartPlusOne)
        ops.set(a, start + 1, aStart)
      }

      var m = 2
      while (m < n) {
        // Speed up already-sorted case by checking last element first
        val next = ops.get(a, start + m)
        if (comparator.compare(next, ops.get(a, start + m - 1)) < 0) {
          var iA = start
          var iB = start + m - 1
          while (iB - iA > 1) {
            val ix = (iA + iB) >>> 1 // Use bit shift to get unsigned div by 2
            if (comparator.compare(next, ops.get(a, ix)) < 0)
              iB = ix
            else
              iA = ix
          }
          val ix = iA + (if (comparator.compare(next, ops.get(a, iA)) < 0) 0 else 1)
          var i = start + m
          while (i > ix) {
            ops.set(a, i, ops.get(a, i - 1))
            i -= 1
          }
          ops.set(a, ix, next)
        }
        m += 1
      }
    }
  }

  @noinline def binarySearch(a: Array[Long], key: Long): Int =
    binarySearchImpl[Long](a, 0, a.length, key)(LongArrayOps)(using LongArrayOps)

  @noinline def binarySearch(a: Array[Long], startIndex: Int, endIndex: Int, key: Long): Int = {
    checkRangeIndices[Long](a, startIndex, endIndex)(using LongArrayOps)
    binarySearchImpl[Long](a, startIndex, endIndex, key)(LongArrayOps)(using LongArrayOps)
  }

  @noinline def binarySearch(a: Array[Int], key: Int): Int =
    binarySearchImpl[Int](a, 0, a.length, key)(IntArrayOps)(using IntArrayOps)

  @noinline def binarySearch(a: Array[Int], startIndex: Int, endIndex: Int, key: Int): Int = {
    checkRangeIndices[Int](a, startIndex, endIndex)(using IntArrayOps)
    binarySearchImpl[Int](a, startIndex, endIndex, key)(IntArrayOps)(using IntArrayOps)
  }

  @noinline def binarySearch(a: Array[Short], key: Short): Int =
    binarySearchImpl[Short](a, 0, a.length, key)(ShortArrayOps)(using ShortArrayOps)

  @noinline def binarySearch(a: Array[Short], startIndex: Int, endIndex: Int, key: Short): Int = {
    checkRangeIndices[Short](a, startIndex, endIndex)(using ShortArrayOps)
    binarySearchImpl[Short](a, startIndex, endIndex, key)(ShortArrayOps)(using ShortArrayOps)
  }

  @noinline def binarySearch(a: Array[Char], key: Char): Int =
    binarySearchImpl[Char](a, 0, a.length, key)(CharArrayOps)(using CharArrayOps)

  @noinline def binarySearch(a: Array[Char], startIndex: Int, endIndex: Int, key: Char): Int = {
    checkRangeIndices[Char](a, startIndex, endIndex)(using CharArrayOps)
    binarySearchImpl[Char](a, startIndex, endIndex, key)(CharArrayOps)(using CharArrayOps)
  }

  @noinline def binarySearch(a: Array[Byte], key: Byte): Int =
    binarySearchImpl[Byte](a, 0, a.length, key)(ByteArrayOps)(using ByteArrayOps)

  @noinline def binarySearch(a: Array[Byte], startIndex: Int, endIndex: Int, key: Byte): Int = {
    checkRangeIndices[Byte](a, startIndex, endIndex)(using ByteArrayOps)
    binarySearchImpl[Byte](a, startIndex, endIndex, key)(ByteArrayOps)(using ByteArrayOps)
  }

  @noinline def binarySearch(a: Array[Double], key: Double): Int =
    binarySearchImpl[Double](a, 0, a.length, key)(DoubleArrayOps)(using DoubleArrayOps)

  @noinline def binarySearch(a: Array[Double], startIndex: Int, endIndex: Int, key: Double): Int = {
    checkRangeIndices[Double](a, startIndex, endIndex)(using DoubleArrayOps)
    binarySearchImpl[Double](a, startIndex, endIndex, key)(DoubleArrayOps)(using DoubleArrayOps)
  }

  @noinline def binarySearch(a: Array[Float], key: Float): Int =
    binarySearchImpl[Float](a, 0, a.length, key)(FloatArrayOps)(using FloatArrayOps)

  @noinline def binarySearch(a: Array[Float], startIndex: Int, endIndex: Int, key: Float): Int = {
    checkRangeIndices[Float](a, startIndex, endIndex)(using FloatArrayOps)
    binarySearchImpl[Float](a, startIndex, endIndex, key)(FloatArrayOps)(using FloatArrayOps)
  }

  @noinline def binarySearch(a: Array[AnyRef], key: AnyRef): Int =
    binarySearchImpl(a, 0, a.length, key)(NaturalComparator)

  @noinline def binarySearch(a: Array[AnyRef], startIndex: Int, endIndex: Int, key: AnyRef): Int = {
    checkRangeIndices(a, startIndex, endIndex)
    binarySearchImpl(a, startIndex, endIndex, key)(NaturalComparator)
  }

  @noinline def binarySearch[T <: AnyRef](a: Array[T], key: T, c: Comparator[_ >: T]): Int =
    binarySearchImpl[T](a, 0, a.length, key)(ifNullUseNaturalComparator(c))

  @noinline def binarySearch[T <: AnyRef](a: Array[T], startIndex: Int, endIndex: Int, key: T,
      c: Comparator[_ >: T]): Int = {
    checkRangeIndices(a, startIndex, endIndex)
    binarySearchImpl[T](a, startIndex, endIndex, key)(ifNullUseNaturalComparator(c))
  }

  @inline
  private def binarySearchImpl[T](a: Array[T], startIndex: Int, endIndex: Int,
      key: T)(
      comparator: Comparator[_ >: T])(
      implicit ops: ArrayOps[T]): Int = {
    // scalastyle:off return
    var low = startIndex
    var high = endIndex
    while (low != high) {
      // Indices are unsigned 31-bit integers, so this does not overflow.
      val mid = (low + high) >>> 1
      val elem = ops.get(a, mid)
      val comparison = comparator.compare(key, elem)
      if (comparison < 0) {
        high = mid
      } else if (comparison == 0) {
        return mid
      } else {
        low = mid + 1
      }
    }
    -low - 1
    // scalastyle:on return
  }

  @noinline def equals(a: Array[Long], b: Array[Long]): Boolean =
    equalsImpl[Long](a, b)(using LongArrayOps)

  @noinline def equals(a: Array[Int], b: Array[Int]): Boolean =
    equalsImpl[Int](a, b)(using IntArrayOps)

  @noinline def equals(a: Array[Short], b: Array[Short]): Boolean =
    equalsImpl[Short](a, b)(using ShortArrayOps)

  @noinline def equals(a: Array[Char], b: Array[Char]): Boolean =
    equalsImpl[Char](a, b)(using CharArrayOps)

  @noinline def equals(a: Array[Byte], b: Array[Byte]): Boolean =
    equalsImpl[Byte](a, b)(using ByteArrayOps)

  @noinline def equals(a: Array[Boolean], b: Array[Boolean]): Boolean =
    equalsImpl[Boolean](a, b)(using BooleanArrayOps)

  @noinline def equals(a: Array[Double], b: Array[Double]): Boolean =
    equalsImpl[Double](a, b)(using DoubleArrayOps)

  @noinline def equals(a: Array[Float], b: Array[Float]): Boolean =
    equalsImpl[Float](a, b)(using FloatArrayOps)

  @noinline def equals(a: Array[AnyRef], b: Array[AnyRef]): Boolean =
    equalsImpl(a, b)

  @inline
  private def equalsImpl[T](a: Array[T], b: Array[T])(
      implicit ops: ArrayOps[T]): Boolean = {
    // scalastyle:off return
    if (a eq b)
      return true
    if (a == null || b == null)
      return false
    val length = ops.length(a)
    if (ops.length(b) != length)
      return false
    var i = 0
    while (i != length) {
      if (!Objects.equals(ops.get(a, i), ops.get(b, i)))
        return false
      i += 1
    }
    true
    // scalastyle:on return
  }

  @noinline def fill(a: Array[Long], value: Long): Unit =
    fillImpl[Long](a, 0, a.length, value, checkIndices = false)(using LongArrayOps)

  @noinline def fill(a: Array[Long], fromIndex: Int, toIndex: Int, value: Long): Unit =
    fillImpl[Long](a, fromIndex, toIndex, value)(using LongArrayOps)

  @noinline def fill(a: Array[Int], value: Int): Unit =
    fillImpl[Int](a, 0, a.length, value, checkIndices = false)(using IntArrayOps)

  @noinline def fill(a: Array[Int], fromIndex: Int, toIndex: Int, value: Int): Unit =
    fillImpl[Int](a, fromIndex, toIndex, value)(using IntArrayOps)

  @noinline def fill(a: Array[Short], value: Short): Unit =
    fillImpl[Short](a, 0, a.length, value, checkIndices = false)(using ShortArrayOps)

  @noinline def fill(a: Array[Short], fromIndex: Int, toIndex: Int, value: Short): Unit =
    fillImpl[Short](a, fromIndex, toIndex, value)(using ShortArrayOps)

  @noinline def fill(a: Array[Char], value: Char): Unit =
    fillImpl[Char](a, 0, a.length, value, checkIndices = false)(using CharArrayOps)

  @noinline def fill(a: Array[Char], fromIndex: Int, toIndex: Int, value: Char): Unit =
    fillImpl[Char](a, fromIndex, toIndex, value)(using CharArrayOps)

  @noinline def fill(a: Array[Byte], value: Byte): Unit =
    fillImpl[Byte](a, 0, a.length, value, checkIndices = false)(using ByteArrayOps)

  @noinline def fill(a: Array[Byte], fromIndex: Int, toIndex: Int, value: Byte): Unit =
    fillImpl[Byte](a, fromIndex, toIndex, value)(using ByteArrayOps)

  @noinline def fill(a: Array[Boolean], value: Boolean): Unit =
    fillImpl[Boolean](a, 0, a.length, value, checkIndices = false)(using BooleanArrayOps)

  @noinline def fill(a: Array[Boolean], fromIndex: Int, toIndex: Int, value: Boolean): Unit =
    fillImpl[Boolean](a, fromIndex, toIndex, value)(using BooleanArrayOps)

  @noinline def fill(a: Array[Double], value: Double): Unit =
    fillImpl[Double](a, 0, a.length, value, checkIndices = false)(using DoubleArrayOps)

  @noinline def fill(a: Array[Double], fromIndex: Int, toIndex: Int, value: Double): Unit =
    fillImpl[Double](a, fromIndex, toIndex, value)(using DoubleArrayOps)

  @noinline def fill(a: Array[Float], value: Float): Unit =
    fillImpl[Float](a, 0, a.length, value, checkIndices = false)(using FloatArrayOps)

  @noinline def fill(a: Array[Float], fromIndex: Int, toIndex: Int, value: Float): Unit =
    fillImpl[Float](a, fromIndex, toIndex, value)(using FloatArrayOps)

  @noinline def fill(a: Array[AnyRef], value: AnyRef): Unit =
    fillImpl(a, 0, a.length, value, checkIndices = false)

  @noinline def fill(a: Array[AnyRef], fromIndex: Int, toIndex: Int, value: AnyRef): Unit =
    fillImpl(a, fromIndex, toIndex, value)

  @inline
  private def fillImpl[T](a: Array[T], fromIndex: Int, toIndex: Int,
      value: T, checkIndices: Boolean = true)(
      implicit ops: ArrayOps[T]): Unit = {
    if (checkIndices)
      checkRangeIndices(a, fromIndex, toIndex)
    var i = fromIndex
    while (i != toIndex) {
      ops.set(a, i, value)
      i += 1
    }
  }

  @noinline def copyOf[T <: AnyRef](original: Array[T], newLength: Int): Array[T] = {
    implicit val tops = new TemplateArrayOps(original)
    copyOfImpl(original, newLength)
  }

  @noinline def copyOf[T <: AnyRef, U <: AnyRef](original: Array[U], newLength: Int,
      newType: Class[?]): Array[T] = {
    implicit val tops = new ClassArrayOps[T](newType)
    copyOfImpl(original, newLength)
  }

  @noinline def copyOf(original: Array[Byte], newLength: Int): Array[Byte] =
    copyOfImpl[Byte, Byte](original, newLength)(using ByteArrayOps, ByteArrayOps)

  @noinline def copyOf(original: Array[Short], newLength: Int): Array[Short] =
    copyOfImpl[Short, Short](original, newLength)(using ShortArrayOps, ShortArrayOps)

  @noinline def copyOf(original: Array[Int], newLength: Int): Array[Int] =
    copyOfImpl[Int, Int](original, newLength)(using IntArrayOps, IntArrayOps)

  @noinline def copyOf(original: Array[Long], newLength: Int): Array[Long] =
    copyOfImpl[Long, Long](original, newLength)(using LongArrayOps, LongArrayOps)

  @noinline def copyOf(original: Array[Char], newLength: Int): Array[Char] =
    copyOfImpl[Char, Char](original, newLength)(using CharArrayOps, CharArrayOps)

  @noinline def copyOf(original: Array[Float], newLength: Int): Array[Float] =
    copyOfImpl[Float, Float](original, newLength)(using FloatArrayOps, FloatArrayOps)

  @noinline def copyOf(original: Array[Double], newLength: Int): Array[Double] =
    copyOfImpl[Double, Double](original, newLength)(using DoubleArrayOps, DoubleArrayOps)

  @noinline def copyOf(original: Array[Boolean], newLength: Int): Array[Boolean] =
    copyOfImpl[Boolean, Boolean](original, newLength)(using BooleanArrayOps, BooleanArrayOps)

  @inline
  private def copyOfImpl[U, T](original: Array[U], newLength: Int)(
      implicit uops: ArrayOps[U], tops: ArrayCreateOps[T]): Array[T] = {
    val copyLength = Math.min(newLength, uops.length(original))
    val ret = tops.create(newLength)
    System.arraycopy(original, 0, ret, 0, copyLength)
    ret
  }

  @noinline def copyOfRange[T <: AnyRef](original: Array[T], from: Int, to: Int): Array[T] = {
    implicit val tops = new TemplateArrayOps(original)
    copyOfRangeImpl(original, from, to)
  }

  @noinline def copyOfRange[T <: AnyRef, U <: AnyRef](original: Array[U],
      from: Int, to: Int, newType: Class[?]): Array[T] = {
    implicit val tops = new ClassArrayOps[T](newType)
    copyOfRangeImpl(original, from, to)
  }

  @noinline def copyOfRange(original: Array[Byte], start: Int, end: Int): Array[Byte] =
    copyOfRangeImpl[Byte, Byte](original, start, end)(using ByteArrayOps, ByteArrayOps)

  @noinline def copyOfRange(original: Array[Short], start: Int, end: Int): Array[Short] =
    copyOfRangeImpl[Short, Short](original, start, end)(using ShortArrayOps, ShortArrayOps)

  @noinline def copyOfRange(original: Array[Int], start: Int, end: Int): Array[Int] =
    copyOfRangeImpl[Int, Int](original, start, end)(using IntArrayOps, IntArrayOps)

  @noinline def copyOfRange(original: Array[Long], start: Int, end: Int): Array[Long] =
    copyOfRangeImpl[Long, Long](original, start, end)(using LongArrayOps, LongArrayOps)

  @noinline def copyOfRange(original: Array[Char], start: Int, end: Int): Array[Char] =
    copyOfRangeImpl[Char, Char](original, start, end)(using CharArrayOps, CharArrayOps)

  @noinline def copyOfRange(original: Array[Float], start: Int, end: Int): Array[Float] =
    copyOfRangeImpl[Float, Float](original, start, end)(using FloatArrayOps, FloatArrayOps)

  @noinline def copyOfRange(original: Array[Double], start: Int, end: Int): Array[Double] =
    copyOfRangeImpl[Double, Double](original, start, end)(using DoubleArrayOps, DoubleArrayOps)

  @noinline def copyOfRange(original: Array[Boolean], start: Int, end: Int): Array[Boolean] =
    copyOfRangeImpl[Boolean, Boolean](original, start, end)(using BooleanArrayOps, BooleanArrayOps)

  @inline
  private def copyOfRangeImpl[T, U](original: Array[U], start: Int, end: Int)(
      implicit uops: ArrayOps[U], tops: ArrayCreateOps[T]): Array[T] = {
    if (start > end)
      throw new IllegalArgumentException("" + start + " > " + end)

    val originalLength = uops.length(original)
    val retLength = end - start
    val copyLength = Math.min(retLength, originalLength - start)
    val ret = tops.create(retLength)
    System.arraycopy(original, start, ret, 0, copyLength)
    ret
  }

  @noinline def asList[T <: AnyRef](a: Array[T]): List[T] = {
    new AbstractList[T] with RandomAccess {
      def size(): Int =
        a.length

      def get(index: Int): T =
        a(index)

      override def set(index: Int, element: T): T = {
        val ret = a(index)
        a(index) = element
        ret
      }
    }
  }

  @noinline def hashCode(a: Array[Long]): Int =
    hashCodeImpl[Long](a)(using LongArrayOps)

  @noinline def hashCode(a: Array[Int]): Int =
    hashCodeImpl[Int](a)(using IntArrayOps)

  @noinline def hashCode(a: Array[Short]): Int =
    hashCodeImpl[Short](a)(using ShortArrayOps)

  @noinline def hashCode(a: Array[Char]): Int =
    hashCodeImpl[Char](a)(using CharArrayOps)

  @noinline def hashCode(a: Array[Byte]): Int =
    hashCodeImpl[Byte](a)(using ByteArrayOps)

  @noinline def hashCode(a: Array[Boolean]): Int =
    hashCodeImpl[Boolean](a)(using BooleanArrayOps)

  @noinline def hashCode(a: Array[Float]): Int =
    hashCodeImpl[Float](a)(using FloatArrayOps)

  @noinline def hashCode(a: Array[Double]): Int =
    hashCodeImpl[Double](a)(using DoubleArrayOps)

  @noinline def hashCode(a: Array[AnyRef]): Int =
    hashCodeImpl(a)

  @inline
  private def hashCodeImpl[T](a: Array[T])(implicit ops: ArrayOps[T]): Int = {
    if (a == null) {
      0
    } else {
      var acc = 1
      val length = ops.length(a)
      var i = 0
      while (i != length) {
        acc = 31 * acc + Objects.hashCode(ops.get(a, i))
        i += 1
      }
      acc
    }
  }

  @noinline def deepHashCode(a: Array[AnyRef]): Int = {
    def rec(a: Array[AnyRef]): Int = {
      var acc = 1
      val length = a.length
      var i = 0
      while (i != length) {
        acc = 31 * acc + (a(i) match {
          case elem: Array[AnyRef]  => rec(elem)
          case elem: Array[Long]    => hashCode(elem)
          case elem: Array[Int]     => hashCode(elem)
          case elem: Array[Short]   => hashCode(elem)
          case elem: Array[Char]    => hashCode(elem)
          case elem: Array[Byte]    => hashCode(elem)
          case elem: Array[Boolean] => hashCode(elem)
          case elem: Array[Float]   => hashCode(elem)
          case elem: Array[Double]  => hashCode(elem)
          case elem                 => Objects.hashCode(elem)
        })
        i += 1
      }
      acc
    }

    if (a == null) 0
    else rec(a)
  }

  @noinline def deepEquals(a1: Array[AnyRef], a2: Array[AnyRef]): Boolean = {
    // scalastyle:off return
    if (a1 eq a2)
      return true
    if (a1 == null || a2 == null)
      return false
    val length = a1.length
    if (a2.length != length)
      return false
    var i = 0
    while (i != length) {
      if (!Objects.deepEquals(a1(i), a2(i)))
        return false
      i += 1
    }
    true
    // scalastyle:on return
  }

  @noinline def toString(a: Array[Long]): String =
    toStringImpl[Long](a)(using LongArrayOps)

  @noinline def toString(a: Array[Int]): String =
    toStringImpl[Int](a)(using IntArrayOps)

  @noinline def toString(a: Array[Short]): String =
    toStringImpl[Short](a)(using ShortArrayOps)

  @noinline def toString(a: Array[Char]): String =
    toStringImpl[Char](a)(using CharArrayOps)

  @noinline def toString(a: Array[Byte]): String =
    toStringImpl[Byte](a)(using ByteArrayOps)

  @noinline def toString(a: Array[Boolean]): String =
    toStringImpl[Boolean](a)(using BooleanArrayOps)

  @noinline def toString(a: Array[Float]): String =
    toStringImpl[Float](a)(using FloatArrayOps)

  @noinline def toString(a: Array[Double]): String =
    toStringImpl[Double](a)(using DoubleArrayOps)

  @noinline def toString(a: Array[AnyRef]): String =
    toStringImpl[AnyRef](a)

  @inline
  private def toStringImpl[T](a: Array[T])(implicit ops: ArrayOps[T]): String = {
    if (a == null) {
      "null"
    } else {
      var result = "["
      val length = ops.length(a)
      var i = 0
      while (i != length) {
        if (i != 0)
          result += ", "
        result += ops.get(a, i)
        i += 1
      }
      result + "]"
    }
  }

  def deepToString(a: Array[AnyRef]): String = {
    /* Keep a short stack of the arrays on the current recursion path.
     * This preserves the Scala.js algorithm without depending on js.Array.
     */
    var seen = new Array[Array[AnyRef]](8)
    var seenSize = 0

    @inline def wasSeen(a: Array[AnyRef]): Boolean = {
      var i = 0
      while (i < seenSize) {
        if (seen(i) eq a)
          return true
        i += 1
      }
      false
    }

    @inline def pushSeen(a: Array[AnyRef]): Unit = {
      if (seenSize == seen.length) {
        val grown = new Array[Array[AnyRef]](seenSize * 2)
        System.arraycopy(seen, 0, grown, 0, seenSize)
        seen = grown
      }
      seen(seenSize) = a
      seenSize += 1
    }

    @inline def popSeen(): Unit = {
      seenSize -= 1
    }

    def rec(a: Array[AnyRef]): String = {
      var result = "["
      val length = a.length
      var i = 0
      while (i != length) {
        if (i != 0)
          result += ", "
        val elem = a(i)
        if (elem.isInstanceOf[Array[AnyRef]]) {
          val nestedArray = elem.asInstanceOf[Array[AnyRef]]
          if ((nestedArray eq a) || wasSeen(nestedArray)) {
            result += "[...]"
          } else {
            pushSeen(a)
            result += rec(nestedArray)
            popSeen()
          }
        } else if (elem.isInstanceOf[Array[Long]]) {
          result += toString(elem.asInstanceOf[Array[Long]])
        } else if (elem.isInstanceOf[Array[Int]]) {
          result += toString(elem.asInstanceOf[Array[Int]])
        } else if (elem.isInstanceOf[Array[Short]]) {
          result += toString(elem.asInstanceOf[Array[Short]])
        } else if (elem.isInstanceOf[Array[Byte]]) {
          result += toString(elem.asInstanceOf[Array[Byte]])
        } else if (elem.isInstanceOf[Array[Char]]) {
          result += toString(elem.asInstanceOf[Array[Char]])
        } else if (elem.isInstanceOf[Array[Boolean]]) {
          result += toString(elem.asInstanceOf[Array[Boolean]])
        } else if (elem.isInstanceOf[Array[Float]]) {
          result += toString(elem.asInstanceOf[Array[Float]])
        } else if (elem.isInstanceOf[Array[Double]]) {
          result += toString(elem.asInstanceOf[Array[Double]])
        } else {
          result += String.valueOf(elem)
        }
        i += 1
      }
      result + "]"
    }

    if (a == null) "null"
    else rec(a)
  }

  @inline
  private def checkRangeIndices[T](a: Array[T], start: Int, end: Int)(
      implicit ops: ArrayOps[T]): Unit = {
    if (start > end)
      throw new IllegalArgumentException("fromIndex(" + start + ") > toIndex(" + end + ")")

    // bounds checks
    if (start < 0)
      ops.get(a, start)

    if (end > 0)
      ops.get(a, end - 1)
  }
}
