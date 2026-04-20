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

import java.lang.Cloneable
import java.lang.BoundsChecks

import scala.language.unsafeNulls

class ArrayList[E] private (private var inner: Array[AnyRef], private var _size: Int)
    extends AbstractList[E] with RandomAccess with Cloneable with Serializable {

  def this(initialCapacity: Int) = {
    this(
      {
        BoundsChecks.checkCapacity(initialCapacity)
        new Array[AnyRef](Math.max(initialCapacity, 16))
      },
      0
    )
  }

  def this() = this(16)

  def this(c: Collection[_ <: E]) = {
    this(c.size())
    addAll(c)
  }

  def trimToSize(): Unit = {
    if (inner.length != _size)
      inner = Arrays.copyOf(inner, _size)
  }

  def ensureCapacity(minCapacity: Int): Unit = {
    if (inner.length < minCapacity)
      resizeTo(Math.max(Math.max(inner.length * 2, 16), minCapacity))
  }

  def size(): Int =
    _size

  override def clone(): AnyRef =
    new ArrayList(inner.clone(), _size)

  def get(index: Int): E = {
    checkIndexInBounds(index)
    inner(index).asInstanceOf[E]
  }

  override def set(index: Int, element: E): E = {
    val oldValue = get(index)
    inner(index) = element.asInstanceOf[AnyRef]
    oldValue
  }

  override def add(e: E): Boolean = {
    ensureCapacity(_size + 1)
    inner(_size) = e.asInstanceOf[AnyRef]
    _size += 1
    true
  }

  override def add(index: Int, element: E): Unit = {
    checkIndexOnBounds(index)
    ensureCapacity(_size + 1)
    if (index < _size)
      System.arraycopy(inner, index, inner, index + 1, _size - index)
    inner(index) = element.asInstanceOf[AnyRef]
    _size += 1
  }

  override def remove(index: Int): E = {
    checkIndexInBounds(index)
    val removed = inner(index).asInstanceOf[E]
    val tailCount = _size - index - 1
    if (tailCount > 0)
      System.arraycopy(inner, index + 1, inner, index, tailCount)
    _size -= 1
    inner(_size) = null
    removed
  }

  override def clear(): Unit = {
    Arrays.fill(inner, 0, _size, null)
    _size = 0
  }

  override def addAll(index: Int, c: Collection[_ <: E]): Boolean = {
    checkIndexOnBounds(index)

    val count = c.size()
    if (count == 0) {
      false
    } else {
      ensureCapacity(_size + count)
      if (index < _size)
        System.arraycopy(inner, index, inner, index + count, _size - index)

      c match {
        case other: ArrayList[?] =>
          System.arraycopy(other.inner, 0, inner, index, other._size)
        case _ =>
          writeCollectionAt(index, c)
      }

      _size += count
      true
    }
  }

  override protected def removeRange(fromIndex: Int, toIndex: Int): Unit = {
    val count = BoundsChecks.checkStartEnd(fromIndex, toIndex, _size)
    if (count != 0) {
      System.arraycopy(inner, toIndex, inner, fromIndex, _size - toIndex)
      val newSize = _size - count
      Arrays.fill(inner, newSize, _size, null)
      _size = newSize
    }
  }

  private def resizeTo(newCapacity: Int): Unit =
    inner = Arrays.copyOf(inner, newCapacity)

  private def writeCollectionAt(index: Int, c: Collection[_ <: E]): Unit = {
    val iter = c.iterator()
    var writeIndex = index
    while (iter.hasNext()) {
      inner(writeIndex) = iter.next().asInstanceOf[AnyRef]
      writeIndex += 1
    }
  }
}
