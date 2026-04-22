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

package java.nio

import scala.language.unsafeNulls

object ShortBuffer {
  private final val HashSeed = 383731478

  def allocate(capacity: Int): ShortBuffer = {
    BoundsChecks.checkCapacity(capacity)
    wrap(new Array[Short](capacity))
  }

  def wrap(array: Array[Short], offset: Int, length: Int): ShortBuffer =
    HeapShortBuffer.wrap(array, 0, array.length, offset, length, false)

  def wrap(array: Array[Short]): ShortBuffer =
    wrap(array, 0, array.length)
}

abstract class ShortBuffer private[nio] (
    _capacity: Int,
    private[nio] val _array: Array[Short],
    private[nio] val _arrayOffset: Int
) extends Buffer(_capacity)
    with Comparable[ShortBuffer] {

  private[nio] type ElementType = Short
  private[nio] type BufferType = ShortBuffer

  def this(_capacity: Int) = this(_capacity, null, -1)

  def slice(): ShortBuffer
  def duplicate(): ShortBuffer
  def asReadOnlyBuffer(): ShortBuffer
  def get(): Short
  def put(s: Short): ShortBuffer
  def get(index: Int): Short
  def put(index: Int, s: Short): ShortBuffer

  @noinline
  def get(dst: Array[Short], offset: Int, length: Int): ShortBuffer =
    GenBuffer(this).getArray(dst, offset, length)

  def get(dst: Array[Short]): ShortBuffer =
    get(dst, 0, dst.length)

  @noinline
  def put(src: ShortBuffer): ShortBuffer =
    GenBuffer(this).putBuffer(src)

  @noinline
  def put(src: Array[Short], offset: Int, length: Int): ShortBuffer =
    GenBuffer(this).putArray(src, offset, length)

  final def put(src: Array[Short]): ShortBuffer =
    put(src, 0, src.length)

  @inline final def hasArray(): Boolean =
    GenBuffer(this).generic_hasArray()

  @inline final def array(): Array[Short] =
    GenBuffer(this).generic_array().asInstanceOf[Array[Short]]

  @inline final def arrayOffset(): Int =
    GenBuffer(this).generic_arrayOffset()

  @inline override def position(newPosition: Int): ShortBuffer = { super.position(newPosition); this }
  @inline override def limit(newLimit: Int): ShortBuffer = { super.limit(newLimit); this }
  @inline override def mark(): ShortBuffer = { super.mark(); this }
  @inline override def reset(): ShortBuffer = { super.reset(); this }
  @inline override def clear(): ShortBuffer = { super.clear(); this }
  @inline override def flip(): ShortBuffer = { super.flip(); this }
  @inline override def rewind(): ShortBuffer = { super.rewind(); this }

  def compact(): ShortBuffer
  def isDirect(): Boolean

  @noinline
  override def hashCode(): Int =
    GenBuffer(this).generic_hashCode(ShortBuffer.HashSeed)

  override def equals(that: Any): Boolean = that match
    case that: ShortBuffer => compareTo(that) == 0
    case _                 => false

  @noinline
  def compareTo(that: ShortBuffer): Int =
    GenBuffer(this).generic_compareTo(that)(java.lang.Short.compare(_, _))

  def order(): ByteOrder

  private[nio] def load(index: Int): Short
  private[nio] def store(index: Int, elem: Short): Unit

  @inline
  private[nio] def load(startIndex: Int, dst: Array[Short], offset: Int, length: Int): Unit =
    GenBuffer(this).loadInto(startIndex, dst, offset, length)

  @inline
  private[nio] def store(startIndex: Int, src: Array[Short], offset: Int, length: Int): Unit =
    GenBuffer(this).storeFrom(startIndex, src, offset, length)
}
