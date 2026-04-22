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

abstract class Buffer private[nio] (val _capacity: Int) {
  private[nio] type ElementType

  private[nio] type BufferType >: this.type <: Buffer {
    type ElementType = Buffer.this.ElementType
  }

  private var _limit: Int = capacity()
  private var _position: Int = 0
  private[nio] var _mark: Int = -1

  final def capacity(): Int = _capacity

  final def position(): Int = _position

  def position(newPosition: Int): Buffer = {
    if (BoundsChecks.isIndexInclusiveInvalid(newPosition, limit()))
      throw new IllegalArgumentException
    _position = newPosition
    if (_mark > newPosition)
      _mark = -1
    this
  }

  final def limit(): Int = _limit

  def limit(newLimit: Int): Buffer = {
    if (BoundsChecks.isIndexInclusiveInvalid(newLimit, capacity()))
      throw new IllegalArgumentException
    _limit = newLimit
    if (_position > newLimit) {
      _position = newLimit
      if (_mark > newLimit)
        _mark = -1
    }
    this
  }

  def mark(): Buffer = {
    _mark = _position
    this
  }

  def reset(): Buffer = {
    if (_mark == -1)
      throw new InvalidMarkException
    _position = _mark
    this
  }

  def clear(): Buffer = {
    _mark = -1
    _position = 0
    _limit = capacity()
    this
  }

  def flip(): Buffer = {
    _mark = -1
    _limit = _position
    _position = 0
    this
  }

  def rewind(): Buffer = {
    _mark = -1
    _position = 0
    this
  }

  @inline final def remaining(): Int = limit() - position()

  @inline final def hasRemaining(): Boolean = position() != limit()

  def isReadOnly(): Boolean

  def hasArray(): Boolean

  def array(): Array[ElementType]

  def arrayOffset(): Int

  def isDirect(): Boolean

  override def toString(): String =
    s"${getClass().getName()}[pos=${position()} lim=${limit()} cap=${capacity()}]"

  private[nio] def _array: Array[ElementType]
  private[nio] def _arrayOffset: Int

  private[nio] def load(index: Int): ElementType

  private[nio] def store(index: Int, elem: ElementType): Unit

  private[nio] def load(startIndex: Int, dst: Array[ElementType], offset: Int, length: Int): Unit

  private[nio] def store(startIndex: Int, src: Array[ElementType], offset: Int, length: Int): Unit

  private[nio] def _byteArray: Array[Byte] =
    throw new UnsupportedOperationException

  private[nio] def _byteArrayOffset: Int =
    throw new UnsupportedOperationException

  private[nio] def isBigEndian: Boolean =
    throw new UnsupportedOperationException

  @inline private[nio] def ensureNotReadOnly(): Unit = {
    if (isReadOnly())
      throw new ReadOnlyBufferException
  }

  @inline private[nio] def getPosAndAdvanceRead(): Int = {
    val p = _position
    if (p == limit())
      throw new BufferUnderflowException
    _position = p + 1
    p
  }

  @inline private[nio] def getPosAndAdvanceRead(length: Int): Int = {
    val p = _position
    val newPos = p + length
    if (newPos > limit())
      throw new BufferUnderflowException
    _position = newPos
    p
  }

  @inline private[nio] def getPosAndAdvanceWrite(): Int = {
    val p = _position
    if (p == limit())
      throw new BufferOverflowException
    _position = p + 1
    p
  }

  @inline private[nio] def getPosAndAdvanceWrite(length: Int): Int = {
    val p = _position
    val newPos = p + length
    if (newPos > limit())
      throw new BufferOverflowException
    _position = newPos
    p
  }
}

