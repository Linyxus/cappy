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

private[nio] object ByteArrayBits {
  def apply(array: Array[Byte], arrayOffset: Int, isBigEndian: Boolean, indexMultiplier: Int = 1): ByteArrayBits =
    new ByteArrayBits(array, arrayOffset, isBigEndian, indexMultiplier)
}

@inline
private[nio] final class ByteArrayBits(
    array: Array[Byte],
    arrayOffset: Int,
    isBigEndian: Boolean,
    indexMultiplier: Int
) {
  def loadChar(index: Int): Char = {
    val idx = byteIndex(index)
    if (isBigEndian)
      (((array(idx) & 0xff) << 8) | (array(idx + 1) & 0xff)).toChar
    else
      (((array(idx + 1) & 0xff) << 8) | (array(idx) & 0xff)).toChar
  }

  def loadShort(index: Int): Short =
    loadChar(index).toShort

  def loadInt(index: Int): Int = {
    val idx = byteIndex(index)
    if (isBigEndian)
      ((array(idx) & 0xff) << 24) |
        ((array(idx + 1) & 0xff) << 16) |
        ((array(idx + 2) & 0xff) << 8) |
        (array(idx + 3) & 0xff)
    else
      ((array(idx + 3) & 0xff) << 24) |
        ((array(idx + 2) & 0xff) << 16) |
        ((array(idx + 1) & 0xff) << 8) |
        (array(idx) & 0xff)
  }

  def loadLong(index: Int): Long = {
    val idx = byteIndex(index)
    if (isBigEndian)
      ((array(idx).toLong & 0xffL) << 56) |
        ((array(idx + 1).toLong & 0xffL) << 48) |
        ((array(idx + 2).toLong & 0xffL) << 40) |
        ((array(idx + 3).toLong & 0xffL) << 32) |
        ((array(idx + 4).toLong & 0xffL) << 24) |
        ((array(idx + 5).toLong & 0xffL) << 16) |
        ((array(idx + 6).toLong & 0xffL) << 8) |
        (array(idx + 7).toLong & 0xffL)
    else
      ((array(idx + 7).toLong & 0xffL) << 56) |
        ((array(idx + 6).toLong & 0xffL) << 48) |
        ((array(idx + 5).toLong & 0xffL) << 40) |
        ((array(idx + 4).toLong & 0xffL) << 32) |
        ((array(idx + 3).toLong & 0xffL) << 24) |
        ((array(idx + 2).toLong & 0xffL) << 16) |
        ((array(idx + 1).toLong & 0xffL) << 8) |
        (array(idx).toLong & 0xffL)
  }

  def loadFloat(index: Int): Float =
    java.lang.Float.intBitsToFloat(loadInt(index))

  def loadDouble(index: Int): Double =
    java.lang.Double.longBitsToDouble(loadLong(index))

  def storeChar(index: Int, v: Char): Unit = {
    val idx = byteIndex(index)
    val value = v.toInt
    if (isBigEndian) {
      array(idx) = (value >> 8).toByte
      array(idx + 1) = value.toByte
    } else {
      array(idx) = value.toByte
      array(idx + 1) = (value >> 8).toByte
    }
  }

  def storeShort(index: Int, v: Short): Unit =
    storeChar(index, v.toChar)

  def storeInt(index: Int, v: Int): Unit = {
    val idx = byteIndex(index)
    if (isBigEndian) {
      array(idx) = (v >> 24).toByte
      array(idx + 1) = (v >> 16).toByte
      array(idx + 2) = (v >> 8).toByte
      array(idx + 3) = v.toByte
    } else {
      array(idx) = v.toByte
      array(idx + 1) = (v >> 8).toByte
      array(idx + 2) = (v >> 16).toByte
      array(idx + 3) = (v >> 24).toByte
    }
  }

  def storeLong(index: Int, v: Long): Unit = {
    val idx = byteIndex(index)
    if (isBigEndian) {
      array(idx) = (v >> 56).toByte
      array(idx + 1) = (v >> 48).toByte
      array(idx + 2) = (v >> 40).toByte
      array(idx + 3) = (v >> 32).toByte
      array(idx + 4) = (v >> 24).toByte
      array(idx + 5) = (v >> 16).toByte
      array(idx + 6) = (v >> 8).toByte
      array(idx + 7) = v.toByte
    } else {
      array(idx) = v.toByte
      array(idx + 1) = (v >> 8).toByte
      array(idx + 2) = (v >> 16).toByte
      array(idx + 3) = (v >> 24).toByte
      array(idx + 4) = (v >> 32).toByte
      array(idx + 5) = (v >> 40).toByte
      array(idx + 6) = (v >> 48).toByte
      array(idx + 7) = (v >> 56).toByte
    }
  }

  def storeFloat(index: Int, v: Float): Unit =
    storeInt(index, java.lang.Float.floatToRawIntBits(v))

  def storeDouble(index: Int, v: Double): Unit =
    storeLong(index, java.lang.Double.doubleToRawLongBits(v))

  @inline
  private def byteIndex(index: Int): Int =
    indexMultiplier * index + arrayOffset
}
