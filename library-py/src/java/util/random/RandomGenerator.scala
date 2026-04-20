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

package java.util.random

import scala.annotation.tailrec

trait RandomGenerator {
  def nextLong(): Long

  def nextBoolean(): Boolean =
    nextInt() < 0

  def nextBytes(bytes: Array[Byte]): Unit = {
    var i = 0
    while (i < bytes.length) {
      var rnd = nextLong()
      var n = Math.min(bytes.length - i, 8)
      while (n > 0) {
        bytes(i) = rnd.toByte
        rnd >>>= 8
        n -= 1
        i += 1
      }
    }
  }

  def nextFloat(): Float = {
    val bits = nextInt() >>> 8
    bits.toFloat * (1.0f / (1 << 24))
  }

  def nextFloat(bound: Float): Float = {
    if (!(bound > 0.0f) || java.lang.Float.isInfinite(bound))
      throw new IllegalArgumentException(s"Illegal bound: $bound")
    val value = nextFloat() * bound
    if (value < bound) value else java.lang.Math.nextDown(bound)
  }

  def nextFloat(origin: Float, bound: Float): Float = {
    if (!(origin < bound) || java.lang.Float.isInfinite(bound) || java.lang.Float.isInfinite(origin))
      throw new IllegalArgumentException(s"Illegal bounds: [$origin, $bound)")
    val value = origin + (nextFloat() * (bound - origin))
    if (value < bound) value else java.lang.Math.nextDown(bound)
  }

  def nextDouble(): Double = {
    val bits = nextLong() >>> 11
    bits.toDouble * (1.0 / (1L << 53))
  }

  def nextDouble(bound: Double): Double = {
    if (!(bound > 0.0) || java.lang.Double.isInfinite(bound))
      throw new IllegalArgumentException(s"Illegal bound: $bound")
    val value = nextDouble() * bound
    if (value < bound) value else java.lang.Math.nextDown(bound)
  }

  def nextDouble(origin: Double, bound: Double): Double = {
    if (!(origin < bound) || java.lang.Double.isInfinite(bound) || java.lang.Double.isInfinite(origin))
      throw new IllegalArgumentException(s"Illegal bounds: [$origin, $bound)")
    val value = origin + (nextDouble() * (bound - origin))
    if (value < bound) value else java.lang.Math.nextDown(bound)
  }

  def nextInt(): Int =
    (nextLong() >>> 32).toInt

  def nextInt(bound: Int): Int = {
    if (bound <= 0)
      throw new IllegalArgumentException(s"Illegal bound: $bound")
    nextBoundedInt(bound)
  }

  def nextInt(origin: Int, bound: Int): Int = {
    if (bound <= origin)
      throw new IllegalArgumentException(s"Illegal bounds: [$origin, $bound)")
    val difference = bound - origin
    if (difference > 0)
      origin + nextBoundedInt(difference)
    else {
      var result = nextInt()
      while (result < origin || result >= bound)
        result = nextInt()
      result
    }
  }

  def nextLong(bound: Long): Long = {
    if (bound <= 0L)
      throw new IllegalArgumentException(s"Illegal bound: $bound")
    nextBoundedLong(bound)
  }

  def nextLong(origin: Long, bound: Long): Long = {
    if (bound <= origin)
      throw new IllegalArgumentException(s"Illegal bounds: [$origin, $bound)")
    val difference = bound - origin
    if (difference > 0L)
      origin + nextBoundedLong(difference)
    else {
      var result = nextLong()
      while (result < origin || result >= bound)
        result = nextLong()
      result
    }
  }

  private def nextBoundedInt(bound: Int): Int = {
    if ((bound & -bound) == bound) {
      nextInt() & (bound - 1)
    } else {
      @tailrec
      def loop(): Int = {
        val bits = nextInt() >>> 1
        val value = bits % bound
        if (bits - value + (bound - 1) < 0) loop()
        else value
      }
      loop()
    }
  }

  private def nextBoundedLong(bound: Long): Long = {
    if ((bound & -bound) == bound) {
      nextLong() & (bound - 1L)
    } else {
      @tailrec
      def loop(): Long = {
        val bits = nextLong() >>> 1
        val value = java.lang.Long.remainderUnsigned(bits, bound)
        if (bits - value + (bound - 1L) < 0L) loop()
        else value
      }
      loop()
    }
  }
}
