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

import java.util.random.RandomGenerator

class Random(seedIn: Long) extends AnyRef with RandomGenerator with java.io.Serializable {
  private var seed: Long = 0L
  private var nextNextGaussian: Double = 0.0
  private var haveNextNextGaussian: Boolean = false

  setSeed(seedIn)

  def this() = this(Random.randomSeed())

  def setSeed(seedIn: Long): Unit = {
    seed = (seedIn ^ 0x5deece66dL) & ((1L << 48) - 1)
    haveNextNextGaussian = false
  }

  protected def next(bits: Int): Int = {
    val newSeed = (seed * 0x5deece66dL + 0xbL) & ((1L << 48) - 1)
    seed = newSeed
    (newSeed >>> (48 - bits)).toInt
  }

  override def nextDouble(): Double =
    ((next(26).toDouble * (1L << 27).toDouble) + next(27).toDouble) / (1L << 53).toDouble

  override def nextBoolean(): Boolean =
    next(1) != 0

  override def nextInt(): Int =
    next(32)

  override def nextInt(n: Int): Int = {
    if (n <= 0)
      throw new IllegalArgumentException("n must be positive")
    else if ((n & -n) == n)
      next(31) >> Integer.numberOfLeadingZeros(n)
    else {
      @tailrec
      def loop(): Int = {
        val bits = next(31)
        val value = bits % n
        if (bits - value + (n - 1) < 0) loop()
        else value
      }
      loop()
    }
  }

  def nextLong(): Long =
    (next(32).toLong << 32) + next(32)

  override def nextFloat(): Float =
    (next(24).toDouble / (1 << 24).toDouble).toFloat

  override def nextBytes(bytes: Array[Byte]): Unit = {
    var i = 0
    while (i < bytes.length) {
      var rnd = nextInt()
      var n = Math.min(bytes.length - i, 4)
      while (n > 0) {
        bytes(i) = rnd.toByte
        rnd >>= 8
        n -= 1
        i += 1
      }
    }
  }

  def nextGaussian(): Double = {
    if (haveNextNextGaussian) {
      haveNextNextGaussian = false
      nextNextGaussian
    } else {
      var x = 0.0
      var y = 0.0
      var rds = 0.0

      var needsSample = true
      while (needsSample) {
        x = nextDouble() * 2.0 - 1.0
        y = nextDouble() * 2.0 - 1.0
        rds = x * x + y * y
        needsSample = rds == 0.0 || rds > 1.0
      }

      val c = Math.sqrt(-2.0 * Math.log(rds) / rds)
      nextNextGaussian = y * c
      haveNextNextGaussian = true
      x * c
    }
  }
}

object Random {
  private var seedUniquifier = 8682522807148012L

  private def randomSeed(): Long =
    val seed = seedUniquifier
    seedUniquifier = seed + 0x9e3779b97f4a7c15L
    seed
}
