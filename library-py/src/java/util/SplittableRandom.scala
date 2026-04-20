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

import java.util.random.RandomGenerator

private object SplittableRandom {
  private final val GoldenGamma = 0x9e3779b97f4a7c15L

  private var defaultGen: Long = new Random().nextLong()

  private def nextDefaultGen(): Long = {
    val s = defaultGen
    defaultGen = s + (2 * GoldenGamma)
    s
  }

  private final def mix64ForGamma(z: Long): Long = {
    val z1 = (z ^ (z >>> 33)) * 0xff51afd7ed558ccdL
    val z2 = (z1 ^ (z1 >>> 33)) * 0xc4ceb9fe1a85ec53L
    z2 ^ (z2 >>> 33)
  }

  private final def mix32(z: Long): Int = {
    val z1 = (z ^ (z >>> 33)) * 0x62a9d9ed799705f5L
    val z2 = (z1 ^ (z1 >>> 28)) * 0xcb24d0a5c88c35b3L
    (z2 >>> 32).toInt
  }

  private final def mix64(z: Long): Long = {
    val z1 = (z ^ (z >>> 30)) * 0xbf58476d1ce4e5b9L
    val z2 = (z1 ^ (z1 >>> 27)) * 0x94d049bb133111ebL
    z2 ^ (z2 >>> 31)
  }

  private final def mixGamma(z: Long): Long = {
    val z1 = mix64ForGamma(z) | 1L
    val n = java.lang.Long.bitCount(z1 ^ (z1 >>> 1))
    if (n <= 24) z1 ^ 0xaaaaaaaaaaaaaaaaL
    else z1
  }
}

final class SplittableRandom private (private var seed: Long, private var gamma: Long) extends RandomGenerator {
  import SplittableRandom._

  def this(seed: Long) = this(seed, SplittableRandom.GoldenGamma)

  def this() = {
    this(0L, SplittableRandom.GoldenGamma)
    val s = SplittableRandom.nextDefaultGen()
    this.seed = SplittableRandom.mix64(s)
    this.gamma = SplittableRandom.mixGamma(s + SplittableRandom.GoldenGamma)
  }

  def split(): SplittableRandom =
    new SplittableRandom(mix64(nextSeed()), mixGamma(nextSeed()))

  private def nextSeed(): Long = {
    seed += gamma
    seed
  }

  override def nextInt(): Int =
    mix32(nextSeed())

  def nextLong(): Long =
    mix64(nextSeed())
}
