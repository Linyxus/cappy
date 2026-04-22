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

final class UUID(private val mostSigBits: Long, private val leastSigBits: Long)
    extends AnyRef with java.io.Serializable with Comparable[UUID] {

  import UUID._

  private def paddedHex8(x: Long, offset: Int): String = {
    val s = Integer.toHexString((x >>> offset).toInt)
    "00000000".substring(s.length) + s
  }

  private def paddedHex4(x: Long, offset: Int): String = {
    val s = Integer.toHexString((x >>> offset).toInt & 0xffff)
    "0000".substring(s.length) + s
  }

  def getLeastSignificantBits(): Long =
    leastSigBits

  def getMostSignificantBits(): Long =
    mostSigBits

  def version(): Int =
    (mostSigBits.toInt & 0xf000) >> 12

  def variant(): Int = {
    val i3 = (leastSigBits >>> 32).toInt
    if ((i3 & 0x80000000) == 0) 0
    else if ((i3 & 0x40000000) != 0) (i3 & 0xe0000000) >>> 29
    else 2
  }

  def timestamp(): Long = {
    if (version() != TimeBased)
      throw new UnsupportedOperationException("Not a time-based UUID")
    val lo = mostSigBits.toInt
    val resHi = (lo >>> 16) | ((lo & 0x0fff) << 16)
    (resHi.toLong << 32) | (mostSigBits >>> 32)
  }

  def clockSequence(): Int = {
    if (version() != TimeBased)
      throw new UnsupportedOperationException("Not a time-based UUID")
    (leastSigBits >>> 48).toInt & 0x3fff
  }

  def node(): Long = {
    if (version() != TimeBased)
      throw new UnsupportedOperationException("Not a time-based UUID")
    leastSigBits & 0x0000ffffffffffffL
  }

  override def toString(): String = {
    paddedHex8(mostSigBits, 32) + "-" + paddedHex4(mostSigBits, 16) + "-" +
      paddedHex4(mostSigBits, 0) + "-" + paddedHex4(leastSigBits, 48) + "-" +
      paddedHex4(leastSigBits, 32) + paddedHex8(leastSigBits, 0)
  }

  override def hashCode(): Int =
    java.lang.Long.hashCode(mostSigBits) ^ java.lang.Long.hashCode(leastSigBits)

  override def equals(that: Any): Boolean =
    if (that == null) false
    else {
      val other = that.asInstanceOf[UUID]
      mostSigBits == other.getMostSignificantBits() &&
      leastSigBits == other.getLeastSignificantBits()
    }

  def compareTo(that: UUID): Int = {
    val thisHi = this.getMostSignificantBits()
    val thatHi = that.getMostSignificantBits()
    if (thisHi != thatHi) {
      if (thisHi < thatHi) -1 else 1
    } else {
      val thisLo = this.getLeastSignificantBits()
      val thatLo = that.getLeastSignificantBits()
      if (thisLo != thatLo) {
        if (thisLo < thatLo) -1 else 1
      } else 0
    }
  }
}

object UUID {
  private final val TimeBased = 1

  def randomUUID(): UUID = {
    val random = new Random()
    val most = (random.nextLong() & ~0x000000000000f000L) | 0x0000000000004000L
    val least = (random.nextLong() & ~0xc000000000000000L) | 0x8000000000000000L
    new UUID(most, least)
  }

  def fromString(name: String): UUID = {
    import Integer.parseInt

    def fail(): Nothing =
      throw new IllegalArgumentException("Invalid UUID string: " + name)

    def parseHex8(his: String, los: String): Int =
      (parseInt(his, 16) << 16) | parseInt(los, 16)

    if (name.length != 36 || name.charAt(8) != '-' ||
        name.charAt(13) != '-' || name.charAt(18) != '-' || name.charAt(23) != '-') {
      fail()
    }

    try {
      val i1 = parseHex8(name.substring(0, 4), name.substring(4, 8))
      val i2 = parseHex8(name.substring(9, 13), name.substring(14, 18))
      val i3 = parseHex8(name.substring(19, 23), name.substring(24, 28))
      val i4 = parseHex8(name.substring(28, 32), name.substring(32, 36))
      val most = (i1.toLong << 32) | Integer.toUnsignedLong(i2)
      val least = (i3.toLong << 32) | Integer.toUnsignedLong(i4)
      new UUID(most, least)
    } catch {
      case _: NumberFormatException => fail()
    }
  }
}
