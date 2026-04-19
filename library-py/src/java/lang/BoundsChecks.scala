/*
 * Port of scala-js javalib BoundsChecks, adapted for the ScalaPy backend.
 */

package java.lang

/** Utilities to perform bounds checks. */
private[java] object BoundsChecks {

  @inline
  def checkCapacity(capacity: Int): Unit = {
    if (capacity < 0)
      BoundsChecks.throwIllegalCapacityException(capacity)
  }

  @noinline
  private def throwIllegalCapacityException(capacity: Int): Nothing =
    throw new IllegalArgumentException(s"Illegal capacity: $capacity")

  @inline
  def checkIndex(index: Int, length: Int): Unit = {
    if (isIndexInvalid(index, length))
      BoundsChecks.throwIOOBE(index, length)
  }

  @noinline
  private def throwIOOBE(index: Int, length: Int): Nothing =
    throw new IndexOutOfBoundsException(s"Index $index out of bounds [0, $length)")

  @inline
  def isIndexInvalid(index: Int, length: Int): scala.Boolean =
    Integer.unsigned_>=(index, length)

  @inline
  def checkIndexInclusive(index: Int, length: Int): Unit = {
    if (isIndexInclusiveInvalid(index, length))
      BoundsChecks.throwInclusiveIOOBE(index, length)
  }

  @noinline
  private def throwInclusiveIOOBE(index: Int, length: Int): Nothing =
    throw new IndexOutOfBoundsException(s"Index $index out of bounds [0, $length]")

  @inline
  def isIndexInclusiveInvalid(index: Int, length: Int): scala.Boolean =
    Integer.unsigned_>(index, length)

  @inline
  def checkStartEnd(start: Int, end: Int, length: Int): Int = {
    val count = end - start
    if (isStartCountEndInvalid(start, count, end, length))
      BoundsChecks.throwStartEndOOBE(start, end, length)
    count
  }

  @noinline
  private def throwStartEndOOBE(start: Int, end: Int, length: Int): Nothing =
    throw new IndexOutOfBoundsException(s"Range [$start, $end) out of bounds [0, $length)")

  @inline
  def checkOffsetCount(offset: Int, count: Int, length: Int): Int = {
    val endOffset = offset + count
    if (isStartCountEndInvalid(offset, count, endOffset, length))
      BoundsChecks.throwOffsetCountOOBE(offset, count, length)
    endOffset
  }

  @noinline
  private def throwOffsetCountOOBE(offset: Int, count: Int, length: Int): Nothing = {
    throw new IndexOutOfBoundsException(
        s"Range [$offset, $offset + $count) out of bounds [0, $length)")
  }

  @inline
  def isStartCountEndInvalid(start: Int, count: Int, end: Int, length: Int): scala.Boolean =
    (start | count | end | (length - end)) < 0
}
