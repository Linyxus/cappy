package java.util

import java.io.Serializable
import java.util.function.*

private def requireNonNullValue[T](value: T): T =
  if value == null then throw new NullPointerException()
  value

trait Comparator[A] { self =>
  import Comparator.*

  def compare(o1: A, o2: A): Int

  def reversed(): Comparator[A] =
    new Comparator[A] with Serializable:
      def compare(o1: A, o2: A): Int =
        self.compare(o2, o1)

  @inline
  def thenComparing(other: Comparator[_ >: A]): Comparator[A] =
    requireNonNullValue(other)
    new Comparator[A] with Serializable:
      def compare(o1: A, o2: A): Int =
        val cmp = self.compare(o1, o2)
        if cmp != 0 then cmp else other.compare(o1, o2)

  def thenComparing[U](keyExtractor: Function[_ >: A, _ <: U], keyComparator: Comparator[_ >: U]): Comparator[A] =
    thenComparing(comparing[A, U](keyExtractor, keyComparator))

  def thenComparing[U <: Comparable[U]](keyExtractor: Function[_ >: A, _ <: U]): Comparator[A] =
    thenComparing(comparing[A, U](keyExtractor))

  def thenComparingInt(keyExtractor: ToIntFunction[_ >: A]): Comparator[A] =
    thenComparing(comparingInt(keyExtractor))

  def thenComparingLong(keyExtractor: ToLongFunction[_ >: A]): Comparator[A] =
    thenComparing(comparingLong(keyExtractor))

  def thenComparingDouble(keyExtractor: ToDoubleFunction[_ >: A]): Comparator[A] =
    thenComparing(comparingDouble(keyExtractor))
}

object Comparator:
  def reverseOrder[T <: Comparable[T]](): Comparator[T] =
    naturalOrder[T]().reversed()

  @inline
  def naturalOrder[T <: Comparable[T]](): Comparator[T] =
    ReusableNaturalComparator.asInstanceOf[Comparator[T]]

  private object ReusableNaturalComparator extends Comparator[Any]:
    def compare(o1: Any, o2: Any): Int =
      o1.asInstanceOf[Comparable[Any]].compareTo(o2)

  @inline
  def nullsFirst[T](comparator: Comparator[_ >: T]): Comparator[T] =
    new Comparator[T] with Serializable:
      def compare(o1: T, o2: T): Int =
        if o1 == null && o2 == null then 0
        else if o1 == null then -1
        else if o2 == null then 1
        else if comparator == null then 0
        else comparator.compare(o1, o2)

  @inline
  def nullsLast[T](comparator: Comparator[_ >: T]): Comparator[T] =
    new Comparator[T] with Serializable:
      def compare(o1: T, o2: T): Int =
        if o1 == null && o2 == null then 0
        else if o1 == null then 1
        else if o2 == null then -1
        else if comparator == null then 0
        else comparator.compare(o1, o2)

  @inline
  def comparing[T, U](keyExtractor: Function[_ >: T, _ <: U], keyComparator: Comparator[_ >: U]): Comparator[T] =
    requireNonNullValue(keyExtractor)
    requireNonNullValue(keyComparator)
    new Comparator[T] with Serializable:
      def compare(o1: T, o2: T): Int =
        keyComparator.compare(keyExtractor(o1), keyExtractor(o2))

  @inline
  def comparing[T, U <: Comparable[U]](keyExtractor: Function[_ >: T, _ <: U]): Comparator[T] =
    requireNonNullValue(keyExtractor)
    new Comparator[T] with Serializable:
      def compare(o1: T, o2: T): Int =
        keyExtractor(o1).compareTo(keyExtractor(o2))

  @inline
  def comparingInt[T](keyExtractor: ToIntFunction[_ >: T]): Comparator[T] =
    requireNonNullValue(keyExtractor)
    new Comparator[T] with Serializable:
      def compare(o1: T, o2: T): Int =
        compareInts(keyExtractor.applyAsInt(o1), keyExtractor.applyAsInt(o2))

  @inline
  def comparingLong[T](keyExtractor: ToLongFunction[_ >: T]): Comparator[T] =
    requireNonNullValue(keyExtractor)
    new Comparator[T] with Serializable:
      def compare(o1: T, o2: T): Int =
        compareLongs(keyExtractor.applyAsLong(o1), keyExtractor.applyAsLong(o2))

  @inline
  def comparingDouble[T](keyExtractor: ToDoubleFunction[_ >: T]): Comparator[T] =
    requireNonNullValue(keyExtractor)
    new Comparator[T] with Serializable:
      def compare(o1: T, o2: T): Int =
        compareDoubles(keyExtractor.applyAsDouble(o1), keyExtractor.applyAsDouble(o2))

  private def compareInts(left: Int, right: Int): Int =
    if left < right then -1 else if left > right then 1 else 0

  private def compareLongs(left: Long, right: Long): Int =
    if left < right then -1 else if left > right then 1 else 0

  private def compareDoubles(left: Double, right: Double): Int =
    if left < right then -1
    else if left > right then 1
    else if left == right then 0
    else if left != left then if right != right then 0 else 1
    else -1
