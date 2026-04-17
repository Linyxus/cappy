package java.util.function

import java.util.Comparator

trait BinaryOperator[T] extends BiFunction[T, T, T]

object BinaryOperator:
  def minBy[T](comparator: Comparator[_ >: T]): BinaryOperator[T] =
    new BinaryOperator[T]:
      def apply(a: T, b: T): T =
        if comparator.compare(a, b) <= 0 then a else b

  def maxBy[T](comparator: Comparator[_ >: T]): BinaryOperator[T] =
    new BinaryOperator[T]:
      def apply(a: T, b: T): T =
        if comparator.compare(a, b) >= 0 then a else b
