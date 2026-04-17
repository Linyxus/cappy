package java.util.function

trait Function[T, R]:
  def apply(t: T): R

  def andThen[V](after: Function[_ >: R, _ <: V]): Function[T, V] =
    new Function[T, V]:
      def apply(t: T): V =
        after.apply(Function.this.apply(t))

  def compose[V](before: Function[_ >: V, _ <: T]): Function[V, R] =
    new Function[V, R]:
      def apply(v: V): R =
        Function.this.apply(before.apply(v))

object Function:
  def identity[T](): Function[T, T] =
    new Function[T, T]:
      def apply(t: T): T = t
