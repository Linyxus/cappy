package java.util.function

trait BiFunction[T, U, R]:
  def apply(t: T, u: U): R

  def andThen[V](after: Function[_ >: R, _ <: V]): BiFunction[T, U, V] =
    new BiFunction[T, U, V]:
      def apply(t: T, u: U): V =
        after.apply(BiFunction.this.apply(t, u))
