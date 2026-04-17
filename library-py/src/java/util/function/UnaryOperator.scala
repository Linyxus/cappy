package java.util.function

trait UnaryOperator[T] extends Function[T, T]

object UnaryOperator:
  def identity[T](): UnaryOperator[T] =
    new UnaryOperator[T]:
      def apply(t: T): T = t
