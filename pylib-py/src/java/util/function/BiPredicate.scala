package java.util.function

trait BiPredicate[T, U]:
  def test(t: T, u: U): Boolean

  def and(other: BiPredicate[_ >: T, _ >: U]): BiPredicate[T, U] =
    new BiPredicate[T, U]:
      def test(t: T, u: U): Boolean =
        BiPredicate.this.test(t, u) && other.test(t, u)

  def negate(): BiPredicate[T, U] =
    new BiPredicate[T, U]:
      def test(t: T, u: U): Boolean =
        !BiPredicate.this.test(t, u)

  def or(other: BiPredicate[_ >: T, _ >: U]): BiPredicate[T, U] =
    new BiPredicate[T, U]:
      def test(t: T, u: U): Boolean =
        BiPredicate.this.test(t, u) || other.test(t, u)
