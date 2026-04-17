package java.util.function

trait DoublePredicate:
  def test(t: Double): Boolean

  def and(other: DoublePredicate): DoublePredicate =
    new DoublePredicate:
      def test(value: Double): Boolean =
        DoublePredicate.this.test(value) && other.test(value)

  def negate(): DoublePredicate =
    new DoublePredicate:
      def test(value: Double): Boolean =
        !DoublePredicate.this.test(value)

  def or(other: DoublePredicate): DoublePredicate =
    new DoublePredicate:
      def test(value: Double): Boolean =
        DoublePredicate.this.test(value) || other.test(value)
