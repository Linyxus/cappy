package java.util.function

trait IntPredicate:
  def test(t: Int): Boolean

  def and(other: IntPredicate): IntPredicate =
    new IntPredicate:
      def test(value: Int): Boolean =
        IntPredicate.this.test(value) && other.test(value)

  def negate(): IntPredicate =
    new IntPredicate:
      def test(value: Int): Boolean =
        !IntPredicate.this.test(value)

  def or(other: IntPredicate): IntPredicate =
    new IntPredicate:
      def test(value: Int): Boolean =
        IntPredicate.this.test(value) || other.test(value)
