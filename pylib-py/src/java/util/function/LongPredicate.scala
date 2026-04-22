package java.util.function

trait LongPredicate:
  def test(t: Long): Boolean

  def and(other: LongPredicate): LongPredicate =
    new LongPredicate:
      def test(value: Long): Boolean =
        LongPredicate.this.test(value) && other.test(value)

  def negate(): LongPredicate =
    new LongPredicate:
      def test(value: Long): Boolean =
        !LongPredicate.this.test(value)

  def or(other: LongPredicate): LongPredicate =
    new LongPredicate:
      def test(value: Long): Boolean =
        LongPredicate.this.test(value) || other.test(value)
