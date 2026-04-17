package java.util.function

trait LongUnaryOperator:
  def applyAsLong(operand: Long): Long

  def andThen(after: LongUnaryOperator): LongUnaryOperator =
    new LongUnaryOperator:
      def applyAsLong(l: Long): Long =
        after.applyAsLong(LongUnaryOperator.this.applyAsLong(l))

  def compose(before: LongUnaryOperator): LongUnaryOperator =
    new LongUnaryOperator:
      def applyAsLong(l: Long): Long =
        LongUnaryOperator.this.applyAsLong(before.applyAsLong(l))

object LongUnaryOperator:
  def identity(): LongUnaryOperator =
    new LongUnaryOperator:
      def applyAsLong(l: Long): Long = l
