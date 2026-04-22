package java.util.function

trait IntUnaryOperator:
  def applyAsInt(operand: Int): Int

  def andThen(after: IntUnaryOperator): IntUnaryOperator =
    new IntUnaryOperator:
      def applyAsInt(i: Int): Int =
        after.applyAsInt(IntUnaryOperator.this.applyAsInt(i))

  def compose(before: IntUnaryOperator): IntUnaryOperator =
    new IntUnaryOperator:
      def applyAsInt(i: Int): Int =
        IntUnaryOperator.this.applyAsInt(before.applyAsInt(i))

object IntUnaryOperator:
  def identity(): IntUnaryOperator =
    new IntUnaryOperator:
      def applyAsInt(i: Int): Int = i
