package java.util.function

trait DoubleUnaryOperator:
  def applyAsDouble(operand: Double): Double

  def andThen(after: DoubleUnaryOperator): DoubleUnaryOperator =
    new DoubleUnaryOperator:
      def applyAsDouble(d: Double): Double =
        after.applyAsDouble(DoubleUnaryOperator.this.applyAsDouble(d))

  def compose(before: DoubleUnaryOperator): DoubleUnaryOperator =
    new DoubleUnaryOperator:
      def applyAsDouble(d: Double): Double =
        DoubleUnaryOperator.this.applyAsDouble(before.applyAsDouble(d))

object DoubleUnaryOperator:
  def identity(): DoubleUnaryOperator =
    new DoubleUnaryOperator:
      def applyAsDouble(d: Double): Double = d
