package java.util.function

trait DoubleConsumer:
  def accept(value: Double): Unit

  def andThen(after: DoubleConsumer): DoubleConsumer =
    new DoubleConsumer:
      def accept(value: Double): Unit =
        DoubleConsumer.this.accept(value)
        after.accept(value)
