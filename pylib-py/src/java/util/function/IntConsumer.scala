package java.util.function

trait IntConsumer:
  def accept(value: Int): Unit

  def andThen(after: IntConsumer): IntConsumer =
    new IntConsumer:
      def accept(value: Int): Unit =
        IntConsumer.this.accept(value)
        after.accept(value)
