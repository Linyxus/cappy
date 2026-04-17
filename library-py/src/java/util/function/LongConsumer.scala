package java.util.function

trait LongConsumer:
  def accept(value: Long): Unit

  def andThen(after: LongConsumer): LongConsumer =
    new LongConsumer:
      def accept(value: Long): Unit =
        LongConsumer.this.accept(value)
        after.accept(value)
