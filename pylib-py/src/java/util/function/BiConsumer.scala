package java.util.function

trait BiConsumer[T, U]:
  def accept(t: T, u: U): Unit

  def andThen(after: BiConsumer[T, U]): BiConsumer[T, U] =
    new BiConsumer[T, U]:
      def accept(t: T, u: U): Unit =
        BiConsumer.this.accept(t, u)
        after.accept(t, u)
