package java.util.function

trait Consumer[T]:
  def accept(t: T): Unit

  def andThen(after: Consumer[_ >: T]): Consumer[T] =
    new Consumer[T]:
      def accept(t: T): Unit =
        Consumer.this.accept(t)
        after.accept(t)
