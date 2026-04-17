package java.util.function

trait ObjLongConsumer[T]:
  def accept(t: T, value: Long): Unit
