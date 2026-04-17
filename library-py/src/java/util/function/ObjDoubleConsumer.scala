package java.util.function

trait ObjDoubleConsumer[T]:
  def accept(t: T, value: Double): Unit
