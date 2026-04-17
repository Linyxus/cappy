package java.util.function

trait ObjIntConsumer[T]:
  def accept(t: T, value: Int): Unit
