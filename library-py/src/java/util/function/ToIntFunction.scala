package java.util.function

trait ToIntFunction[T]:
  def applyAsInt(t: T): Int
