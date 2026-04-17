package java.util.function

trait ToLongFunction[T]:
  def applyAsLong(t: T): Long
