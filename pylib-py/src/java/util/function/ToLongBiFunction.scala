package java.util.function

trait ToLongBiFunction[T, U]:
  def applyAsLong(t: T, u: U): Long
