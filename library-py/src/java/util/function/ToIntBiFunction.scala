package java.util.function

trait ToIntBiFunction[T, U]:
  def applyAsInt(t: T, u: U): Int
