package java.util.function

trait ToDoubleFunction[T]:
  def applyAsDouble(t: T): Double
