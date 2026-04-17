package java.util.function

trait DoubleFunction[R]:
  def apply(value: Double): R
