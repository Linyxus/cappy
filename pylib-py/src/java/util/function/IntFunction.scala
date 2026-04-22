package java.util.function

trait IntFunction[R]:
  def apply(value: Int): R
