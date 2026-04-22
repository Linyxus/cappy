package java.util.function

trait LongFunction[R]:
  def apply(value: Long): R
