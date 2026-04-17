package java.util.function

trait LongBinaryOperator:
  def applyAsLong(left: Long, right: Long): Long
