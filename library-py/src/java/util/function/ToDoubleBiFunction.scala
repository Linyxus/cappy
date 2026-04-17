package java.util.function

trait ToDoubleBiFunction[T, U]:
  def applyAsDouble(t: T, u: U): Double
