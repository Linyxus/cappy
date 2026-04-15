import scala.python.*

@extern("builtins")
object X extends PyAny:
  @name("print")
  def first(value: Any): Unit = native

  @name("print")
  def second(value: Any): Unit = native // error
