import scala.python.*

@extern("builtins")
object pybuiltins extends PyAny:
  def print(value: Any): Unit = native
  @name("len")
  def lengthOf(value: PyAny): Int = native

@extern("math")
object pymath extends PyAny:
  val pi: Double = native

@extern("builtins", "list")
class PyList extends PyAny:
  def append(value: String): Unit = native

@main def facadeBuiltins(): Unit =
  pybuiltins.print(pymath.pi > 3.0)

  val xs = new PyList()
  xs.append("a")
  xs.append("bb")
  pybuiltins.print(pybuiltins.lengthOf(xs))
