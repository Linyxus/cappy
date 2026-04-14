import scala.python.*

@extern("builtins", "len")
def pyLen(value: PyAny): Int = native

@extern("builtins", "print")
def pyPrint(value: Any): Unit = native

@extern("builtins", "list")
class ExternDefList extends PyAny:
  def append(value: String): Unit = native

@main def externDefBuiltins(): Unit =
  val xs = new ExternDefList()
  xs.append("x")
  xs.append("yz")
  pyPrint(pyLen(xs))
