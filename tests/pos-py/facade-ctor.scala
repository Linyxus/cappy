import scala.python.*

@extern("builtins", "range")
class PyRange(start: Int, stop: Int, step: Int) extends PyAny

@extern("builtins", "list")
class PyList(iterable: PyAny) extends PyAny

@extern("builtins", "len")
def pyLen(x: PyAny): Int = native

@extern("builtins", "sum")
def pySum(x: PyAny): Int = native

@extern("builtins", "print")
def pyPrint(x: Any, y: Any): Unit = native

@main def facadeCtor(): Unit =
  val r = new PyRange(0, 10, 2)      // Python: range(0, 10, 2)
  val xs = new PyList(r)             // Python: list(r) = [0, 2, 4, 6, 8]
  pyPrint("len=", pyLen(xs))         // len= 5
  pyPrint("sum=", pySum(xs))         // sum= 20
