import scala.python.*

@extern("numpy")
object np extends PyAny:
  @name("array")
  def fromIterable(values: PyAny): NDArray = native
  def arange(stop: Int): NDArray = native
  @name("sum")
  def total(values: NDArray): Int = native

@extern("numpy", "ndarray")
class NDArray extends PyAny:
  def reshape(rows: Int, cols: Int): NDArray = native
  @name("tolist")
  def toScalaView(): PyAny = native
  val shape: PyAny = native

@extern("builtins", "range")
class PyRange(start: Int, stop: Int, step: Int) extends PyAny

@extern("builtins", "list")
class PyList(iterable: PyAny) extends PyAny

@main def helloNp(): Unit =
  val ints = np.fromIterable(new PyList(new PyRange(1, 5, 1)))
  println(ints.toScalaView())
  println(np.total(ints))

  val grid = np.arange(6).reshape(2, 3)
  println(grid.shape)
  println(grid.toScalaView())
