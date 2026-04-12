package dotty.tools.backend.python.ir.pyir

/** Source position attached to every PyIR tree node.
 *
 *  Lightweight: source path + line + column. A sentinel `NoPosition`
 *  lives in the companion object for missing/synthetic positions.
 */
final case class PyPosition(source: String, line: Int, column: Int):
  def isDefined: Boolean = source.nonEmpty
  def isEmpty: Boolean = !isDefined
  def show: String = s"$line:$column"

object PyPosition:
  /** Sentinel for missing positions. */
  val NoPosition: PyPosition = PyPosition("", 0, 0)
