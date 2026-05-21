package dotty.tools.benchmarks.py.strings

import scala.collection.mutable.StringBuilder

/** String construction patterns: a mutable `StringBuilder`, quadratic
 *  s-interpolation accumulation, and a one-shot `mkString`. */
class StringBuildBench:
  var size: Int = 0
  var parts: Vector[Int] = Vector.empty

  def setup(size: Int): Unit =
    this.size = size
    parts = (0 until size).toVector

  val operations: Map[String, () => Any] = Map(
    "builder" -> { () =>
      val sb = new StringBuilder(size)
      var i = 0
      while i < size do
        sb.append('a')
        i += 1
      sb.toString
    },
    "interpolate" -> { () =>
      var s = ""
      var i = 0
      while i < size do
        s = s"$s[$i]"
        i += 1
      s.length
    },
    "mkString" -> (() => parts.mkString(",")),
  )

@main def main(): Unit = ()
