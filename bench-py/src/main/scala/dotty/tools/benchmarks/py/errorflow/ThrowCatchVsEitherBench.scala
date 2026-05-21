package dotty.tools.benchmarks.py.errorflow

class DivError(msg: String) extends RuntimeException(msg)

/** Isolates Python exception throw/catch cost against wrapper-allocation cost:
 *  safe-divide implemented via try/catch vs Option vs Either, plus a
 *  user-defined exception throw/catch variant. */
class ThrowCatchVsEitherBench:
  var size: Int = 0

  def setup(size: Int): Unit =
    this.size = size

  val operations: Map[String, () => Any] = Map(
    "throwCatch" -> (() =>
      var acc = 0
      var i = 0
      while i < size do
        acc += (try 100 / (i % 7) catch case _: ArithmeticException => 0)
        i += 1
      acc
    ),
    "optionDiv" -> (() =>
      var acc = 0
      var i = 0
      while i < size do
        acc += (if i % 7 == 0 then None else Some(100 / (i % 7))).getOrElse(0)
        i += 1
      acc
    ),
    "eitherDiv" -> (() =>
      var acc = 0
      var i = 0
      while i < size do
        val v: Either[String, Int] = if i % 7 == 0 then Left("zero") else Right(100 / (i % 7))
        acc += v.getOrElse(0)
        i += 1
      acc
    ),
    "customThrow" -> (() =>
      var acc = 0
      var i = 0
      while i < size do
        acc += (try
          if i % 7 == 0 then throw new DivError("bad")
          100 / (i % 7)
        catch case _: DivError => 0)
        i += 1
      acc
    ),
  )

@main def main(): Unit = ()
