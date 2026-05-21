package dotty.tools.benchmarks.py.hasheq

/** Case-class `.hashCode` and structural `==` in tight loops. Exercises the
 *  synthesized `caseHashCodeBody` murmur chains and multi-field equality. */
case class Point(x: Int, y: Int)

class CaseClassHashBench:
  var size: Int = 0
  var points: Vector[Point] = Vector.empty

  def setup(size: Int): Unit =
    this.size = size
    points = (0 until size).map(i => Point(i, i * 31 + 7)).toVector

  val operations: Map[String, () => Any] = Map(
    "sumHashCodes" -> (() => {
      var s = 0
      var i = 0
      while i < points.size do
        s += points(i).hashCode
        i += 1
      s
    }),
    "countEqual" -> (() => {
      var n = 0
      var i = 0
      while i < points.size do
        if points(i) == Point(i, i * 31 + 7) then n += 1
        i += 1
      n
    }),
    "countDistinctHash" -> (() => {
      val s = scala.collection.mutable.HashSet.empty[Int]
      var i = 0
      while i < points.size do
        s += points(i).hashCode
        i += 1
      s.size
    }),
  )

@main def main(): Unit = ()
