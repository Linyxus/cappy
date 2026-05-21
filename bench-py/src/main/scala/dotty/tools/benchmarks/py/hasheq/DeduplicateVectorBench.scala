package dotty.tools.benchmarks.py.hasheq

/** Deduplicate a `Vector` of two-String records via `toSet` and `distinct`.
 *  Drives double `h * 31 + char` hashing + two-String `equals` per element. */
case class Tag(namespace: String, label: String)

class DeduplicateVectorBench:
  var size: Int = 0
  var tags: Vector[Tag] = Vector.empty

  def setup(size: Int): Unit =
    this.size = size
    val span = math.max(size / 2, 1)
    tags = (0 until size).map(i => Tag(s"ns${i % 8}", s"lbl${i % span}")).toVector

  val operations: Map[String, () => Any] = Map(
    "toHashSet"      -> (() => tags.toSet),
    "toDistinct"     -> (() => tags.distinct),
    "unionSelf"      -> (() => tags.toSet | tags.drop(size / 2).toSet),
    "sizeAfterDedup" -> (() => tags.toSet.size),
  )

@main def main(): Unit = ()
