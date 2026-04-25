import scala.collection.immutable.HashMap

@main def scalaHashmapFrom(): Unit =
  // HashMap.from(IterableOnce) goes through MapNode$ initialization,
  // which forces ClassTag.cache$ -> ClassValueCompat -> WeakReference.
  // Avoid Range.map / specialized Tuple2 to keep this targeted.
  val pairs = List(("a", 1), ("b", 2), ("c", 3), ("d", 4))
  val m = HashMap.from(pairs)
  println("size:" + m.size)
  println("a:" + m.get("a"))
  println("z:" + m.get("z"))

  val empty = HashMap.from[String, Int](Nil)
  println("empty:" + empty.isEmpty + ":" + empty.size)
