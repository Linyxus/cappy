// Coverage for scala.collection.immutable.ListMap.
// Iteration order is implementation-defined relative to insertion; we capture
// whatever order ListMap uses through `mkString` so the .check pins it.

import scala.collection.immutable.ListMap

@main def scalaStdlibImmutableListMap(): Unit =
  val m = ListMap("a" -> 1, "b" -> 2, "c" -> 3, "d" -> 4)

  // 1. build
  val empty = ListMap.empty[String, Int]
  val fromList = ListMap.from(List("x" -> 10, "y" -> 20, "z" -> 30))
  println("build:" + empty.size + ":" + empty.isEmpty + ":" + fromList.size)

  // 2. access — head / last / tail / size
  println("access:" + m.head._1 + "=" + m.head._2 + ":" + m.last._1 + "=" + m.last._2 + ":" + m.size + ":" + m.contains("b"))
  println("getOrElse:" + m.get("a").getOrElse(-1) + ":" + m.getOrElse("zz", -1))

  // 3. order — keys / values / iterator (deterministic for ListMap)
  println("order:" + m.iterator.toList.map((k, v) => k + "=" + v).mkString(","))
  println("keys:" + m.keys.toList.mkString(","))
  println("values:" + m.values.toList.mkString(","))

  // 4. transform — updated / removed / + / ++
  val upd = m.updated("e", 5)
  println("updated:" + upd.size + ":" + upd.get("e").getOrElse(-1))
  val rm = m.removed("a")
  println("removed:" + rm.size + ":" + rm.contains("a"))
  val merged = m ++ ListMap("f" -> 6, "g" -> 7)
  println("merged:" + merged.size + ":" + merged.get("f").getOrElse(-1))
  val tail = m.tail
  println("tail:" + tail.size)

  // 5. aggregate — foldLeft / values.sum / filter
  println("aggregate:" + m.foldLeft(0)((acc, kv) => acc + kv._2) + ":" + m.values.sum + ":" + m.filter((_, v) => v > 2).size)

  // 6. convert
  println("convert:" + m.toList.size + ":" + m.toMap.size + ":" + m.iterator.size)
