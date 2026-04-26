// Coverage for scala.collection.immutable.VectorMap — insertion-order map
// backed by a Vector for O(1) indexing.

import scala.collection.immutable.VectorMap

@main def scalaStdlibImmutableVectorMap(): Unit =
  val m = VectorMap("a" -> 1, "b" -> 2, "c" -> 3, "d" -> 4)

  // 1. build
  val empty = VectorMap.empty[String, Int]
  val fromList = VectorMap.from(List("x" -> 10, "y" -> 20, "z" -> 30))
  println("build:" + empty.size + ":" + empty.isEmpty + ":" + fromList.size)

  // 2. access — head / last / size / contains / get
  println("access:" + m.head._1 + "=" + m.head._2 + ":" + m.last._1 + "=" + m.last._2 + ":" + m.size + ":" + m.contains("b") + ":" + m.get("c").getOrElse(-1))

  // 3. order — keys / values / iterator (deterministic for VectorMap)
  println("order:" + m.iterator.toList.map((k, v) => k + "=" + v).mkString(","))
  println("keys:" + m.keys.mkString(","))
  println("values:" + m.values.toList.mkString(","))

  // 4. Vector indexing — keys is a Vector
  println("index:" + m.keys(0) + ":" + m.keys(2) + ":" + m.keys.last)

  // 5. transform — updated / removed / + / ++
  val upd = m.updated("e", 5)
  println("updated:" + upd.size + ":" + upd.last._1 + ":" + upd.get("e").getOrElse(-1))
  val rm = m.removed("a")
  println("removed:" + rm.size + ":" + rm.head._1 + ":" + rm.contains("a"))
  val tail = m.tail
  println("tail:" + tail.size + ":" + tail.head._1)
  val init = m.init
  println("init:" + init.size + ":" + init.last._1)

  // 6. aggregate
  println("aggregate:" + m.foldLeft(0)((acc, kv) => acc + kv._2) + ":" + m.values.sum + ":" + m.filter((_, v) => v > 2).size)

  // 7. convert
  println("convert:" + m.toList.size + ":" + m.toMap.size + ":" + m.iterator.size)
