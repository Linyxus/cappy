// Coverage for scala.collection.mutable.LinkedHashMap — insertion-order map.

import scala.collection.mutable.LinkedHashMap

@main def scalaStdlibMutableLinkedHashMap(): Unit =
  val m = LinkedHashMap("a" -> 1, "b" -> 2, "c" -> 3, "d" -> 4)

  // 1. build
  val empty = LinkedHashMap.empty[String, Int]
  val fromList = LinkedHashMap.from(List("x" -> 10, "y" -> 20, "z" -> 30))
  println("build:" + empty.size + ":" + empty.isEmpty + ":" + fromList.size)

  // 2. access — head / last / contains / get
  println("access:" + m.head._1 + "=" + m.head._2 + ":" + m.last._1 + "=" + m.last._2 + ":" + m.size + ":" + m.contains("c"))
  println("getOrElse:" + m.get("a").getOrElse(-1) + ":" + m.getOrElse("zz", -1) + ":" + m("b"))

  // 3. order — iterator / keys / values (deterministic for LinkedHashMap)
  println("order:" + m.iterator.toList.map((k, v) => k + "=" + v).mkString(","))
  println("keys:" + m.keys.toList.mkString(","))
  println("values:" + m.values.toList.mkString(","))

  // 4. transform — filter / count / exists (read-only HOFs)
  println("transform:" + m.filter((_, v) => v > 1).size + ":" + m.count((_, v) => v > 2) + ":" + m.exists((_, v) => v == 4))

  // 5. mutate — update / += / -= / ++=
  m.update("e", 5)
  println("mutate1:" + m.size + ":" + m.last._1 + ":" + m.get("e").getOrElse(-1))
  m += ("f" -> 6)
  println("mutate2:" + m.size + ":" + m.last._1)
  m -= "a"
  println("mutate3:" + m.size + ":" + m.head._1 + ":" + m.contains("a"))
  m ++= LinkedHashMap("g" -> 7, "h" -> 8)
  println("mutate4:" + m.size + ":" + m.last._1)

  // 6. aggregate
  println("aggregate:" + m.foldLeft(0)((acc, kv) => acc + kv._2) + ":" + m.values.sum)

  // 7. convert
  println("convert:" + m.toList.size + ":" + m.toMap.size + ":" + m.iterator.size)

  // 8. clone + clear
  val cloned = m.clone()
  println("clone:" + cloned.size)
  m.clear()
  println("clear:" + m.size + ":" + cloned.size)
