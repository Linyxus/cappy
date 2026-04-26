// Broader operation coverage for mutable.HashMap[String, Int].

import scala.collection.mutable.HashMap

@main def scalaStdlibMutableHashMap(): Unit =
  val m = HashMap("a" -> 1, "b" -> 2, "c" -> 3)

  // 1. Construction via varargs + size + isEmpty/nonEmpty
  println("size:" + m.size + ":" + m.isEmpty + ":" + m.nonEmpty)

  // 2. HashMap.empty
  val e = HashMap.empty[String, Int]
  println("empty:" + e.size + ":" + e.isEmpty)

  // 3. HashMap.from(List)
  val fromList = HashMap.from(List("x" -> 10, "y" -> 20))
  println("fromList:" + fromList.size + ":" + fromList.get("x") + ":" + fromList.get("y"))

  // 4. apply / get / getOrElse / contains
  println("apply:" + m("a") + ":" + m.get("b") + ":" + m.getOrElse("z", -1) + ":" + m.contains("c"))

  // 5. keys / values stable through sort
  println("keys:" + m.keys.toList.sorted.mkString(","))
  println("values:" + m.values.toList.sorted.mkString(","))

  // 6. foreach driven counter (deterministic — sums all values)
  var s = 0
  m.foreach((_, v) => s += v)
  println("foreach-sum:" + s)

  // 7. higher-order: filter / exists / count / find
  println("filter:" + m.filter((_, v) => v > 1).size)
  println("exists:" + m.exists((_, v) => v == 1))
  println("count:" + m.count((_, v) => v > 0))
  // find returns Option[(K,V)] — keep deterministic by checking key only
  println("find:" + m.find((_, v) => v == 2).map(_._1))

  // 8. foldLeft over values (deterministic since we sum)
  println("foldLeft:" + m.foldLeft(0)((acc, kv) => acc + kv._2))
  println("values-sum:" + m.values.sum)

  // 9. mutation: update via .update
  m.update("d", 4)
  println("update:" + m.size + ":" + m.get("d"))

  // 10. -= removal
  m -= "a"
  println("minus:" + m.size + ":" + m.contains("a"))

  // 11. ++= bulk add
  val other = HashMap("e" -> 5, "f" -> 6)
  m ++= other
  println("plusplus:" + m.size + ":" + m.get("e") + ":" + m.get("f"))

  // 12. clone + clear
  val cloned = m.clone
  println("clone:" + cloned.size)
  m.clear()
  println("clear:" + m.size + ":" + cloned.size)

  // 13. conversions
  println("toList:" + cloned.toList.size)
  println("toMap:" + cloned.toMap.size)
  println("iter:" + cloned.iterator.size)
