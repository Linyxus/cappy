// Broad coverage of scala.collection.immutable.HashMap operations.
// Hash iteration order is implementation-defined: print only stable derived
// values (size, contains, get, sorted-toList, foldLeft, .values.sum).

import scala.collection.immutable.HashMap

@main def scalaStdlibImmutableHashMap(): Unit =
  val m = HashMap(1 -> 10, 2 -> 20, 3 -> 30, 4 -> 40)

  // 1. construction variants
  val empty = HashMap.empty[Int, Int]
  val fromList = HashMap.from(List(1 -> 10, 2 -> 20, 3 -> 30))
  println("ctor:" + empty.size + ":" + empty.isEmpty + ":" + fromList.size)

  // 2. accessors: get / contains / getOrElse
  println("get:" + m.get(2) + ":" + m.get(99))
  println("contains:" + m.contains(1) + ":" + m.contains(99))
  println("getOrElse:" + m.getOrElse(2, -1) + ":" + m.getOrElse(99, -1))

  // 3. direct apply — exercises the specialized `apply_mcII_sp__I__I` path.
  println("apply:" + m(2))

  // 4. sorted keys / values / entries
  println("keys:" + m.keys.toList.sorted.mkString(","))
  println("values:" + m.values.toList.sorted.mkString(","))
  println("entries:" + m.toList.sortBy(_._1).mkString(","))

  // 5. higher-order: filter / exists / count
  println("filter:" + m.filter((_, v) => v > 15).size)
  println("exists:" + m.exists((_, v) => v == 20))
  println("count:" + m.count((_, v) => v > 15))

  // 6. partition (size of each side)
  val parts = m.partition((_, v) => v > 15)
  println("partition:" + parts._1.size + ":" + parts._2.size)

  // 7. updated / removed / merge
  val u = m.updated(5, 50)
  println("updated:" + u.size + ":" + u.get(5))
  val r = m.removed(1)
  println("removed:" + r.size + ":" + r.contains(1))
  val merged = m ++ HashMap(5 -> 50, 6 -> 60)
  println("merge:" + merged.size + ":" + merged.get(6))

  // 8. mass remove
  val mm = m -- List(1, 2)
  println("massRemove:" + mm.size + ":" + mm.contains(1) + ":" + mm.contains(3))

  // 9. folds
  println("foldLeft:" + m.foldLeft(0)((acc, kv) => acc + kv._2))
  println("valuesSum:" + m.values.sum)

  // 10. conversions
  println("toList:" + m.toList.size + ":" + m.toMap.size + ":" + m.toSeq.size)
  println("keySet:" + m.keySet.size + ":" + m.keySet.contains(3))
