// Broader operation coverage for mutable.HashSet[Int].

import scala.collection.mutable.HashSet

@main def scalaStdlibMutableHashSet(): Unit =
  val s = HashSet(1, 2, 3)

  // 1. Construction varargs + size + isEmpty
  println("size:" + s.size + ":" + s.isEmpty)

  // 2. HashSet.empty
  val e = HashSet.empty[Int]
  println("empty:" + e.size + ":" + e.isEmpty)

  // 3. HashSet.from(List) with duplicates -> dedup
  val fromList = HashSet.from(List(1, 2, 3, 1))
  println("fromList:" + fromList.size + ":" + fromList.contains(1))

  // 4. contains
  println("contains:" + s.contains(1) + ":" + s.contains(99))

  // 5. set algebra: union / intersect / diff -- print stable derived values
  val uni = s.union(HashSet(4, 5))
  println("union:" + uni.size + ":" + uni.toList.sorted.mkString(","))
  val inter = s.intersect(HashSet(2, 3, 4))
  println("intersect:" + inter.size + ":" + inter.toList.sorted.mkString(","))
  val df = s.diff(HashSet(1, 2))
  println("diff:" + df.size + ":" + df.toList.sorted.mkString(","))

  // 6. higher-order: filter / exists / forall / count
  println("filter:" + s.filter(_ > 1).size)
  println("exists:" + s.exists(_ > 2))
  println("forall:" + s.forall(_ > 0))
  println("count:" + s.count(_ > 1))

  // 7. partition (returns (matching, non-matching))
  val (yes, no) = s.partition(_ > 1)
  println("partition:" + yes.size + ":" + no.size)

  // 8. folds / sum / min / max
  println("sum:" + s.sum)
  println("foldLeft:" + s.foldLeft(0)(_ + _))
  println("min:" + s.min + ":max:" + s.max)

  // 9. += single
  s += 99
  println("plus:" + s.size + ":" + s.contains(99))

  // 10. -= single
  s -= 1
  println("minus:" + s.size + ":" + s.contains(1))

  // 11. ++= bulk
  s ++= HashSet(10, 20)
  println("plusplus:" + s.size + ":" + s.contains(10) + ":" + s.contains(20))

  // 12. clone + clear
  val cloned = s.clone
  println("clone:" + cloned.size)
  s.clear()
  println("clear:" + s.size + ":" + cloned.size)

  // 13. conversions
  println("toList:" + cloned.toList.sorted.mkString(","))
  println("iter:" + cloned.iterator.size)
