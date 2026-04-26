// Broad coverage of scala.collection.immutable.HashSet operations.
// Hash iteration order is implementation-defined: print only stable derived
// values (size, contains, sorted-toList, foldLeft, sum/min/max).

import scala.collection.immutable.HashSet

@main def scalaStdlibImmutableHashSet(): Unit =
  val s = HashSet(1, 2, 3, 4)

  // 1. construction variants (incl. dedup via .from)
  val empty = HashSet.empty[Int]
  val dedup = HashSet.from(List(1, 2, 3, 1, 2))
  println("ctor:" + empty.size + ":" + empty.isEmpty + ":" + dedup.size)

  // 2. accessors
  println("contains:" + s.contains(2) + ":" + s.contains(99))
  println("size:" + s.size + ":" + s.isEmpty)

  // 3. set algebra (sorted views for stable output).
  val other = HashSet(2, 3, 4, 5)
  val inter = s.intersect(other)
  println("intersect:" + inter.size + ":" + inter.toList.sorted.mkString(","))
  val uni = s.union(HashSet(4, 5, 6))
  println("union:" + uni.size + ":" + uni.toList.sorted.mkString(","))
  val df = s.diff(HashSet(1, 2))
  println("diff:" + df.size + ":" + df.toList.sorted.mkString(","))

  // 4. higher-order: filter / exists / forall / count
  println("filter:" + s.filter(_ > 1).size)
  println("exists:" + s.exists(_ > 3))
  println("forall:" + s.forall(_ > 0))
  println("count:" + s.count(_ > 2))

  // 5. partition (size of each side)
  val parts = s.partition(_ > 2)
  println("partition:" + parts._1.size + ":" + parts._2.size)

  // 6. folds / aggregates
  println("sum:" + s.sum)
  println("foldLeft:" + s.foldLeft(0)(_ + _))
  println("min:" + s.min + ":max:" + s.max)

  // 7. updated (incl. + / - / ++ / --)
  val plus = s + 99
  println("plus:" + plus.size + ":" + plus.contains(99))
  val minus = s - 1
  println("minus:" + minus.size + ":" + minus.contains(1))
  val plusMany = s ++ HashSet(10, 20)
  println("plusMany:" + plusMany.size + ":" + plusMany.contains(20))
  val minusMany = s -- List(1, 2)
  println("minusMany:" + minusMany.size + ":" + minusMany.contains(3))

  // 8. conversions
  println("toList:" + s.toList.sorted.mkString(","))
  println("toSeq:" + s.toSeq.size + ":iter:" + s.iterator.size)
