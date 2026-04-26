// Coverage for scala.collection.immutable.TreeSet — sorted set, deterministic iteration.

import scala.collection.immutable.TreeSet

@main def scalaStdlibImmutableTreeSet(): Unit =
  val s = TreeSet(5, 1, 3, 2, 4)

  // 1. build
  val empty = TreeSet.empty[Int]
  val fromList = TreeSet.from(List(3, 1, 2, 1, 3))
  println("build:" + empty.size + ":" + empty.isEmpty + ":" + fromList.size)

  // 2. access — head / last / min / max / contains
  println("access:" + s.head + ":" + s.last + ":" + s.min + ":" + s.max + ":" + s.contains(3) + ":" + s.contains(99))

  // 3. order — sorted iteration
  println("order:" + s.iterator.toList.mkString(","))

  // 4. transform — incl / excl / + / - / ++ / --
  val plus = s + 99
  println("plus:" + plus.size + ":" + plus.last)
  val minus = s - 1
  println("minus:" + minus.size + ":" + minus.head)
  val plusMany = s ++ TreeSet(10, 20)
  println("plusMany:" + plusMany.size + ":" + plusMany.last)
  val minusMany = s -- List(1, 2)
  println("minusMany:" + minusMany.size + ":" + minusMany.head)

  // 5. range / from / until — sorted-set range queries
  println("range:" + s.range(2, 4).iterator.toList.mkString(","))
  println("rangeFrom:" + s.rangeFrom(3).iterator.toList.mkString(","))
  println("rangeUntil:" + s.rangeUntil(4).iterator.toList.mkString(","))

  // 6. aggregate
  println("aggregate:" + s.foldLeft(0)(_ + _) + ":" + s.sum + ":" + s.filter(_ > 2).size + ":" + s.count(_ > 3))

  // 7. convert
  println("convert:" + s.toList.size + ":" + s.toSeq.size + ":" + s.iterator.size)
