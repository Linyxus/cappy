// Coverage for scala.collection.immutable.ListSet — insertion-order set.

import scala.collection.immutable.ListSet

@main def scalaStdlibImmutableListSet(): Unit =
  val s = ListSet(1, 2, 3, 4, 5)

  // 1. build
  val empty = ListSet.empty[Int]
  val fromList = ListSet.from(List(10, 20, 30, 20, 10))
  println("build:" + empty.size + ":" + empty.isEmpty + ":" + fromList.size)

  // 2. access — head / last / size / contains
  println("access:" + s.head + ":" + s.last + ":" + s.size + ":" + s.contains(3) + ":" + s.contains(99))

  // 3. order — iteration is deterministic for ListSet
  println("order:" + s.iterator.toList.mkString(","))
  println("tail:" + s.tail.iterator.toList.mkString(","))

  // 4. transform — incl / excl / + / - / ++ / --
  val plus = s + 99
  println("plus:" + plus.size + ":" + plus.contains(99))
  val minus = s - 1
  println("minus:" + minus.size + ":" + minus.contains(1))
  val plusMany = s ++ ListSet(10, 20)
  println("plusMany:" + plusMany.size + ":" + plusMany.contains(20))
  val minusMany = s -- List(1, 2)
  println("minusMany:" + minusMany.size + ":" + minusMany.contains(3))

  // 5. aggregate — foldLeft / sum / filter / count / forall / exists
  println("aggregate:" + s.foldLeft(0)(_ + _) + ":" + s.sum + ":" + s.filter(_ > 2).size + ":" + s.count(_ > 3) + ":" + s.forall(_ > 0) + ":" + s.exists(_ > 4))

  // 6. convert
  println("convert:" + s.toList.size + ":" + s.toSeq.size + ":" + s.iterator.size)
