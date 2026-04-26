// Coverage for scala.collection.mutable.TreeSet — sorted set.

import scala.collection.mutable.TreeSet

@main def scalaStdlibMutableTreeSet(): Unit =
  val s = TreeSet(5, 1, 3, 2, 4)

  // 1. build
  val empty = TreeSet.empty[Int]
  val fromList = TreeSet.from(List(3, 1, 2, 1, 3))
  println("build:" + empty.size + ":" + empty.isEmpty + ":" + fromList.size)

  // 2. access — head / last / firstKey / lastKey / contains
  println("access:" + s.head + ":" + s.last + ":" + s.firstKey + ":" + s.lastKey + ":" + s.contains(3) + ":" + s.contains(99))

  // 3. order — sorted iteration
  println("order:" + s.iterator.mkString(","))

  // 4. transform — filter / count / exists.
  println("transform:" + s.filter(_ > 2).size + ":" + s.count(_ > 3) + ":" + s.exists(_ == 4))

  // 5. range / from / until.
  println("range:" + s.range(2, 5).toList.mkString(","))
  println("rangeFrom:" + s.rangeFrom(3).toList.mkString(","))
  println("rangeUntil:" + s.rangeUntil(4).toList.mkString(","))

  // 6. mutate — += / -= / ++=
  s += 99
  println("mutate1:" + s.size + ":" + s.lastKey + ":" + s.contains(99))
  s -= 1
  println("mutate2:" + s.size + ":" + s.firstKey + ":" + s.contains(1))
  s ++= List(100, 200)
  println("mutate3:" + s.size + ":" + s.lastKey)
  s --= List(2, 3)
  println("mutate4:" + s.size + ":" + s.firstKey)

  // 7. aggregate via iterator-driven sum.
  println("aggregate:" + s.sum)

  // 8. convert — toList / toSeq / iterator.
  println("convert:" + s.toList.size + ":" + s.toSeq.size + ":" + s.iterator.size)

  // 9. clone.
  val cloned = s.clone()
  println("clone:" + cloned.size + ":" + cloned.lastKey)
  val before = s.size
  s.clear()
  println("clear:" + s.size + ":" + before)
