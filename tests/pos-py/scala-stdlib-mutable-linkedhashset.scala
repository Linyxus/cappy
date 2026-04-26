// Coverage for scala.collection.mutable.LinkedHashSet — insertion-order set.

import scala.collection.mutable.LinkedHashSet

@main def scalaStdlibMutableLinkedHashSet(): Unit =
  val s = LinkedHashSet(1, 2, 3, 4, 5)

  // 1. build
  val empty = LinkedHashSet.empty[Int]
  val fromList = LinkedHashSet.from(List(10, 20, 30, 20, 10))
  println("build:" + empty.size + ":" + empty.isEmpty + ":" + fromList.size)

  // 2. access — head / last / contains / size
  println("access:" + s.head + ":" + s.last + ":" + s.size + ":" + s.contains(3) + ":" + s.contains(99))

  // 3. order — iteration is deterministic for LinkedHashSet
  println("order:" + s.iterator.toList.mkString(","))

  // 4. transform — read-only HOFs
  println("transform:" + s.filter(_ > 2).size + ":" + s.count(_ > 3) + ":" + s.exists(_ == 5) + ":" + s.forall(_ > 0))

  // 5. mutate — += / -= / ++= / --=
  s += 99
  println("mutate1:" + s.size + ":" + s.last + ":" + s.contains(99))
  s -= 1
  println("mutate2:" + s.size + ":" + s.head + ":" + s.contains(1))
  s ++= LinkedHashSet(100, 200)
  println("mutate3:" + s.size + ":" + s.last)
  s --= List(2, 3)
  println("mutate4:" + s.size + ":" + s.contains(2))

  // 6. aggregate
  println("aggregate:" + s.foldLeft(0)(_ + _) + ":" + s.sum)

  // 7. convert
  println("convert:" + s.toList.size + ":" + s.toSeq.size + ":" + s.iterator.size)

  // 8. clone + clear
  val cloned = s.clone()
  println("clone:" + cloned.size)
  s.clear()
  println("clear:" + s.size + ":" + cloned.size)
