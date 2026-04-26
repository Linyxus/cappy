// Coverage for scala.collection.mutable.TreeMap — sorted map.

import scala.collection.mutable.TreeMap

@main def scalaStdlibMutableTreeMap(): Unit =
  val tm = TreeMap(1 -> 10, 2 -> 20, 3 -> 30, 4 -> 40, 5 -> 50)

  // 1. build
  val empty = TreeMap.empty[Int, Int]
  val fromList = TreeMap.from(List(2 -> 20, 1 -> 10, 3 -> 30))
  println("build:" + empty.size + ":" + empty.isEmpty + ":" + fromList.size)

  // 2. access
  println("access:" + tm.head._1 + "=" + tm.head._2 + ":" + tm.last._1 + "=" + tm.last._2 + ":" + tm.size + ":" + tm.contains(3) + ":" + tm.firstKey + ":" + tm.lastKey)
  println("getOrElse:" + tm.get(2).getOrElse(-1) + ":" + tm.getOrElse(99, -1) + ":" + tm(4))

  // 3. order — sorted iteration
  println("order:" + tm.iterator.map((k, v) => k.toString + "=" + v.toString).mkString(","))

  // 4. transform — filter / count / exists go through iterator now.
  println("transform:" + tm.filter(_._1 > 2).size + ":" + tm.count(_._2 > 20) + ":" + tm.exists(_._1 == 4))

  // 5. range / from / until — projections also iterate.
  println("range:" + tm.range(2, 5).keys.toList.mkString(","))
  println("rangeFrom:" + tm.rangeFrom(3).keys.toList.mkString(","))
  println("rangeUntil:" + tm.rangeUntil(4).keys.toList.mkString(","))

  // 6. mutate — update / += / -= / ++=
  tm.update(6, 60)
  println("mutate1:" + tm.size + ":" + tm.lastKey + ":" + tm.get(6).getOrElse(-1))
  tm += (7 -> 70)
  println("mutate2:" + tm.size + ":" + tm.lastKey)
  tm -= 1
  println("mutate3:" + tm.size + ":" + tm.firstKey + ":" + tm.contains(1))
  tm ++= List(8 -> 80, 9 -> 90)
  println("mutate4:" + tm.size + ":" + tm.lastKey)

  // 7. aggregate via iterator-driven foldLeft.
  println("aggregate:" + tm.foldLeft(0)((acc, kv) => acc + kv._2))

  // 8. convert — toList / toMap.
  println("convert:" + tm.toList.size + ":" + tm.toMap.size + ":" + tm.keys.toList.mkString(","))

  // 9. clone goes through iterator.
  val cloned = tm.clone()
  println("clone:" + cloned.size + ":" + cloned.lastKey)
  val before = tm.size
  tm.clear()
  println("clear:" + tm.size + ":" + before)
