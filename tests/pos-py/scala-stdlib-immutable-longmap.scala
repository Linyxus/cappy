// Broad coverage of scala.collection.immutable.LongMap.
// Iteration is in unsigned key order; non-negative keys give natural order.
//
// Built without Range.map (see CLAUDE.md): pairs come from a literal List.

import scala.collection.immutable.LongMap

@main def scalaStdlibImmutableLongMap(): Unit =
  val m = LongMap.empty[Int] + (1L -> 10) + (2L -> 20) + (3L -> 30) + (4L -> 40) + (5L -> 50)

  // 1. build
  val empty = LongMap.empty[Int]
  val fromList = LongMap.from(List((10L, 100), (20L, 200), (30L, 300)))
  val applied = LongMap((7L, 70), (8L, 80))
  println("build:" + empty.size + ":" + empty.isEmpty + ":" + fromList.size + ":" + applied.size)

  // 2. access
  println("access:" + m.firstKey + ":" + m.lastKey + ":" + m.get(2L).getOrElse(-1) + ":" + m.contains(3L) + ":" + m(4L) + ":" + m.getOrElse(99L, -1))

  // 3. order — iterator + keys + values in sorted key order
  println("order:" + m.iterator.toList.map((k, v) => k + "=" + v).mkString(","))
  println("keys:" + m.keysIterator.toList.mkString(","))
  println("values:" + m.valuesIterator.toList.mkString(","))

  // 4. transform — filter / updated / removed
  val filt = m.filter((_, v) => v > 20)
  println("transform:" + filt.size + ":" + filt.firstKey + ":" + filt.lastKey)
  val upd = m.updated(6L, 60)
  println("updated:" + upd.size + ":" + upd.lastKey + ":" + upd.get(6L).getOrElse(-1))
  val rm = m.removed(1L)
  println("removed:" + rm.size + ":" + rm.firstKey + ":" + rm.contains(1L))

  // 5. aggregate
  println("aggregate:" + m.foldLeft(0)((acc, kv) => acc + kv._2) + ":" + m.values.sum + ":" + m.size)

  // 6. union / intersection
  val a = LongMap.empty[Int] + (1L -> 1) + (2L -> 2) + (3L -> 3)
  val b = LongMap.empty[Int] + (2L -> 20) + (3L -> 30) + (4L -> 40)
  val u = a ++ b
  println("union:" + u.size + ":" + u.iterator.toList.map((k, v) => k + "=" + v).mkString(","))
  val inter = a.intersection(b)
  println("intersect:" + inter.size + ":" + inter.iterator.toList.map((k, v) => k + "=" + v).mkString(","))

  // 7. convert
  println("convert:" + m.toList.size + ":" + m.toMap.size + ":" + m.iterator.size)
