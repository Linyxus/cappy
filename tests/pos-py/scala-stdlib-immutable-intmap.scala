// Broad coverage of scala.collection.immutable.IntMap.
// Iteration is in unsigned-int key order; we use non-negative keys so that
// matches natural increasing order, making prints deterministic.
//
// Built without Range.map (see CLAUDE.md): pairs come from a literal List.

import scala.collection.immutable.IntMap

@main def scalaStdlibImmutableIntMap(): Unit =
  // Build via repeated `+` to dodge Range.map closure paths; IntMap.from
  // accepts IterableOnce and is also exercised below.
  val m = IntMap.empty[Int] + (1 -> 10) + (2 -> 20) + (3 -> 30) + (4 -> 40) + (5 -> 50)

  // 1. build
  val empty = IntMap.empty[Int]
  val fromList = IntMap.from(List((10, 100), (20, 200), (30, 300)))
  val applied = IntMap((7, 70), (8, 80))
  println("build:" + empty.size + ":" + empty.isEmpty + ":" + fromList.size + ":" + applied.size)

  // 2. access (firstKey / lastKey / get / contains / apply / getOrElse)
  println("access:" + m.firstKey + ":" + m.lastKey + ":" + m.get(2).getOrElse(-1) + ":" + m.contains(3) + ":" + m(4) + ":" + m.getOrElse(99, -1))

  // 3. order — iterator + keys + values in sorted key order
  println("order:" + m.iterator.toList.map((k, v) => k + "=" + v).mkString(","))
  println("keys:" + m.keysIterator.toList.mkString(","))
  println("values:" + m.valuesIterator.toList.mkString(","))

  // 4. transform — filter / transform / updated / removed / + / -
  val filt = m.filter((_, v) => v > 20)
  println("transform:" + filt.size + ":" + filt.firstKey + ":" + filt.lastKey)
  val tr = m.transform((k, v) => v + k)
  println("transform2:" + tr.iterator.toList.map((k, v) => k + "=" + v).mkString(","))
  val upd = m.updated(6, 60)
  println("updated:" + upd.size + ":" + upd.lastKey + ":" + upd.get(6).getOrElse(-1))
  val rm = m.removed(1)
  println("removed:" + rm.size + ":" + rm.firstKey + ":" + rm.contains(1))

  // 5. aggregate — foldLeft / values.sum / size / min/max via keys
  println("aggregate:" + m.foldLeft(0)((acc, kv) => acc + kv._2) + ":" + m.values.sum + ":" + m.size)

  // 6. union / intersection (IntMap-specific bulk ops)
  val a = IntMap.empty[Int] + (1 -> 1) + (2 -> 2) + (3 -> 3)
  val b = IntMap.empty[Int] + (2 -> 20) + (3 -> 30) + (4 -> 40)
  val u = a ++ b
  println("union:" + u.size + ":" + u.iterator.toList.map((k, v) => k + "=" + v).mkString(","))
  val inter = a.intersection(b)
  println("intersect:" + inter.size + ":" + inter.iterator.toList.map((k, v) => k + "=" + v).mkString(","))

  // 7. convert
  println("convert:" + m.toList.size + ":" + m.toMap.size + ":" + m.iterator.size)
