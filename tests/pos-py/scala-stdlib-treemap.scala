// Broader stdlib coverage for `immutable.TreeMap` beyond the build/access/
// transform/mutate set in `scala-bench-immutable-treemap.scala`. TreeMap
// iterates in key-sorted order, so prints are fully deterministic.

import scala.collection.immutable.TreeMap

@main def scalaStdlibTreeMap(): Unit =
  val tm = TreeMap(1 -> 10, 2 -> 20, 3 -> 30, 4 -> 40, 5 -> 50)

  // 1. construction variants
  val empty = TreeMap.empty[Int, Int]
  println("empty:" + empty.size + ":" + empty.isEmpty)

  val fromPairs = TreeMap.from(List(2 -> 20, 1 -> 10, 3 -> 30))
  println("fromPairs:" + fromPairs.size + ":" + fromPairs.head._1 + ":" + fromPairs.last._1)

  // 2. accessors / extremes (sorted by key)
  println("headLast:" + tm.head._1 + ":" + tm.head._2 + ":" + tm.last._1 + ":" + tm.last._2)
  println("firstLastKey:" + tm.firstKey + ":" + tm.lastKey)

  // 3. lookup
  println("lookup:" + tm.get(2).getOrElse(-1) + ":" + tm.contains(2) + ":" + tm.getOrElse(99, -1))

  // 4. sorted iteration over keys / values / pairs
  println("keys:" + tm.keys.toList.mkString(","))
  println("values:" + tm.values.toList.mkString(","))
  println("pairs:" + tm.toList.map((k, v) => k + "=" + v).mkString(","))

  // 5. predicates / counts / filter
  println("preds:" + tm.exists((_, v) => v == 30) + ":" + tm.count((_, v) => v > 20))
  println("filter:" + tm.filter((_, v) => v > 20).size + ":" + tm.filter((_, v) => v > 20).values.sum)

  // 6. range queries (sorted, half-open)
  println("range:" + tm.range(2, 4).size + ":" + tm.range(2, 4).keys.toList.mkString(","))
  println("fromUntil:" + tm.from(3).size + ":" + tm.until(3).size)

  // 7. updates: updated / removed / `+`
  println("updates:" + tm.updated(7, 70).get(7).getOrElse(-1) + ":" + tm.removed(1).size + ":" + (tm + (8 -> 80)).get(8).getOrElse(-1))

  // 8. folds over values
  println("folds:" + tm.foldLeft(0)((acc, kv) => acc + kv._2) + ":" + tm.values.sum)

  // 9. take / drop on sorted order
  println("takeDrop:" + tm.take(2).keys.toList.mkString(",") + ":" + tm.drop(3).keys.toList.mkString(","))

  // 10. conversions
  println("conv:" + tm.toList.size + ":" + tm.toMap.size + ":" + tm.iterator.size)

  // 11. min / max by key
  println("minMax:" + tm.minBy((k, _) => k)._1 + ":" + tm.maxBy((k, _) => k)._1)

  // 12. mkString of entries (deterministic)
  println("mkString:" + tm.mkString(";"))

  // 13. hashCode equality across two TreeMaps with the same key/value
  //     content but different insertion orders. Regression coverage for
  //     `notes/issue-treemap-hashcode-none.md`: TreeMap inherits
  //     `hashCode` from `Map`, but its own `equals` would otherwise
  //     null-out Python's `__hash__` slot.
  val tmA = TreeMap(1 -> 10, 2 -> 20, 3 -> 30)
  val tmB = TreeMap(3 -> 30, 1 -> 10, 2 -> 20)
  println("hashEq:" + (tmA.hashCode == tmB.hashCode))
