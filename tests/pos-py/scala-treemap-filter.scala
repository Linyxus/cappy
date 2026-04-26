// Regression: lifted lambdas inside class-level methods of an
// `immutable.TreeMap` (a value class whose companion module also
// exists in the bundle) used to be emitted as `@staticmethod` on the
// value class but called via the underscored companion module
// singleton. The dispatch was wrong because the staticmethod is on
// `scala_collection_immutable_TreeMap` (the value class), not on
// `scala_collection_immutable_TreeMap_` (the module).
// See `notes/issue-treemap-filter-anonfun-wrong-owner.md`.

import scala.collection.immutable.TreeMap

@main def scalaTreeMapFilter(): Unit =
  val tm = TreeMap(1 -> 10, 2 -> 20, 3 -> 30, 4 -> 40, 5 -> 50)

  // 1. filter by value predicate
  val gt20 = tm.filter((_, v) => v > 20)
  println("filter:" + gt20.size + ":" + gt20.values.sum)
  println("filterKeys:" + gt20.keys.toList.mkString(","))

  // 2. filter by key predicate
  val keyEven = tm.filter((k, _) => k % 2 == 0)
  println("filterKey:" + keyEven.size + ":" + keyEven.keys.toList.mkString(","))

  // 3. filterNot
  val notSmall = tm.filterNot((_, v) => v <= 20)
  println("filterNot:" + notSmall.size + ":" + notSmall.values.sum)

  // 4. filter to empty
  val none = tm.filter((_, v) => v > 999)
  println("empty:" + none.size + ":" + none.isEmpty)

  // 5. partition uses a similar lifted-lambda pattern internally
  val (lo, hi) = tm.partition((_, v) => v < 30)
  println("partition:" + lo.size + ":" + hi.size + ":" + lo.values.sum + ":" + hi.values.sum)

  // 6. takeWhile uses the lifted-lambda path through `countWhile`
  val tw = tm.takeWhile((_, v) => v < 40)
  println("takeWhile:" + tw.size + ":" + tw.keys.toList.mkString(","))

  // 7. span — also goes through `countWhile`
  val (sl, sr) = tm.span((_, v) => v < 30)
  println("span:" + sl.size + ":" + sr.size)

  // 8. round-trip from + filter (covers `from(...)` constructor too)
  val rebuilt = TreeMap.from(List(1 -> 10, 2 -> 20, 3 -> 30)).filter((_, v) => v >= 20)
  println("rebuilt:" + rebuilt.size + ":" + rebuilt.values.sum)
