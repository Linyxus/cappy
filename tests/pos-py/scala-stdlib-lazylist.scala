// Broader stdlib coverage for `immutable.LazyList[Int]` beyond the build/access/
// transform/mutate set in `scala-bench-immutable-lazylist.scala`. Each line of
// output is a labelled, derived value (sizes, indices, sums) — never a raw
// `toString` of an infinite LazyList. Forced finite prefixes are the only
// values printed.
//
// All previously-skipped ops (`.map`, `.mkString` directly on LazyList,
// and `LazyList ++ LazyList`) are now exercised: they were collaterally
// fixed by the `PyApplyStatic` self-owned-static-method rerouting fix
// (`hasOwnStaticMethod` in `PyIREmitter`). The lifted helpers
// (`map_anonfun`, `appendHead_1`, `appendedAll_anonfun_1`) are emitted
// as `@staticmethod`s on the `LazyList` value class itself, not on the
// `LazyList_` companion module, so dispatching on the class directly
// resolves them.

@main def scalaStdlibLazyList(): Unit =
  val ll = LazyList(1, 2, 3, 4, 5, 6, 7, 8)

  // 1. construction variants
  val empty = LazyList.empty[Int]
  val fromInf = LazyList.from(0).take(5)
  val iter = LazyList.iterate(1)(_ * 2).take(5)
  val ranged = LazyList.range(0, 5)
  println("ctor:" + empty.size + ":" + fromInf.size + ":" + iter.size + ":" + ranged.size)

  // 2. accessors / option-returning heads
  println("headTail:" + ll.head + ":" + ll.tail.head)
  println("emptyFlags:" + ll.isEmpty + ":" + ll.nonEmpty + ":" + empty.isEmpty)
  println("len:" + ll.length)

  // 3. search predicates
  println("preds:" + ll.contains(3) + ":" + ll.indexOf(3) + ":" + ll.exists(_ > 5) + ":" + ll.forall(_ > 0))

  // 4. count / find
  println("findCount:" + ll.find(_ > 5).getOrElse(-1) + ":" + ll.count(_ > 5))

  // 5. slicing — convert to List for size since LazyList itself is what we are slicing
  println("slice:" + ll.take(3).toList.size + ":" + ll.drop(3).toList.size + ":" + ll.slice(1, 4).toList.size)
  println("twDw:" + ll.takeWhile(_ < 5).toList.size + ":" + ll.dropWhile(_ < 5).toList.size)

  // 6. higher-order — .map / .filter / .flatMap all work.
  println("filter:" + ll.filter(_ > 5).take(3).toList.size)
  println("flatMap:" + LazyList(1, 2, 3).flatMap(i => LazyList(i, i * 10)).take(6).toList.size)
  println("map:" + LazyList(1, 2, 3).map(_ * 10).take(3).toList.mkString(","))

  // 7. lazy semantics — force a finite prefix of an infinite LazyList
  val infPrefix = LazyList.from(0).take(5).toList
  println("inf:" + infPrefix.size + ":" + infPrefix.sum)

  // 8. folds (over a finite LazyList)
  val small = LazyList(1, 2, 3, 4)
  println("folds:" + small.sum + ":" + small.product + ":" + small.foldLeft(10)(_ + _) + ":" + small.reduce(_ + _))
  println("rmm:" + ll.min + ":" + ll.max)

  // 9. foreach-driven counter
  var c = 0
  ll.foreach(c += _)
  println("foreachSum:" + c)

  // 10. cons (#::) prepend
  val prep = 0 #:: ll
  println("cons:" + prep.head + ":" + prep.length)

  // 11. iterate / from results — mkString directly on LazyList works.
  println("iter:" + iter.mkString(","))
  println("rangeStr:" + ranged.mkString(","))

  // 11b. concatenation — `LazyList ++ LazyList` builds a deferred thunk.
  val cat = LazyList(1, 2) ++ LazyList(3, 4)
  println("concat:" + cat.length + ":" + cat.mkString(","))

  // 12. conversions
  println("conv:" + ll.toList.size + ":" + ll.toVector.size)
