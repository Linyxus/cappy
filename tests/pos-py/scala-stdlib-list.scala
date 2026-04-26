// Broader stdlib coverage for `immutable.List[Int]` beyond the build/access/
// transform/mutate set in `scala-bench-immutable-list.scala`. Each line of
// output is a labelled, derived value (sizes, indices, sums) — never a raw
// `toString` of the collection.

@main def scalaStdlibList(): Unit =
  val xs = List(1, 2, 3, 4, 5, 6, 7, 8)

  // 1. construction variants
  val filled = List.fill(4)(7)
  val tabulated = List.tabulate(5)(_ * 2)
  val ranged = List.range(0, 5)
  val fromVec = List.from(Vector(1, 2, 3))
  println("ctor:" + filled.size + ":" + tabulated.size + ":" + ranged.size + ":" + fromVec.size)

  // 2. accessors / option-returning heads
  println("headLast:" + xs.head + ":" + xs.last)
  println("hOpt:" + xs.headOption.getOrElse(-1) + ":" + xs.lastOption.getOrElse(-1))
  println("tailInit:" + xs.tail.size + ":" + xs.init.size + ":" + xs.length + ":" + xs.isEmpty + ":" + xs.nonEmpty)

  // 3. search predicates
  println("preds:" + xs.contains(2) + ":" + xs.indexOf(2) + ":" + xs.exists(_ > 5) + ":" + xs.forall(_ > 0))

  // 4. count / find
  println("findCount:" + xs.find(_ > 5).getOrElse(-1) + ":" + xs.count(_ > 5))

  // 5. slicing
  println("slice:" + xs.take(2).size + ":" + xs.drop(2).size + ":" + xs.slice(1, 3).size)
  println("twDw:" + xs.takeWhile(_ < 5).size + ":" + xs.dropWhile(_ < 5).size)

  // 6. higher-order
  println("filter:" + xs.filter(_ > 5).size + ":" + xs.filterNot(_ > 5).size)
  println("flatMap:" + xs.flatMap(i => List(i, i * 2)).size)
  println("zipIdx:" + xs.zipWithIndex.size)
  println("collect:" + xs.collect { case i if i > 5 => i * 10 }.sum)

  // 7. folds
  val small = List(1, 2, 3, 4)
  println("folds:" + small.sum + ":" + small.product + ":" + small.foldLeft(10)(_ + _) + ":" + small.foldRight(0)(_ + _))
  println("rmm:" + xs.reduce(_ + _) + ":" + xs.min + ":" + xs.max)
  println("scanLeft:" + small.scanLeft(0)(_ + _).mkString(","))

  // 8. foreach-driven counter
  var c = 0
  xs.foreach(c += _)
  println("foreachSum:" + c)

  // 9. sorting
  val unsorted = List(3, 1, 4, 1, 5)
  println("sortRev:" + unsorted.sorted.head + ":" + unsorted.sortWith(_ > _).head + ":" + unsorted.reverse.head)

  // 10. partition
  val (lo, hi) = xs.partition(_ < 5)
  println("partition:" + lo.size + ":" + hi.size)

  // 11. conversions
  println("conv:" + xs.toVector.size + ":" + xs.toSet.size + ":" + xs.iterator.size)

  // 12. mkString
  println("mkString:" + List(1, 2, 3).mkString(","))
