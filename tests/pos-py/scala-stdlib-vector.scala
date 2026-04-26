// Broader stdlib coverage for `immutable.Vector` beyond the build/access/
// transform/mutate set in `scala-bench-immutable-vector.scala`. Each line of
// output is a labelled, derived value (sizes, indices, sums, joins) — never a
// raw `toString` of the collection.

@main def scalaStdlibVector(): Unit =
  val vec = Vector(1, 2, 3, 4, 5, 6, 7, 8)

  // 1. construction variants
  val filled = Vector.fill(4)(7)
  println("fill:" + filled.size + ":" + filled(0) + ":" + filled(3))

  val tabulated = Vector.tabulate(5)(_ * 2)
  println("tabulate:" + tabulated.size + ":" + tabulated(0) + ":" + tabulated(4))

  val ranged = Vector.range(0, 5)
  println("range:" + ranged.size + ":" + ranged(0) + ":" + ranged(4))

  // 2. accessors / option-returning heads
  println("headLast:" + vec.head + ":" + vec.last)
  println("hOpt:" + vec.headOption.getOrElse(-1) + ":" + vec.lastOption.getOrElse(-1))
  println("tailInit:" + vec.tail.size + ":" + vec.init.size)

  // 3. predicates
  println("preds:" + vec.contains(5) + ":" + vec.indexOf(5) + ":" + vec.exists(_ > 7) + ":" + vec.forall(_ > 0))

  // 4. count / find
  val foundOpt = vec.find(_ > 5)
  println("findCount:" + foundOpt.getOrElse(-1) + ":" + vec.count(_ > 5))

  // 5. slicing
  println("slice:" + vec.take(3).size + ":" + vec.drop(3).size + ":" + vec.slice(2, 5).size)

  // 6. takeWhile / dropWhile
  println("twDw:" + vec.takeWhile(_ < 5).size + ":" + vec.dropWhile(_ < 5).size)

  // 7. folds / sum / product (small numbers to avoid overflow)
  val small = Vector(1, 2, 3, 4)
  println("folds:" + small.sum + ":" + small.product + ":" + small.foldLeft(10)(_ + _))

  // 8. reduce / min / max
  println("rmm:" + vec.reduce(_ + _) + ":" + vec.min + ":" + vec.max)

  // 9. updates: updated / appended / prepended
  println("upd:" + vec.updated(0, 99).head + ":" + (vec :+ 0).last + ":" + (-1 +: vec).head)

  // 10. conversions
  println("conv:" + vec.toList.length + ":" + vec.toSet.size + ":" + vec.iterator.size)

  // 11. mkString
  println("mkString:" + Vector(1, 2, 3).mkString(","))

  // 12. sorted / reverse
  val unsorted = Vector(3, 1, 4, 1, 5)
  println("sortRev:" + unsorted.sorted.head + ":" + unsorted.reverse.head)
