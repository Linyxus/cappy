// Broader stdlib coverage for `mutable.ArrayBuffer` beyond the build/access/
// transform/mutate set in `scala-bench-mutable-arraybuffer.scala`. Each line
// of output is a labelled, derived value (sizes, indices, sums) — never a raw
// `toString` of the collection.

import scala.collection.mutable.ArrayBuffer

@main def scalaStdlibArrayBuffer(): Unit =
  val buf = ArrayBuffer(1, 2, 3, 4, 5, 6, 7, 8)

  // 1. construction variants
  val filled = ArrayBuffer.fill(4)(7)
  println("fill:" + filled.size + ":" + filled(0) + ":" + filled(3))

  val tabulated = ArrayBuffer.tabulate(5)(_ * 2)
  println("tabulate:" + tabulated.size + ":" + tabulated(0) + ":" + tabulated(4))

  // 2. accessors / option-returning heads
  println("headLast:" + buf.head + ":" + buf.last)
  println("hOpt:" + buf.headOption.getOrElse(-1) + ":" + buf.lastOption.getOrElse(-1))
  println("tailInit:" + buf.tail.size + ":" + buf.init.size)

  // 3. predicates
  println("preds:" + buf.contains(5) + ":" + buf.indexOf(5) + ":" + buf.exists(_ > 7) + ":" + buf.forall(_ > 0))

  // 4. count / find
  val foundOpt = buf.find(_ > 5)
  println("findCount:" + foundOpt.getOrElse(-1) + ":" + buf.count(_ > 5))

  // 5. slicing
  println("slice:" + buf.take(3).size + ":" + buf.drop(3).size + ":" + buf.slice(2, 5).size)

  // 6. takeWhile / dropWhile
  println("twDw:" + buf.takeWhile(_ < 5).size + ":" + buf.dropWhile(_ < 5).size)

  // 7. folds / sum / product (small numbers to avoid overflow)
  val small = ArrayBuffer(1, 2, 3, 4)
  println("folds:" + small.sum + ":" + small.product + ":" + small.foldLeft(10)(_ + _))

  // 8. reduce / min / max
  println("rmm:" + buf.reduce(_ + _) + ":" + buf.min + ":" + buf.max)

  // 9. foreach-driven counter
  var c = 0
  buf.foreach(c += _)
  println("foreachSum:" + c)

  // 10. conversions
  println("conv:" + buf.toList.length + ":" + buf.toVector.size + ":" + buf.toSet.size)

  // 11. mkString
  println("mkString:" + ArrayBuffer(1, 2, 3).mkString(","))

  // 12. sorted / reverse
  val unsorted = ArrayBuffer(3, 1, 4, 1, 5)
  println("sortRev:" + unsorted.sorted.head + ":" + unsorted.reverse.head)
