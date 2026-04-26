// Broader stdlib coverage for `mutable.ArrayDeque` beyond the build/access/
// transform/mutate set in `scala-bench-mutable-arraydeque.scala`. Adds deque-
// specific operations (head/last, prepend, append, removeHead, removeLast,
// clone) alongside the iterable surface. Each printed line is a labelled,
// derived value (sizes/elements/sums) — never raw `toString`.

import scala.collection.mutable.ArrayDeque

@main def scalaStdlibArrayDeque(): Unit =
  val dq = ArrayDeque(1, 2, 3, 4, 5, 6, 7, 8)

  // 1. accessors and option-returning heads
  println("headLast:" + dq.head + ":" + dq.last)
  println("hOpt:" + dq.headOption.getOrElse(-1) + ":" + dq.lastOption.getOrElse(-1))
  println("emptyChk:" + dq.isEmpty + ":" + dq.nonEmpty)

  // 2. predicates / search
  println("preds:" + dq.contains(5) + ":" + dq.indexOf(5) + ":" + dq.exists(_ > 7) + ":" + dq.forall(_ > 0))

  // 3. find + count
  println("findCount:" + dq.find(_ > 5).getOrElse(-1) + ":" + dq.count(_ > 5))

  // 4. folds / sum / product
  val small = ArrayDeque(1, 2, 3, 4)
  println("folds:" + small.sum + ":" + small.product + ":" + small.foldLeft(10)(_ + _))

  // 5. reduce / min / max
  println("rmm:" + dq.reduce(_ + _) + ":" + dq.min + ":" + dq.max)

  // 6. foreach-driven counter
  var c = 0
  dq.foreach(c += _)
  println("foreachSum:" + c)

  // 7. mkString (read-only iteration)
  println("mkString:" + ArrayDeque(1, 2, 3).mkString(","))

  // 8. deque-specific append/prepend (mutating)
  val ap = ArrayDeque(2, 3, 4)
  ap.prepend(1)
  ap.append(5)
  println("prependAppend:" + ap.size + ":" + ap.head + ":" + ap.last)

  // 9. removeHead / removeLast
  val rm = ArrayDeque(10, 20, 30, 40)
  val first = rm.removeHead()
  val lastV = rm.removeLast()
  println("removeHL:" + first + ":" + lastV + ":" + rm.size + ":" + rm.head + ":" + rm.last)

  // 10. clone (independence check)
  val orig = ArrayDeque(1, 2, 3)
  val cloned = orig.clone()
  cloned.append(99)
  println("clone:" + orig.size + ":" + cloned.size + ":" + cloned.last)

  // 11. clear
  val toClear = ArrayDeque(1, 2, 3)
  toClear.clear()
  println("clear:" + toClear.isEmpty + ":" + toClear.size)

  // 12. sorted (returns a new strict collection, not a view)
  val unsorted = ArrayDeque(3, 1, 4, 1, 5)
  val sortedList = unsorted.sorted
  println("sorted:" + sortedList.head + ":" + sortedList.last)
