// Coverage for `mutable.ArraySeq`. Array-backed seq with primitive
// specialization. Supports in-place `update(idx, elem)` (i.e. `s(0) = x`).
// Each printed line is a labelled, derived value (sizes, sums, joins) —
// never raw `toString`.

import scala.collection.mutable.ArraySeq

@main def scalaStdlibMutableArraySeq(): Unit =
  val a = ArraySeq(1, 2, 3, 4, 5, 6, 7, 8)

  // build:
  println("build:" + a.size + ":" + a.head + ":" + a.last)
  val empty = ArraySeq.empty[Int]
  println("empty:" + empty.size + ":" + empty.isEmpty)
  val filled = ArraySeq.fill(4)(7)
  println("fill:" + filled.size + ":" + filled.head + ":" + filled.last)
  val tabulated = ArraySeq.tabulate(5)(_ * 2)
  println("tab:" + tabulated.size + ":" + tabulated.head + ":" + tabulated.last)
  val fromRange = ArraySeq.from(0 until 6)
  println("from:" + fromRange.size + ":" + fromRange.head + ":" + fromRange.last)

  // access:
  println("access:" + a(0) + ":" + a(7) + ":" + a.head + ":" + a.last)
  println("hOpt:" + a.headOption.getOrElse(-1) + ":" + a.lastOption.getOrElse(-1))
  println("emptyChk:" + a.isEmpty + ":" + a.nonEmpty)

  // iter order
  println("order:" + ArraySeq(1, 2, 3, 4, 5).mkString(","))

  // transform:
  val mapped = a.map(_ + 1)
  println("map:" + mapped.size + ":" + mapped.head + ":" + mapped.last)
  val filtered = a.filter(_ % 2 == 0)
  println("filter:" + filtered.size + ":" + filtered.head + ":" + filtered.last)
  val reversed = a.reverse
  println("reverse:" + reversed.size + ":" + reversed.head + ":" + reversed.last)

  // mutate: in-place update via apply
  val m = ArraySeq(10, 20, 30, 40)
  println("preMut:" + m(0) + ":" + m(3))
  m(0) = 99
  m(3) = 88
  println("update:" + m(0) + ":" + m(3) + ":" + m.size)

  // mutate via update method
  val m2 = ArraySeq(1, 2, 3)
  m2.update(1, 222)
  println("updateMethod:" + m2(0) + ":" + m2(1) + ":" + m2(2))

  // aggregate (in-place iteration, no new collection):
  val small = ArraySeq(1, 2, 3, 4)
  println("folds:" + small.sum + ":" + small.product + ":" + small.foldLeft(10)(_ + _))
  println("reduce:" + a.reduce(_ + _) + ":" + a.min + ":" + a.max)
  println("preds:" + a.contains(5) + ":" + a.indexOf(5) + ":" + a.exists(_ > 7) + ":" + a.forall(_ > 0))
  println("findCount:" + a.find(_ > 5).getOrElse(-1) + ":" + a.count(_ > 5))

  // slicing:
  println("slice:" + a.take(3).size + ":" + a.drop(3).size + ":" + a.slice(2, 5).size)
  println("twDw:" + a.takeWhile(_ < 5).size + ":" + a.dropWhile(_ < 5).size)

  // sorted:
  val unsorted = ArraySeq(3, 1, 4, 1, 5)
  println("sorted:" + unsorted.sorted.head + ":" + unsorted.sorted.last)

  // convert:
  println("convList:" + a.toList.length + ":" + a.toList.head + ":" + a.toList.last)
  println("convVec:" + a.toVector.size + ":" + a.toVector.head + ":" + a.toVector.last)
  println("convSet:" + a.toSet.size)
  println("mkString:" + ArraySeq(1, 2, 3).mkString(","))
