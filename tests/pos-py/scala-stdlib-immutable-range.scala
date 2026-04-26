// Coverage for `immutable.Range`. Range.map flows through
// IndexedSeqView.Map and previously hit a Function1 dispatch bug; the
// recent backend fix landed in the 14-commit series. Each printed line
// is a labelled, derived value (sizes, sums, joins) — never a raw
// `toString` of the collection.

@main def scalaStdlibImmutableRange(): Unit =
  val size = 16
  val r = 0 until size

  // build:
  println("build:" + r.size + ":" + r.head + ":" + r.last)
  val inc = 0 to size
  println("buildInc:" + inc.size + ":" + inc.head + ":" + inc.last)
  val step = 0 until size by 2
  println("buildStep:" + step.size + ":" + step.head + ":" + step.last)

  // access:
  println("access:" + r(0) + ":" + r(size - 1) + ":" + r.contains(7) + ":" + r.contains(99))
  println("emptyChk:" + r.isEmpty + ":" + r.nonEmpty)
  println("indexOf:" + r.indexOf(7) + ":" + r.indexOf(99))

  // iter order
  println("order:" + (0 until 5).mkString(","))
  println("orderStep:" + (0 until 10 by 2).mkString(","))
  println("orderInc:" + (0 to 5).mkString(","))

  // transform:
  val mapped = r.map(_ + 1)
  println("map:" + mapped.size + ":" + mapped.head + ":" + mapped.last)
  val filtered = r.filter(_ % 2 == 0)
  println("filter:" + filtered.size + ":" + filtered.head + ":" + filtered.last)
  val reversed = r.reverse
  println("reverse:" + reversed.size + ":" + reversed.head + ":" + reversed.last)

  // aggregate:
  val small = 0 until 5
  println("folds:" + small.sum + ":" + small.foldLeft(10)(_ + _) + ":" + small.foldRight(0)(_ + _))
  println("reduce:" + small.reduce(_ + _) + ":" + small.min + ":" + small.max)
  println("count:" + r.count(_ > 7) + ":" + r.exists(_ > 10) + ":" + r.forall(_ >= 0))

  // slicing:
  println("slice:" + r.take(3).size + ":" + r.drop(3).size + ":" + r.slice(2, 5).size)
  println("twDw:" + r.takeWhile(_ < 5).size + ":" + r.dropWhile(_ < 5).size)

  // convert:
  println("convList:" + r.toList.length + ":" + r.toList.head + ":" + r.toList.last)
  println("convVec:" + r.toVector.size + ":" + r.toVector.head + ":" + r.toVector.last)
  println("convSet:" + r.toSet.size)
  println("convArr:" + r.toArray.length + ":" + r.toArray(0))
  println("mkString:" + (0 until 4).mkString(","))

  // BigInt range factory:
  val br = Range.BigInt(0, 4, 1)
  println("bigintRange:" + br.size + ":" + br.head + ":" + br.last)
