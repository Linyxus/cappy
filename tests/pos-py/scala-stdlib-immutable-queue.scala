// Coverage for `immutable.Queue`. Queue is implemented as two stacks under
// the hood; its `dequeue` returns `(elem, newQueue)` and `enqueue` returns
// a new Queue. Each printed line is a labelled, derived value (sizes,
// elements, sums, joins) — never raw `toString`.

import scala.collection.immutable.Queue

@main def scalaStdlibImmutableQueue(): Unit =
  val size = 16
  val q = Queue(1, 2, 3, 4, 5, 6, 7, 8)

  // build:
  println("build:" + q.size + ":" + q.head + ":" + q.last)
  val empty = Queue.empty[Int]
  println("empty:" + empty.size + ":" + empty.isEmpty)
  val fromRange = Queue.from(0 until size)
  println("from:" + fromRange.size + ":" + fromRange.head + ":" + fromRange.last)

  // access:
  println("access:" + q.head + ":" + q.last + ":" + q(0) + ":" + q(7))
  println("hOpt:" + q.headOption.getOrElse(-1) + ":" + q.lastOption.getOrElse(-1))
  println("tailInit:" + q.tail.size + ":" + q.tail.head + ":" + q.init.size + ":" + q.init.last)
  println("emptyChk:" + q.isEmpty + ":" + q.nonEmpty)

  // iter order
  println("order:" + Queue(1, 2, 3, 4, 5).mkString(","))

  // queue api: enqueue / dequeue (functional)
  val enq = q.enqueue(99)
  println("enqueue:" + enq.size + ":" + enq.head + ":" + enq.last)
  val (deqElem, deqRest) = q.dequeue
  println("dequeue:" + deqElem + ":" + deqRest.size + ":" + deqRest.head + ":" + deqRest.last)
  // enqueue chains: original is unchanged
  println("immut:" + q.size + ":" + q.head + ":" + q.last)

  // dequeueOption
  val dOpt = q.dequeueOption
  println("dequeueOpt:" + dOpt.isDefined + ":" + dOpt.get._1 + ":" + dOpt.get._2.size)

  // transform:
  val mapped = q.map(_ + 1)
  println("map:" + mapped.size + ":" + mapped.head + ":" + mapped.last)
  val filtered = q.filter(_ % 2 == 0)
  println("filter:" + filtered.size + ":" + filtered.head + ":" + filtered.last)
  val reversed = q.reverse
  println("reverse:" + reversed.size + ":" + reversed.head + ":" + reversed.last)

  // aggregate:
  val small = Queue(1, 2, 3, 4)
  println("folds:" + small.sum + ":" + small.product + ":" + small.foldLeft(10)(_ + _))
  println("reduce:" + q.reduce(_ + _) + ":" + q.min + ":" + q.max)
  println("preds:" + q.contains(5) + ":" + q.indexOf(5) + ":" + q.exists(_ > 7) + ":" + q.forall(_ > 0))
  println("findCount:" + q.find(_ > 5).getOrElse(-1) + ":" + q.count(_ > 5))

  // slicing
  println("slice:" + q.take(3).size + ":" + q.drop(3).size + ":" + q.slice(2, 5).size)
  println("twDw:" + q.takeWhile(_ < 5).size + ":" + q.dropWhile(_ < 5).size)

  // convert:
  println("convList:" + q.toList.length + ":" + q.toList.head + ":" + q.toList.last)
  println("convVec:" + q.toVector.size + ":" + q.toVector.head + ":" + q.toVector.last)
  println("convSet:" + q.toSet.size)
  println("mkString:" + Queue(1, 2, 3).mkString(","))
