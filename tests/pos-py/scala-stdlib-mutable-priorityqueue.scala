// Coverage for `mutable.PriorityQueue`. Heap-ordered queue under
// `Ordering.Int`. The `Ordering` typeclass interaction is closure-heavy
// and could surface a Function1 boxing bug. We print the dequeue
// sequence to confirm priority semantics (max-first by default).

import scala.collection.mutable.PriorityQueue
import scala.math.Ordering.Int.given

@main def scalaStdlibMutablePriorityQueue(): Unit =
  val pq = PriorityQueue(3, 1, 4, 1, 5, 9, 2, 6)

  // build:
  println("build:" + pq.size + ":" + pq.head)
  val empty = PriorityQueue.empty[Int]
  println("empty:" + empty.size + ":" + empty.isEmpty)

  // access:
  println("access:" + pq.head + ":" + pq.size)
  println("hOpt:" + pq.headOption.getOrElse(-1))
  println("emptyChk:" + pq.isEmpty + ":" + pq.nonEmpty)

  // priority semantics: dequeue all, expect descending order (max-first)
  val drain = PriorityQueue(3, 1, 4, 1, 5, 9, 2, 6)
  val sb = new StringBuilder
  while drain.nonEmpty do
    if sb.nonEmpty then sb.append(",")
    sb.append(drain.dequeue())
  println("drain:" + sb.toString)

  // mutate: enqueue + dequeue interleaved
  val m = PriorityQueue(10, 20, 30)
  println("preMut:" + m.size + ":" + m.head)
  m.enqueue(25)
  println("enq1:" + m.size + ":" + m.head)
  val top = m.dequeue()
  println("deq1:" + top + ":" + m.size + ":" + m.head)
  m.enqueue(100)
  println("enq2:" + m.size + ":" + m.head)

  // varargs enqueue
  val v = PriorityQueue(1, 2)
  v.enqueue(5, 3, 4)
  println("enqVarargs:" + v.size + ":" + v.head)

  // clone independence
  val orig = PriorityQueue(1, 2, 3)
  val cloned = orig.clone()
  cloned.enqueue(99)
  println("clone:" + orig.size + ":" + cloned.size + ":" + cloned.head)

  // clear
  val toClear = PriorityQueue(1, 2, 3)
  toClear.clear()
  println("clear:" + toClear.isEmpty + ":" + toClear.size)

  // aggregate (iteration order is heap order, so use whole-collection ops)
  val small = PriorityQueue(1, 2, 3, 4)
  println("folds:" + small.sum + ":" + small.foldLeft(10)(_ + _))
  println("counts:" + pq.size + ":" + pq.count(_ > 3) + ":" + pq.exists(_ > 8) + ":" + pq.forall(_ > 0))

  // convert (sortedness preserved by toList? No - PriorityQueue.toList returns
  // in heap order; sort it explicitly to compare deterministically)
  val asList = pq.toList.sorted
  println("convList:" + asList.length + ":" + asList.head + ":" + asList.last)
  val asVec = pq.toVector.sorted
  println("convVec:" + asVec.size + ":" + asVec.head + ":" + asVec.last)
  println("convSet:" + pq.toSet.size)
