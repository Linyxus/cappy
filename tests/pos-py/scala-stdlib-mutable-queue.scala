// Coverage for `mutable.Queue`. Mutable queue uses in-place enqueue and
// dequeue (FIFO). `front` returns the head without removing. Each printed
// line is a labelled, derived value (sizes, elements) — never a raw
// `toString` of the collection.

import scala.collection.mutable.Queue

@main def scalaStdlibMutableQueue(): Unit =
  val size = 16
  val q = Queue(1, 2, 3, 4, 5, 6, 7, 8)

  // build:
  println("build:" + q.size + ":" + q.head + ":" + q.last)
  val empty = Queue.empty[Int]
  println("empty:" + empty.size + ":" + empty.isEmpty)
  val fromRange = Queue.from(0 until size)
  println("from:" + fromRange.size + ":" + fromRange.head + ":" + fromRange.last)

  // access:
  println("access:" + q.head + ":" + q.last + ":" + q.front)
  println("hOpt:" + q.headOption.getOrElse(-1) + ":" + q.lastOption.getOrElse(-1))
  println("emptyChk:" + q.isEmpty + ":" + q.nonEmpty)

  // iter order
  println("order:" + Queue(1, 2, 3, 4, 5).mkString(","))

  // transform:
  val mapped = q.map(_ + 1)
  println("map:" + mapped.size + ":" + mapped.head + ":" + mapped.last)
  val filtered = q.filter(_ % 2 == 0)
  println("filter:" + filtered.size + ":" + filtered.head + ":" + filtered.last)

  // aggregate:
  val small = Queue(1, 2, 3, 4)
  println("folds:" + small.sum + ":" + small.product + ":" + small.foldLeft(10)(_ + _))
  println("reduce:" + q.reduce(_ + _) + ":" + q.min + ":" + q.max)
  println("preds:" + q.contains(5) + ":" + q.indexOf(5) + ":" + q.exists(_ > 7) + ":" + q.forall(_ > 0))
  println("findCount:" + q.find(_ > 5).getOrElse(-1) + ":" + q.count(_ > 5))

  // slicing:
  println("slice:" + q.take(3).size + ":" + q.drop(3).size + ":" + q.slice(2, 5).size)

  // mutate: enqueue / dequeue / clear
  val m = Queue(10, 20, 30)
  m.enqueue(40)
  m.enqueue(50)
  println("enq:" + m.size + ":" + m.head + ":" + m.last)

  val first = m.dequeue()
  println("deq:" + first + ":" + m.size + ":" + m.head + ":" + m.last)

  // enqueueAll
  val ea = Queue(1, 2)
  ea.enqueueAll(List(3, 4, 5))
  println("enqAll:" + ea.size + ":" + ea.head + ":" + ea.last)

  // clone independence
  val orig = Queue(1, 2, 3)
  val cloned = orig.clone()
  cloned.enqueue(99)
  println("clone:" + orig.size + ":" + cloned.size + ":" + cloned.last)

  // clear
  val toClear = Queue(1, 2, 3)
  toClear.clear()
  println("clear:" + toClear.isEmpty + ":" + toClear.size)

  // convert:
  println("convList:" + q.toList.length + ":" + q.toList.head + ":" + q.toList.last)
  println("convVec:" + q.toVector.size + ":" + q.toVector.head + ":" + q.toVector.last)
  println("convSet:" + q.toSet.size)
  println("mkString:" + Queue(1, 2, 3).mkString(","))
