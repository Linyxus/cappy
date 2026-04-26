// Regression: `ArrayDeque[Int].map(_ + 1)` used to RecursionError because
// the bundled Python class hierarchy emitted `class IndexedSeqView_Id(
// SeqView_Id, IndexedSeqView)`. Python C3 placed `SeqView_Id` before
// `IndexedSeqView`, so `super().iterator__()` from a subclass resolved
// to the wrong base and looped through the underlying ArrayDeque again.
// Fixed by emitting traits before the Scala superclass in the Python
// bases tuple, matching Scala's linearization rule.
//
// See notes/issue-arraydeque-map-class-walk-recursion.md.

import scala.collection.mutable.ArrayDeque

@main def scalaArrayDequeMap(): Unit =
  val d = new ArrayDeque[Int](4)
  var i = 0
  while i < 4 do
    d += i
    i += 1
  val mapped = d.map(_ + 1)
  println(mapped.size)
  val it = mapped.iterator
  while it.hasNext do
    print(it.next())
    print(":")
  println("done")
