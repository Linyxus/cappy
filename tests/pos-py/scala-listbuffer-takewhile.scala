// Regression: ListBuffer.takeWhile / .dropWhile previously raised
// NoSuchElementException("next on empty iterator") because the code
// generator hoisted the side-effecting block on the right-hand side
// of `&&` in `Iterator.takeWhile.hasNext` out of the short-circuit,
// running `tail.next()` unconditionally.

import scala.collection.mutable.ListBuffer

@main def scalaListBufferTakeWhile(): Unit =
  val a = ListBuffer(1, 2, 3, 6, 7, 8)

  // takeWhile: predicate matches a prefix
  val tw = a.takeWhile(_ < 5)
  println("tw-prefix:" + tw.size + ":" + tw.mkString(","))

  // takeWhile: predicate matches all
  val twAll = a.takeWhile(_ < 100)
  println("tw-all:" + twAll.size + ":" + twAll.mkString(","))

  // takeWhile: predicate matches none
  val twNone = a.takeWhile(_ < 0)
  println("tw-none:" + twNone.size)

  // dropWhile: predicate matches a prefix
  val dw = a.dropWhile(_ < 5)
  println("dw-prefix:" + dw.size + ":" + dw.mkString(","))

  // dropWhile: predicate matches all
  val dwAll = a.dropWhile(_ < 100)
  println("dw-all:" + dwAll.size)

  // dropWhile: predicate matches none
  val dwNone = a.dropWhile(_ < 0)
  println("dw-none:" + dwNone.size + ":" + dwNone.mkString(","))

  // Same on Vector and List (also routes through Iterator.takeWhile/dropWhile)
  val v = Vector(1, 2, 3, 6, 7, 8)
  println("vec:" + v.takeWhile(_ < 5).size + ":" + v.dropWhile(_ < 5).size)

  val l = List(1, 2, 3, 6, 7, 8)
  println("lst:" + l.takeWhile(_ < 5).size + ":" + l.dropWhile(_ < 5).size)
