// Regression: `Set.intersect(other)` is implemented as `filter(other)`
// because `Set` extends `A => Boolean`. After erasure that goes through
// `Function1.apply__Ljava_lang_Object__Ljava_lang_Object` on the
// receiver `Set`. Before the reachability fix the boxed unspecialized
// `apply` on `SetOps` was DCE'd. See
// notes/issue-set-intersect-apply-object-missing.md.

import scala.collection.{immutable, mutable}

@main def scalaSetIntersect(): Unit =
  // 1. Immutable HashSet & immutable HashSet.
  val a = immutable.HashSet(1, 2, 3, 4)
  val b = immutable.HashSet(3, 4, 5, 6)
  val ab = a.intersect(b)
  println("immut:" + ab.size + ":" + ab.toList.sorted.mkString(","))

  // 2. Mutable HashSet & mutable HashSet.
  val ma = mutable.HashSet(1, 2, 3)
  val mb = mutable.HashSet(2, 3, 4)
  val mab = ma.intersect(mb)
  println("mut:" + mab.size + ":" + mab.toList.sorted.mkString(","))

  // 3. Empty intersection.
  val empty = immutable.HashSet(1, 2).intersect(immutable.HashSet(3, 4))
  println("empty:" + empty.size + ":" + empty.isEmpty)

  // 4. Identity intersection.
  val same = a.intersect(a)
  println("self:" + same.size + ":" + same.toList.sorted.mkString(","))

  // 5. `&` operator (alias for intersect).
  val amp = a & b
  println("ampAlias:" + amp.size)

  // 6. String element type — exercises the same boxed apply path.
  val sa = immutable.HashSet("a", "b", "c")
  val sb = immutable.HashSet("b", "c", "d")
  val sab = sa.intersect(sb)
  println("strs:" + sab.size + ":" + sab.toList.sorted.mkString(","))
