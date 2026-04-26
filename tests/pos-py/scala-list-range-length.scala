// Regression for notes/issue-list-range-lazyhandle-none.md.
// `List.range(0, n)` builds a `NumericRange.Exclusive`; `Nil.prependedAll`
// then probes its `knownSize`, which forces the lazy `length`. That lazy
// val expanded to a `compareAndSet` on a per-instance `_lzyHandle` field
// that the Python backend used to leave at `None`. Same root bug
// reproduced via `LazyList.range`.

@main def scalaListRangeLength(): Unit =
  val xs = List.range(0, 4)
  println(xs.length)
  println(xs.head)
  println(xs.last)

  val ys = LazyList.range(0, 4)
  println(ys.length)
  println(ys.head)
  println(ys.last)
