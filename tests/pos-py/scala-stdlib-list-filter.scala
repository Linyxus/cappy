// Regression test for the lifted `noneIn_1` helper that
// `scala.collection.immutable.List#filterCommon` references on the
// `List` value class itself (lifted from a nested `def noneIn` inside
// `filter`/`filterNot`). Previously, `PyApplyStatic` rendering routed
// through `_scpy_mod_<class>_`, dropping these self-owned `@staticmethod`
// helpers. See `notes/issue-list-filter-nonein-missing.md`.

@main def scalaStdlibListFilter(): Unit =
  val xs = List(1, 2, 3, 4, 5, 6)
  println("filter:" + xs.filter(_ > 2).size + ":" + xs.filter(_ > 2).head)
  println("filterNot:" + xs.filterNot(_ > 2).size + ":" + xs.filterNot(_ > 2).head)
  // `partition` also routes through `filterCommon`.
  val (lo, hi) = xs.partition(_ < 4)
  println("partition:" + lo.size + ":" + hi.size + ":" + lo.head + ":" + hi.head)
