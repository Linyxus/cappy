// Regression test for two lifted helpers on the `LazyList` value class
// previously dropped by `PyApplyStatic` rerouting:
//   - `appendHead_1` — lifted from `LazyList#addStringNoForce`, hit by
//     `mkString` directly on a `LazyList`. See
//     `notes/issue-lazylist-mkstring-appendhead-missing.md`.
//   - `appendedAll_anonfun_1` — lifted from `LazyList#appendedAll`,
//     deferred-evaluated by `LazyList ++ LazyList`. See
//     `notes/issue-lazylist-concat-appendedall-missing.md`.

@main def scalaStdlibLazyListOps(): Unit =
  // mkString directly on a LazyList — drives `addStringNoForce` ->
  // `LazyList$.appendHead_1(...)`.
  val ll = LazyList(1, 2, 3, 4, 5)
  println("mk:" + ll.mkString(","))

  // LazyList ++ LazyList — builds a deferred thunk that, when forced,
  // calls `LazyList$.appendedAll_anonfun_1(...)` (and then
  // `appendHead_1` again via toString/length paths).
  val cat = LazyList(1, 2) ++ LazyList(3, 4)
  println("len:" + cat.length)
  println("catStr:" + cat.mkString(","))
