// Regression test for the genClosure receiver bug. `LazyList.mapImpl`
// captures `this` into a `Function0` thunk that calls
// `this.mapImpl_anonfun_1(f)`, where `f` is in the closure env. Before
// the fix, GenPython.genClosure pulled env[0] (= f) out as the receiver,
// emitting `f.mapImpl_anonfun_1()` and producing
// `AttributeError: mapImpl__anonfun_1__…` at runtime.
// See notes/issue-lazylist-map-anonfun-missing.md.

@main def scalaLazyListMapClosure(): Unit =
  val ll = LazyList(0, 1, 2, 3)
  ll.length
  println(ll.map(_ + 1).sum)
