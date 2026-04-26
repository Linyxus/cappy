// Regression: `List.collect` calls `pf.applyOrElse(elem, partialNotApplied)`
// where `partialNotApplied` is the singleton `Function1` returned by
// `scala.collection.immutable.List.partialNotApplied()`. Inside
// `applyOrElse` the `default.apply__Ljava_lang_Object__Ljava_lang_Object`
// dispatches to that anonymous Function1's apply method. Before the fix
// the apply was DCE'd because no static call site visible to PyReachability
// referenced it (only the runtime `Function1.apply` virtual dispatch did,
// which used to short-circuit on runtime-provided receivers). See
// notes/issue-list-collect-applyorelse-default.md.

@main def scalaListCollect(): Unit =
  // 1. Int list — primitive specialization in play.
  val xs = List(1, 2, 3, 4, 5, 6, 7, 8)
  val sumOfBig = xs.collect { case i if i > 5 => i * 10 }.sum
  println("intSum:" + sumOfBig)

  // 2. String list — boxed apply, no specialization.
  val words = List("alpha", "beta", "gamma", "delta")
  val firsts = words.collect { case s if s.length > 4 => s.charAt(0) }
  println("firsts:" + firsts.mkString(","))

  // 3. Mixed: `collect` with no matches returns an empty list.
  val empty = xs.collect { case i if i > 100 => i }
  println("empty:" + empty.size + ":" + empty.isEmpty)

  // 4. Pattern with both guard and arithmetic.
  val doubledEvens = xs.collect { case i if i % 2 == 0 => i * 2 }
  println("doubled:" + doubledEvens.mkString(","))

  // 5. Tuple destructuring in pattern.
  val pairs = List(("a", 1), ("b", 2), ("c", 3))
  val sumValues = pairs.collect { case (_, v) if v > 1 => v }.sum
  println("tupleSum:" + sumValues)
