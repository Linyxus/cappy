// Regression test: `List(elems*)` with ≥16 literal arguments hits the
// builder fallback path (`prependedAll` over a `Seq.from(seq)`) which
// uses labeled blocks for its result. Previously the emitter rendered
// the labeled block as a Python conditional expression and silently
// dropped the body that bound `result`, producing
// `NameError: name 'result' is not defined`.
//
// Test only checks construction + non-indexing reads to isolate the
// PyLabeled hoist fix from unrelated gaps in `List.apply`/`drop`.
//
// See notes/issue-list-vector-large-literal-unbound-locals.md.

@main def scalaListLargeLiteral(): Unit =
  val xs = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
  println("len:" + xs.length)
  println("head:" + xs.head)
  println("nonempty:" + xs.nonEmpty)
