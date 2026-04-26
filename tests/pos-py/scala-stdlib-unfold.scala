// Coverage for the `unfold` factory across `LazyList`, `List`, and `Vector`.
// Closure-heavy: the unfold step function returns an `Option[(A, S)]`, which
// exercises tuple/optional packaging and the `_scpy_FnN` boxed apply path.

@main def scalaStdlibUnfold(): Unit =
  // 1. List.unfold — finite step builder.
  val listSeq = List.unfold(0)(s => if s < 5 then Some((s * 10, s + 1)) else None)
  println("list:" + listSeq.size + ":" + listSeq.head + ":" + listSeq.last + ":" + listSeq.sum)

  // 2. Vector.unfold — same step shape.
  val vecSeq = Vector.unfold(1)(s => if s <= 16 then Some((s, s * 2)) else None)
  println("vec:" + vecSeq.size + ":" + vecSeq.head + ":" + vecSeq.last + ":" + vecSeq.sum)

  // 3. LazyList.unfold — infinite-by-default; force a finite prefix.
  val ll = LazyList.unfold(0)(s => Some((s, s + 1))).take(5).toList
  println("lazy:" + ll.size + ":" + ll.head + ":" + ll.last + ":" + ll.sum)

  // 4. List.unfold consumed via map+filter — closure on closure.
  val derived = List.unfold(1)(s => if s <= 6 then Some((s, s + 1)) else None)
    .map(_ * 2)
    .filter(_ > 4)
  println("derived:" + derived.size + ":" + derived.head + ":" + derived.last)

  // 5. Empty unfold — start state immediately yields None.
  val emptyList = List.unfold(0)(_ => None: Option[(Int, Int)])
  println("empty:" + emptyList.size + ":" + emptyList.isEmpty)
