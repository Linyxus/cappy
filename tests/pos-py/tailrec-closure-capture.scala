// Regression guard: a closure created inside a `@tailrec` loop body
// must capture its free locals BY VALUE — Python's lexical-name
// capture would otherwise let the loop's later parameter rebinding
// leak into the deferred thunk, and reads of those locals at thunk-
// invocation time return the post-loop final values.
//
// `LazyList.reverseOnto` is the upstream witness:
//
//     @tailrec
//     private def reverseOnto[B >: A](tl: LazyList[B]): LazyList[B] =
//       if (isEmpty) tl
//       else tail.reverseOnto(newLL(eagerCons(head, tl)))
//
// `newLL(...)` wraps the `eagerCons(head, tl)` argument in a `() =>
// ...` thunk evaluated when the resulting lazy list is first forced.
// dotty's tail-call mini-phase rewrites `tail.reverseOnto(...)` into
// a labeled jump that overwrites the method's `this` and `tl` slots
// in place. The deferred thunk closes over those slots; with name-
// capture semantics, every thunk along the reversed chain sees the
// final (empty-tail) `this`, and `head` throws
// `NoSuchElementException("head of empty lazy list")` when forced.
//
// `tests/run/t153.scala` is the upstream shape that surfaced this.

@main def tailrecClosureCapture(): Unit =
  val xs = LazyList.range(1, 6).reverse
  println(xs.take(5).force)

  // Direct `.toList` round-trip — exercises a different forcing path
  // that also walks the reversed chain.
  println(LazyList.range(10, 14).reverse.toList)
