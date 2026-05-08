// Pins the `EmptyTuple` case-object identity under the
// Python-tuple lowering.
//
// The Scala JVM model treats `EmptyTuple` as a case-object
// singleton, so pattern matches against `case _: EmptyTuple` and
// `case EmptyTuple` desugar to a reference-equality check
// (`x eq EmptyTuple`). With our lowering, every empty Scala tuple
// at runtime must read from the SAME `_scpy_empty_tuple` singleton —
// otherwise the `is` check Python emits for `eq` would compare two
// freshly-constructed `_scpy_ScalaTuple()` instances and return
// False.
//
// Regression: `tests/run/tuple-typetests.scala` matched
// `case _: EmptyTuple` after constructing `Tuple()`. The two empty
// tuples were different objects, so the case missed and threw
// `MatchError`.

@main def tupleEmptySingleton(): Unit =
  // 1. `Tuple()` and `EmptyTuple` are the same instance.
  val a: Tuple  = Tuple()
  val b: Tuple  = EmptyTuple
  println(a eq b)

  // 2. `case _: EmptyTuple` matches a freshly-constructed empty tuple.
  def nonEmpty(x: Any): Boolean = x match
    case _: (_ *: _)  => true
    case _: EmptyTuple => false
  println(nonEmpty(Tuple()))
  println(nonEmpty(EmptyTuple))
  println(nonEmpty((1, 2, 3)))

  // 3. Empty tuples produced by element-wise helpers are also the
  //    singleton — `eq` against `EmptyTuple` succeeds.
  val t = (1, 2, 3)
  println(t.drop(3) eq EmptyTuple)
  val (_, dropped) = t.splitAt(3)
  println(dropped eq EmptyTuple)
  println(Tuple.fromArray(Array.emptyObjectArray) eq EmptyTuple)
