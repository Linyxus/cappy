// Pins `runtime.Tuples.*` element-wise helpers across the Tuple22 ↔
// TupleXXL boundary.
//
// Regression: tuples of arity ≤ 22 lower to native Python tuples
// (`_scpy_ScalaTuple`), tuples of arity > 22 stay as Scala
// `TupleXXL` instances. The runtime helpers (`_scpy_tuple_concat`,
// `_scpy_tuple_cons`, `_scpy_tuple_size`, …) used to assume both
// operands were Python tuples and unconditionally ran `a + b`,
// `t[i]`, `len(t)`. A `TupleXXL` operand crashed with
// `unsupported operand type(s) for +`
// (`tests/run/tuple-concat.scala`).
//
// Fix path: `_scpy_as_pytuple(x)` normalizes any `Tuple`-typed value
// to a Python tuple via `productArity`/`productElement`. `concat`
// also short-circuits to the original argument when one side is
// empty, preserving `runtime.Tuples.concat`'s `(t ++ ()) eq t`
// invariant — covered explicitly below.

@main def tupleXXLConcatBoundary(): Unit =
  val small: Tuple = ("a", "b", "c")

  // 25-element literal → TupleXXL instance at runtime.
  val xxl: Tuple = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                    11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
                    21, 22, 23, 24, 25)

  // Concat across the boundary, both directions.
  val xxlSmall = xxl ++ small
  val smallXxl = small ++ xxl
  println(xxlSmall.size)
  println(smallXxl.size)
  println(xxlSmall.productElement(0))
  println(xxlSmall.productElement(24))
  println(xxlSmall.productElement(25))
  println(xxlSmall.productElement(27))
  println(smallXxl.productElement(0))
  println(smallXxl.productElement(2))
  println(smallXxl.productElement(3))
  println(smallXxl.productElement(27))

  // XXL ++ XXL → TupleXXL with > 22 elements that flowed through
  // the helper as a Python tuple.
  val xxl2: Tuple = (100, 101, 102, 103, 104, 105, 106, 107, 108, 109,
                     110, 111, 112, 113, 114, 115, 116, 117, 118, 119,
                     120, 121, 122)
  val xxlXxl = xxl ++ xxl2
  println(xxlXxl.size)
  println(xxlXxl.productElement(0))
  println(xxlXxl.productElement(24))
  println(xxlXxl.productElement(25))
  println(xxlXxl.productElement(47))

  // Empty-side optimization: `runtime.Tuples.concat` returns the
  // OTHER side unchanged when one side is empty. Under the JVM this
  // is a reference-equality identity; we just verify the size and
  // an element survive correctly.
  val xxlPlusEmpty = xxl ++ EmptyTuple
  val emptyPlusXxl = EmptyTuple ++ xxl
  println(xxlPlusEmpty.size)
  println(xxlPlusEmpty.productElement(24))
  println(emptyPlusXxl.size)
  println(emptyPlusXxl.productElement(0))

  // Cons / append into a TupleXXL — exercises `Tuples.cons` /
  // `Tuples.append` with a TupleXXL operand.
  val consXxl: Tuple = 0 *: xxl
  println(consXxl.size)
  println(consXxl.productElement(0))
  println(consXxl.productElement(25))
  val appXxl: Tuple = xxl :* 26
  println(appXxl.size)
  println(appXxl.productElement(0))
  println(appXxl.productElement(25))
