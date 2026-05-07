// Cross-tuple interop with a Python facade. After lowering Scala
// tuples to native Python tuples, two flavours coexist at runtime:
//
//   1. Scala-emitted tuples — instances of `_scpy_ScalaTuple`, a tag-
//      only `tuple` subclass. They format Scala-style `(1,x,true)` (no
//      spaces, no quotes) and hash with JVM `MurmurHash3.caseClassHash`
//      so a dict key built in Scala matches one built via `*:` cons.
//   2. Foreign tuples returned by `@extern` Python facades (numpy
//      `arr.shape`, `math.frexp`, …) — bare `tuple` instances. They
//      keep Python's native repr `(2, 3)` and Python's `__hash__` so
//      interop with foreign Python code stays bit-faithful.
//
// Both kinds compare equal under content-based `tuple.__eq__`, so a
// Scala tuple constructed with the right shape passes round-trip
// against a foreign tuple of the same content.

import scala.python.*

@extern("numpy")
object np extends PyDynamic

@main def numpyTupleInterop(): Unit =
  // -- Scala tuple → numpy: a Scala-emitted Tuple2 is acceptable
  //    wherever numpy expects a shape. The runtime value is a
  //    `_scpy_ScalaTuple` (subclass of Python `tuple`), so numpy's
  //    C-level shape parser sees a normal tuple.
  val grid = np.zeros((2, 3))
  println(grid.ndim)
  println(grid.size)

  // -- numpy → Scala: `.shape` returns a foreign Python tuple. It
  //    prints Python-style `(2, 3)` (with the post-comma space) — the
  //    `_scpy_to_str` formatter does NOT take Scala-style branches for
  //    bare `tuple` instances.
  val shape = grid.shape
  println(shape)

  // -- Scala-style formatting for a `_scpy_ScalaTuple`. Same content,
  //    different repr — drives home that the tag distinguishes them.
  val scalaShape: (Int, Int) = (2, 3)
  println(scalaShape)

  // -- Pass a literal-built Scala tuple straight into numpy and ask
  //    numpy to compare its own shape attribute against it. The
  //    boolean comes back as a numpy `bool_`; let `_scpy_to_str` use
  //    Python's repr for it.
  println(np.array_equal(shape, scalaShape))

  // -- Multi-arity. A 3-tuple shape:
  val cube = np.zeros((2, 3, 4))
  println(cube.ndim)
  println(cube.shape)

  // -- Tuple vs. integer-shape 1-D case. Scala 1-tuples format with
  //    a trailing comma `(2,)` — same as Python's bare-tuple repr,
  //    but routed through `_scpy_tuple_to_str` rather than `str(t)`.
  val line: Tuple1[Int] = Tuple1(2)
  println(line)

  // -- Tuple operations on a Scala tuple still work after the lowering:
  //    `_N`, `swap`, `hashCode`, and Tuple-runtime-method intercepts.
  println(scalaShape._1)
  println(scalaShape._2)
  println(scalaShape.swap)
  println((1, 2, 3).hashCode == (1, 2, 3).hashCode)

  // -- `Tuple.fromIArray(t.toIArray)` round-trips through the Tuples
  //    helpers. The result is a `_scpy_ScalaTuple` again.
  val rebuilt = Tuple.fromIArray(scalaShape.toIArray)
  println(rebuilt == scalaShape)

  // -- isInstanceOf works against the generated tuple-class names. The
  //    runtime check inspects `len(value)` rather than a real Python
  //    base class, so a foreign tuple with the right length is
  //    accepted by `Tuple{N}` checks too — intentional duck-typing on
  //    the `Tuple{N}` axis (callers reaching for the static type
  //    already accepted the Python-tuple-as-Scala-tuple equivalence).
  println(scalaShape.isInstanceOf[(Int, Int)])
