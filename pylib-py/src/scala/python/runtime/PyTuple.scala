package scala.python.runtime

import scala.python.native

/** A Scala-typed handle on a bare Python `tuple`.
 *
 *  At runtime, a `PyTuple[T]` value IS a Python `tuple` object — there
 *  is no Scala wrapper, and it shares NO runtime identity with Scala's
 *  built-in `Tuple{N}` / `EmptyTuple` types. Scala tuples lower to
 *  `_scpy_ScalaTuple(...)` so they keep Scala-tuple semantics
 *  (string formatting, hashing, pattern destructuring); `PyTuple[T]`
 *  is intended for interop with Python APIs that expect a real
 *  immutable `tuple`.
 *
 *  Construction and every method are intercepted at codegen and
 *  lowered to a Python tuple literal or one of the `_scpy_pytuple_*`
 *  runtime helpers. The `= native` placeholder bodies are never
 *  reached. */
object PyTuple:
  /** Construct an empty `PyTuple`. Lowers to `()` (Python tuple literal). */
  def empty[T](): PyTuple[T] = native

  /** Construct a `PyTuple` from a varargs sequence. Lowers to
   *  `(a, b, c)` — or `(x,)` for a single element. */
  def apply[T](elems: T*): PyTuple[T] = native

final class PyTuple[T] private ():
  // length / emptiness
  def size(): Int       = native
  def isEmpty: Boolean  = native
  def nonEmpty: Boolean = native

  // item access
  def apply(i: Int): T = native

  // membership / search
  def contains(v: T): Boolean = native
  def indexOf(v: T): Int      = native
  def count(v: T): Int        = native

  // concat / slice
  def concat(other: PyTuple[T]): PyTuple[T]    = native
  def slice(from: Int, until: Int): PyTuple[T] = native

  // iteration
  def iterator(): scala.collection.Iterator[T] = native
