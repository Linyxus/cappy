package scala.python.runtime

import scala.python.native

/** A Scala-typed handle on a Python `list`.
 *
 *  At runtime, a `PyList[T]` value IS a Python `list` object — there
 *  is no Scala wrapper. Construction and every method on this class is
 *  intercepted at codegen and lowered to a Python list literal or one
 *  of the `_scpy_list_*` runtime helpers. The `= native` placeholder
 *  bodies typer-inline to a throw and are never reached.
 *
 *  The API mirrors Python's `list` API; method-name divergences are
 *  spelled out where the Scala name differs:
 *    - `update(i, v)` matches Scala's `xs(i) = v` desugaring.
 *    - `removeAt(i)` is Python's positional `pop(i)`; `remove(v)`
 *      removes the first occurrence of a value and returns whether one
 *      was found (Python raises `ValueError`).
 *    - `concat` / `extend` instead of `+` / `+=` (would clash with
 *      arithmetic on numeric element types). */
object PyList:
  /** Construct an empty `PyList`. Lowers to `[]` (Python list literal). */
  def empty[T](): PyList[T] = native

  /** Construct a `PyList` from a varargs sequence. Lowers to
   *  `[a, b, c]` (Python list literal). */
  def apply[T](elems: T*): PyList[T] = native

final class PyList[T] private ():
  // length / emptiness
  def size(): Int       = native
  def isEmpty: Boolean  = native
  def nonEmpty: Boolean = native

  // item access
  def apply(i: Int): T            = native
  def update(i: Int, v: T): Unit  = native

  // mutation
  def append(v: T): Unit              = native
  def prepend(v: T): Unit             = native
  def insert(i: Int, v: T): Unit      = native
  def removeAt(i: Int): T             = native
  def remove(v: T): Boolean           = native
  def clear(): Unit                   = native
  def extend(other: PyList[T]): Unit  = native

  // membership / search
  def contains(v: T): Boolean = native
  def indexOf(v: T): Int      = native
  def count(v: T): Int        = native

  // copy / concat
  def copy(): PyList[T]                       = native
  def concat(other: PyList[T]): PyList[T]     = native
  def slice(from: Int, until: Int): PyList[T] = native

  // reorder
  def sort(): Unit    = native
  def reverse(): Unit = native

  // iteration
  def iterator(): scala.collection.Iterator[T] = native
