package scala.python.runtime

import scala.python.native

/** A Scala-typed handle on a Python `dict`.
 *
 *  At runtime, a `PyMap[K, V]` value IS a Python `dict` object — there
 *  is no Scala wrapper. Construction and every method on this class is
 *  intercepted at codegen and lowered to a Python dict literal or one
 *  of the `_scpy_dict_*` runtime helpers. The `= native` placeholder
 *  bodies typer-inline to a throw and are never reached.
 *
 *  The API mirrors Python's `dict` API; method-name divergences are
 *  spelled out where the Scala name differs:
 *    - `update(k, v)` matches Scala's `m(k) = v` desugaring; the batch
 *      form is `updateAll`.
 *    - `delete(k)` for `del m[k]` (avoids the `del` soft keyword).
 *    - `setDefault` / `popItem` are camelCase.
 *    - `merged` / `mergeInPlace` instead of `|` / `|=` (would clash
 *      with bitwise OR on integer keys).
 *
 *  `apply` and `pop` raise Python `KeyError` on a missing key. Use
 *  `get` / `getOrElse` / `popOrElse` for null-tolerant variants. */
object PyMap:
  /** Construct an empty `PyMap`. Lowers to `{}` (Python dict literal). */
  def empty[K, V](): PyMap[K, V] = native

final class PyMap[K, V] private ():
  // length / emptiness
  def size(): Int       = native
  def isEmpty: Boolean  = native
  def nonEmpty: Boolean = native

  // item access
  def apply(key: K): V                 = native
  def get(key: K): V | Null            = native
  def getOrElse(key: K, default: V): V = native

  // mutation
  def update(key: K, value: V): Unit      = native
  def delete(key: K): Unit                = native
  def setDefault(key: K, default: V): V   = native
  def pop(key: K): V                      = native
  def popOrElse(key: K, default: V): V    = native
  def popItem(): (K, V)                   = native
  def clear(): Unit                       = native
  def updateAll(other: PyMap[K, V]): Unit = native

  // membership
  def contains(key: K): Boolean = native

  // copy / merge
  def copy(): PyMap[K, V]                     = native
  def merged(other: PyMap[K, V]): PyMap[K, V] = native
  def mergeInPlace(other: PyMap[K, V]): Unit  = native

  // views (snapshot iterators)
  def keys():   scala.collection.Iterator[K]      = native
  def values(): scala.collection.Iterator[V]      = native
  def items():  scala.collection.Iterator[(K, V)] = native
