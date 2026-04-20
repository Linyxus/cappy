package scala.python.runtime

import scala.language.dynamics
import scala.python.{PyAny, PyDynamic, extern, name, native}

object PyDict:
  @extern("builtins")
  private[runtime] object builtins extends PyAny:
    @name("dict")
    def newDict(): PyDynamic = native

    @name("len")
    def lengthOf(container: Any): Int = native

    @name("list")
    def toList(iter: Any): PyDynamic = native

  @extern("operator")
  private[runtime] object operator extends PyAny:
    @name("contains")
    def containsKey(container: Any, key: Any): Boolean = native

    @name("setitem")
    def setItem(container: Any, key: Any, value: Any): Unit = native

    @name("delitem")
    def deleteItem(container: Any, key: Any): Unit = native

    @name("getitem")
    def getItem(container: Any, index: Any): Any = native

  def empty[K, V](): PyDict[K, V] =
    new PyDict[K, V](builtins.newDict())

/** Thin facade over Python's native `dict`. Used as the bucket-index
 *  backing store by `java.util.HashMap` and friends; L5+ consumers may
 *  also use it directly.
 *
 *  Iteration views (`keys`, `values`, `items`) are materialised to
 *  Python `list` snapshots — callers walk via `.length` + `.get(i)`
 *  rather than Python's iterator protocol, keeping the bridge
 *  SAM/closure-free.
 *
 *  Caveat: a materialised view is a snapshot. Mutating the underlying
 *  dict afterwards doesn't update the view; re-fetch after mutation
 *  to match JDK's fail-fast convention. */
final class PyDict[K, V] private[runtime] (private val underlying: PyDynamic):
  def contains(key: K): Boolean =
    PyDict.operator.containsKey(underlying, key)

  def get(key: K): V | Null =
    underlying.get(key, null).asInstanceOf[V | Null]

  def update(key: K, value: V): Unit =
    PyDict.operator.setItem(underlying, key, value)

  def remove(key: K): Unit =
    if contains(key) then
      PyDict.operator.deleteItem(underlying, key)

  def clear(): Unit =
    underlying.clear()

  def size(): Int =
    PyDict.builtins.lengthOf(underlying)

  def keys(): PyDictIter[K] =
    new PyDictIter[K](PyDict.builtins.toList(underlying.keys()))

  def values(): PyDictIter[V] =
    new PyDictIter[V](PyDict.builtins.toList(underlying.values()))

  def items(): PyDictEntryIter[K, V] =
    new PyDictEntryIter[K, V](
      PyDict.builtins.toList(underlying.keys()),
      PyDict.builtins.toList(underlying.values())
    )

/** Index-random-access handle over a materialised Python `list`
 *  snapshot. See `PyDict.keys` / `PyDict.values`. */
final class PyDictIter[T] private[runtime] (private val backing: PyDynamic):
  def length: Int =
    PyDict.builtins.lengthOf(backing)

  def get(index: Int): T =
    PyDict.operator.getItem(backing, index).asInstanceOf[T]

/** Paired-index view over key / value snapshot lists. */
final class PyDictEntryIter[K, V] private[runtime] (
    private val keyList: PyDynamic,
    private val valueList: PyDynamic
):
  def length: Int =
    PyDict.builtins.lengthOf(keyList)

  def key(index: Int): K =
    PyDict.operator.getItem(keyList, index).asInstanceOf[K]

  def value(index: Int): V =
    PyDict.operator.getItem(valueList, index).asInstanceOf[V]
