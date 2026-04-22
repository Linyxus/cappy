package scala.python.runtime

import scala.language.dynamics
import scala.python.{PyAny, PyDynamic, extern, name, native}

object PyIdDict:
  @extern("builtins")
  private object builtins extends PyAny:
    @name("dict")
    def newDict(): PyDynamic = native

    @name("id")
    def pyId(value: Any): Long = native

    @name("len")
    def lengthOf(container: Any): Int = native

  @extern("operator")
  private object operator extends PyAny:
    @name("contains")
    def containsKey(container: Any, key: Any): Boolean = native

    @name("setitem")
    def setItem(container: Any, key: Any, value: Any): Unit = native

    @name("delitem")
    def deleteItem(container: Any, key: Any): Unit = native

  private[runtime] def slotOf(value: Any): Long =
    if value == null then 0L else builtins.pyId(value)

  def empty[K, V](): PyIdDict[K, V] =
    new PyIdDict[K, V](builtins.newDict())

final class PyIdDict[K, V] private[runtime] (private val underlying: PyDynamic):
  private def slotOf(key: K): Long =
    PyIdDict.slotOf(key)

  def contains(key: K): Boolean =
    PyIdDict.operator.containsKey(underlying, slotOf(key))

  def get(key: K): V | Null =
    underlying.get(slotOf(key), null).asInstanceOf[V | Null]

  def update(key: K, value: V): Unit =
    PyIdDict.operator.setItem(underlying, slotOf(key), value)

  def remove(key: K): Unit =
    if contains(key) then
      PyIdDict.operator.deleteItem(underlying, slotOf(key))

  def clear(): Unit =
    underlying.clear()

  def size(): Int =
    PyIdDict.builtins.lengthOf(underlying)
