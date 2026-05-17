package scala.python.runtime

import scala.language.dynamics
import scala.python.{PyAny, PyDynamic, extern, name, native}

object PyDeque:
  @extern("collections")
  private object collections extends PyAny:
    @name("deque")
    def newDeque(): PyDynamic = native

  @extern("builtins")
  private object builtins extends PyAny:
    @name("len")
    def lengthOf(container: Any): Int = native

  def empty[T](): PyDeque[T] =
    new PyDeque[T](collections.newDeque())

final class PyDeque[T] private[runtime] (private val underlying: PyDynamic):
  def append(value: T): Unit =
    underlying.append(value)

  def appendLeft(value: T): Unit =
    underlying.appendleft(value)

  def pop(): T =
    underlying.pop().asInstanceOf[T]

  def popLeft(): T =
    underlying.popleft().asInstanceOf[T]

  def clear(): Unit =
    underlying.clear()

  def size(): Int =
    PyDeque.builtins.lengthOf(underlying)
