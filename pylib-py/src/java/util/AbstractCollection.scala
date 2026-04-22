/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package java.util

import java.lang.{reflect => jlr}

abstract class AbstractCollection[E] protected () extends Collection[E] {
  def iterator(): Iterator[E]
  def size(): Int

  def isEmpty(): Boolean = size() == 0

  def contains(o: Any): Boolean = {
    val iter = iterator()
    while (iter.hasNext()) {
      if (Objects.equals(o, iter.next()))
        return true
    }
    false
  }

  def toArray(): Array[AnyRef] =
    toArray(new Array[AnyRef](size()))

  def toArray[T <: AnyRef](a: Array[T]): Array[T] = {
    val toFill: Array[T] =
      if (a.length >= size()) a
      else jlr.Array.newInstance(a.getClass().getComponentType(), size()).asInstanceOf[Array[T]]

    val iter = iterator()
    var index = 0
    while (index < size()) {
      toFill(index) = iter.next().asInstanceOf[T]
      index += 1
    }
    if (toFill.length > size())
      toFill(size()) = null.asInstanceOf[T]
    toFill
  }

  def add(e: E): Boolean =
    throw new UnsupportedOperationException()

  def remove(o: Any): Boolean = {
    val iter = iterator()
    while (iter.hasNext()) {
      if (Objects.equals(iter.next(), o)) {
        iter.remove()
        return true
      }
    }
    false
  }

  def containsAll(c: Collection[_]): Boolean = {
    val iter = c.iterator()
    while (iter.hasNext()) {
      if (!contains(iter.next()))
        return false
    }
    true
  }

  def addAll(c: Collection[_ <: E]): Boolean = {
    val iter = c.iterator()
    var changed = false
    while (iter.hasNext()) {
      if (add(iter.next()))
        changed = true
    }
    changed
  }

  def removeAll(c: Collection[_]): Boolean = {
    val iter = iterator()
    var changed = false
    while (iter.hasNext()) {
      if (c.contains(iter.next())) {
        iter.remove()
        changed = true
      }
    }
    changed
  }

  def retainAll(c: Collection[_]): Boolean = {
    val iter = iterator()
    var changed = false
    while (iter.hasNext()) {
      if (!c.contains(iter.next())) {
        iter.remove()
        changed = true
      }
    }
    changed
  }

  def clear(): Unit = {
    val iter = iterator()
    while (iter.hasNext()) {
      iter.next()
      iter.remove()
    }
  }

  override def toString(): String = {
    val iter = iterator()
    var result = "["
    var first = true
    while (iter.hasNext()) {
      if (first)
        first = false
      else
        result += ", "
      result += String.valueOf(iter.next())
    }
    result + "]"
  }
}
