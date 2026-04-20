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

object AbstractMap {

  private def entryEquals[K, V](entry: Map.Entry[K, V], other: Any): Boolean = {
    other match {
      case other: Map.Entry[_, _] =>
        Objects.equals(entry.getKey(), other.getKey()) &&
        Objects.equals(entry.getValue(), other.getValue())
      case _ => false
    }
  }

  private def entryHashCode[K, V](entry: Map.Entry[K, V]): Int =
    Objects.hashCode(entry.getKey()) ^ Objects.hashCode(entry.getValue())

  class SimpleEntry[K, V](private var key: K, private var value: V)
      extends Map.Entry[K, V] with Serializable {

    def this(entry: Map.Entry[_ <: K, _ <: V]) =
      this(entry.getKey(), entry.getValue())

    def getKey(): K = key

    def getValue(): V = value

    def setValue(value: V): V = {
      val oldValue = this.value
      this.value = value
      oldValue
    }

    override def equals(o: Any): Boolean =
      entryEquals(this, o)

    override def hashCode(): Int =
      entryHashCode(this)

    override def toString(): String =
      "" + getKey() + "=" + getValue()
  }

  class SimpleImmutableEntry[K, V](key: K, value: V) extends Map.Entry[K, V] with Serializable {

    def this(entry: Map.Entry[_ <: K, _ <: V]) =
      this(entry.getKey(), entry.getValue())

    def getKey(): K = key

    def getValue(): V = value

    def setValue(value: V): V =
      throw new UnsupportedOperationException()

    override def equals(o: Any): Boolean =
      entryEquals(this, o)

    override def hashCode(): Int =
      entryHashCode(this)

    override def toString(): String =
      "" + getKey() + "=" + getValue()
  }
}

abstract class AbstractMap[K, V] protected () extends java.util.Map[K, V] {
  self =>

  def size(): Int = entrySet().size()

  def isEmpty(): Boolean = size() == 0

  def containsValue(value: Any): Boolean = {
    val iter = entrySet().iterator()
    while (iter.hasNext()) {
      if (Objects.equals(value, iter.next().getValue()))
        return true
    }
    false
  }

  def containsKey(key: Any): Boolean = {
    val iter = entrySet().iterator()
    while (iter.hasNext()) {
      if (Objects.equals(key, iter.next().getKey()))
        return true
    }
    false
  }

  def get(key: Any): V = {
    val iter = entrySet().iterator()
    while (iter.hasNext()) {
      val entry = iter.next()
      if (Objects.equals(key, entry.getKey()))
        return entry.getValue()
    }
    null.asInstanceOf[V]
  }

  def put(key: K, value: V): V =
    throw new UnsupportedOperationException()

  def remove(key: Any): V = {
    val iter = entrySet().iterator()
    while (iter.hasNext()) {
      val entry = iter.next()
      if (Objects.equals(key, entry.getKey())) {
        val value = entry.getValue()
        iter.remove()
        return value
      }
    }
    null.asInstanceOf[V]
  }

  def putAll(m: Map[_ <: K, _ <: V]): Unit = {
    val iter = m.entrySet().iterator()
    while (iter.hasNext()) {
      val entry = iter.next()
      put(entry.getKey(), entry.getValue())
    }
  }

  def clear(): Unit =
    entrySet().clear()

  def keySet(): Set[K] = {
    new AbstractSet[K] {
      override def size(): Int = self.size()

      def iterator(): Iterator[K] = {
        new Iterator[K] {
          val iter = entrySet().iterator()

          def hasNext(): Boolean = iter.hasNext()

          def next(): K = iter.next().getKey()

          override def remove(): Unit = iter.remove()
        }
      }
    }
  }

  def values(): Collection[V] = {
    new AbstractCollection[V] {
      override def size(): Int = self.size()

      def iterator(): Iterator[V] = {
        new Iterator[V] {
          val iter = entrySet().iterator()

          def hasNext(): Boolean = iter.hasNext()

          def next(): V = iter.next().getValue()

          override def remove(): Unit = iter.remove()
        }
      }
    }
  }

  def entrySet(): Set[Map.Entry[K, V]]

  override def equals(o: Any): Boolean = {
    if (o.asInstanceOf[AnyRef] eq this) true
    else {
      o match {
        case m: Map[_, _] =>
          if (self.size() != m.size()) {
            false
          } else {
            val iter = entrySet().iterator()
            while (iter.hasNext()) {
              val entry = iter.next()
              if (!Objects.equals(m.get(entry.getKey()), entry.getValue()))
                return false
            }
            true
          }
        case _ => false
      }
    }
  }

  override def hashCode(): Int = {
    val iter = entrySet().iterator()
    var hash = 0
    while (iter.hasNext()) {
      hash += iter.next().hashCode()
    }
    hash
  }

  override def toString(): String = {
    var result = "{"
    var first = true
    val iter = entrySet().iterator()
    while (iter.hasNext()) {
      val entry = iter.next()
      if (first)
        first = false
      else
        result += ", "
      result = result + entry.getKey() + "=" + entry.getValue()
    }
    result + "}"
  }
}
