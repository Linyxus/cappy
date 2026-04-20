package java.util

import java.lang.Cloneable

class Hashtable[K, V] private (inner: NullRejectingHashMap[K, V])
    extends Dictionary[K, V] with Map[K, V] with Cloneable with Serializable:

  def this() =
    this(new NullRejectingHashMap[K, V]())

  def this(initialCapacity: Int) =
    this(new NullRejectingHashMap[K, V](initialCapacity))

  def this(initialCapacity: Int, loadFactor: Float) =
    this(new NullRejectingHashMap[K, V](initialCapacity, loadFactor))

  def this(t: Map[? <: K, ? <: V]) =
    this(new NullRejectingHashMap[K, V](t))

  def size(): Int =
    inner.size()

  def isEmpty(): Boolean =
    inner.isEmpty()

  def keys(): Enumeration[K] =
    new Hashtable.IteratorEnumeration[K](keySet().iterator())

  def elements(): Enumeration[V] =
    new Hashtable.IteratorEnumeration[V](values().iterator())

  def contains(value: Any): Boolean =
    containsValue(value)

  def containsValue(value: Any): Boolean =
    inner.containsValue(value)

  def containsKey(key: Any): Boolean =
    inner.containsKey(key)

  def get(key: Any): V =
    inner.get(key)

  def put(key: K, value: V): V =
    inner.put(key, value)

  def remove(key: Any): V =
    inner.remove(key)

  def putAll(m: Map[? <: K, ? <: V]): Unit =
    inner.putAll(m)

  def clear(): Unit =
    inner.clear()

  override def clone(): AnyRef =
    new Hashtable[K, V](this)

  override def toString(): String =
    inner.toString()

  def keySet(): Set[K] =
    inner.keySet()

  def entrySet(): Set[Map.Entry[K, V]] =
    inner.entrySet()

  def values(): Collection[V] =
    inner.values()

  override def equals(other: Any): Boolean =
    inner.equals(other)

  override def hashCode(): Int =
    inner.hashCode()

object Hashtable:
  private final class IteratorEnumeration[A](iter: Iterator[A]) extends Enumeration[A]:
    def hasMoreElements(): Boolean =
      iter.hasNext()

    def nextElement(): A =
      iter.next()
