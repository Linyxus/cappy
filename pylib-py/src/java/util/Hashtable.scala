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

  def size(): Int = this.synchronized {
    inner.size()
  }

  def isEmpty(): Boolean = this.synchronized {
    inner.isEmpty()
  }

  def keys(): Enumeration[K] = this.synchronized {
    new Hashtable.IteratorEnumeration[K](keySet().iterator())
  }

  def elements(): Enumeration[V] = this.synchronized {
    new Hashtable.IteratorEnumeration[V](values().iterator())
  }

  def contains(value: Any): Boolean = this.synchronized {
    containsValue(value)
  }

  def containsValue(value: Any): Boolean = this.synchronized {
    inner.containsValue(value)
  }

  def containsKey(key: Any): Boolean = this.synchronized {
    inner.containsKey(key)
  }

  def get(key: Any): V = this.synchronized {
    inner.get(key)
  }

  def put(key: K, value: V): V = this.synchronized {
    inner.put(key, value)
  }

  def remove(key: Any): V = this.synchronized {
    inner.remove(key)
  }

  def putAll(m: Map[? <: K, ? <: V]): Unit =
    this.synchronized {
      inner.putAll(m)
    }

  def clear(): Unit =
    this.synchronized {
      inner.clear()
    }

  override def clone(): AnyRef = this.synchronized {
    new Hashtable[K, V](this)
  }

  override def toString(): String = this.synchronized {
    inner.toString()
  }

  def keySet(): Set[K] = this.synchronized {
    inner.keySet()
  }

  def entrySet(): Set[Map.Entry[K, V]] = this.synchronized {
    inner.entrySet()
  }

  def values(): Collection[V] = this.synchronized {
    inner.values()
  }

  override def equals(other: Any): Boolean = this.synchronized {
    inner.equals(other)
  }

  override def hashCode(): Int = this.synchronized {
    inner.hashCode()
  }

object Hashtable:
  private final class IteratorEnumeration[A](iter: Iterator[A]) extends Enumeration[A]:
    def hasMoreElements(): Boolean =
      iter.hasNext()

    def nextElement(): A =
      iter.next()
