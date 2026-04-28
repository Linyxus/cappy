package java.util

import java.lang.Cloneable

// v1: strong-key wrapper around HashMap. JVM WeakHashMap evicts entries when
// the key is GC'd, but Python's GC differs and most callers use WeakHashMap
// as a cache where eventual cleanup is not load-bearing. See
// notes/issue-pylib-weakhashmap-port.md.
class WeakHashMap[K, V] private (inner: HashMap[K, V], internal: Boolean)
    extends AbstractMap[K, V] with Map[K, V] with Serializable with Cloneable:
  self =>

  def this(initialCapacity: Int, loadFactor: Float) =
    this(new HashMap[K, V](initialCapacity, loadFactor), internal = true)

  def this(initialCapacity: Int) =
    this(initialCapacity, HashMap.DEFAULT_LOAD_FACTOR)

  def this() =
    this(HashMap.DEFAULT_INITIAL_CAPACITY)

  def this(m: Map[? <: K, ? <: V]) =
    this(m.size())
    putAll(m)

  override def clear(): Unit =
    inner.clear()

  override def containsKey(key: Any): Boolean =
    inner.containsKey(key)

  override def containsValue(value: Any): Boolean =
    inner.containsValue(value)

  override def get(key: Any): V =
    inner.get(key)

  override def isEmpty(): Boolean =
    inner.isEmpty()

  override def put(key: K, value: V): V =
    inner.put(key, value)

  override def remove(key: Any): V =
    inner.remove(key)

  override def size(): Int =
    inner.size()

  override def keySet(): Set[K] =
    inner.keySet()

  override def values(): Collection[V] =
    inner.values()

  override def entrySet(): Set[Map.Entry[K, V]] =
    inner.entrySet()
