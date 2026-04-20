package java.util

import java.util.Objects.requireNonNull

private[util] class NullRejectingHashMap[K, V](initialCapacity: Int, loadFactor: Float)
    extends HashMap[K, V](initialCapacity, loadFactor):

  def this() =
    this(HashMap.DEFAULT_INITIAL_CAPACITY, HashMap.DEFAULT_LOAD_FACTOR)

  def this(initialCapacity: Int) =
    this(initialCapacity, HashMap.DEFAULT_LOAD_FACTOR)

  def this(m: Map[_ <: K, _ <: V]) =
    this(m.size())
    putAll(m)

  override protected[util] def newNode(
      key: K,
      hash: Int,
      value: V,
      nextInBucket: HashMap.Node[K, V] | Null
  ): HashMap.Node[K, V] =
    new NullRejectingHashMap.Node[K, V](key, hash, value, nextInBucket)

  override def get(key: Any): V =
    super.get(requireNonNull(key))

  override def containsKey(key: Any): Boolean =
    super.containsKey(requireNonNull(key))

  override def put(key: K, value: V): V =
    super.put(requireNonNull(key), requireNonNull(value))

  override def putIfAbsent(key: K, value: V): V =
    super.putIfAbsent(requireNonNull(key), requireNonNull(value))

  override def putAll(m: Map[_ <: K, _ <: V]): Unit =
    val iter = m.entrySet().iterator()
    while iter.hasNext() do
      val entry = iter.next()
      put(entry.getKey(), entry.getValue())

  override def remove(key: Any): V =
    super.remove(requireNonNull(key))

  override def remove(key: Any, value: Any): Boolean =
    super.remove(requireNonNull(key), requireNonNull(value))

  override def replace(key: K, oldValue: V, newValue: V): Boolean =
    super.replace(requireNonNull(key), requireNonNull(oldValue), requireNonNull(newValue))

  override def replace(key: K, value: V): V =
    super.replace(requireNonNull(key), requireNonNull(value))

  override def containsValue(value: Any): Boolean =
    super.containsValue(requireNonNull(value))

  override def clone(): AnyRef =
    new NullRejectingHashMap[K, V](this)

object NullRejectingHashMap:
  private final class Node[K, V](key: K, hash: Int, value: V, nextInBucket: HashMap.Node[K, V] | Null)
      extends HashMap.Node[K, V](key, hash, value, nextInBucket):

    override def setValue(newValue: V): V =
      super.setValue(requireNonNull(newValue))
