package java.util

import java.lang.Cloneable

class IdentityHashMap[K, V] private (
    inner: HashMap[IdentityHashMap.IdentityBox[K], V],
    internal: Boolean
) extends AbstractMap[K, V] with Map[K, V] with Serializable with Cloneable:
  self =>

  import IdentityHashMap._

  def this(expectedMaxSize: Int) =
    this(
      new HashMap[IdentityHashMap.IdentityBox[K], V](
        expectedMaxSize,
        HashMap.DEFAULT_LOAD_FACTOR
      ),
      internal = true
    )

  def this() =
    this(HashMap.DEFAULT_INITIAL_CAPACITY)

  def this(initialMap: java.util.Map[? <: K, ? <: V]) =
    this(initialMap.size())
    putAll(initialMap)

  override def clear(): Unit =
    inner.clear()

  override def clone(): AnyRef =
    new IdentityHashMap[K, V](
      inner.clone().asInstanceOf[HashMap[IdentityBox[K], V]],
      internal = true
    )

  override def containsKey(key: Any): Boolean =
    inner.containsKey(new IdentityBox(key))

  override def containsValue(value: Any): Boolean =
    val iter = inner.values().iterator()
    while iter.hasNext() do
      if same(iter.next(), value) then
        return true
    false

  override def get(key: Any): V =
    inner.get(new IdentityBox(key))

  override def isEmpty(): Boolean =
    inner.isEmpty()

  override def put(key: K, value: V): V =
    inner.put(new IdentityBox(key), value)

  override def remove(key: Any): V =
    inner.remove(new IdentityBox(key))

  override def size(): Int =
    inner.size()

  override def values(): Collection[V] =
    new Values

  override def keySet(): Set[K] =
    new KeySet

  override def entrySet(): Set[Map.Entry[K, V]] =
    new EntrySet

  private final class Values extends AbstractCollection[V]:
    def iterator(): Iterator[V] =
      inner.values().iterator()

    def size(): Int =
      self.size()

    override def contains(value: Any): Boolean =
      containsValue(value)

    override def remove(value: Any): Boolean =
      val iter = iterator()
      while iter.hasNext() do
        if same(iter.next(), value) then
          iter.remove()
          return true
      false

    override def removeAll(c: Collection[?]): Boolean =
      val iter = c.iterator()
      var changed = false
      while iter.hasNext() do
        if remove(iter.next()) then
          changed = true
      changed

    override def retainAll(c: Collection[?]): Boolean =
      val iter = iterator()
      var changed = false
      while iter.hasNext() do
        val elem = iter.next()
        if !findSame(elem, c) then
          iter.remove()
          changed = true
      changed

    override def clear(): Unit =
      self.clear()

  private final class KeySet extends AbstractSet[K]:
    def iterator(): Iterator[K] =
      new Iterator[K]:
        private val iter = inner.keySet().iterator()

        def hasNext(): Boolean =
          iter.hasNext()

        def next(): K =
          iter.next().inner

        override def remove(): Unit =
          iter.remove()

    def size(): Int =
      self.size()

    override def contains(value: Any): Boolean =
      containsKey(value)

    override def remove(value: Any): Boolean =
      val hadKey = contains(value)
      if hadKey then
        self.remove(value)
      hadKey

    override def removeAll(c: Collection[?]): Boolean =
      val iter = iterator()
      var changed = false
      while iter.hasNext() do
        if findSame(iter.next(), c) then
          iter.remove()
          changed = true
      changed

    override def retainAll(c: Collection[?]): Boolean =
      val iter = iterator()
      var changed = false
      while iter.hasNext() do
        if !findSame(iter.next(), c) then
          iter.remove()
          changed = true
      changed

    override def clear(): Unit =
      self.clear()

  private final class EntrySet extends AbstractSet[Map.Entry[K, V]]:
    def iterator(): Iterator[Map.Entry[K, V]] =
      new Iterator[Map.Entry[K, V]]:
        private val iter = inner.entrySet().iterator()

        def hasNext(): Boolean =
          iter.hasNext()

        def next(): Map.Entry[K, V] =
          new MapEntry[K, V](iter.next())

        override def remove(): Unit =
          iter.remove()

    def size(): Int =
      inner.size()

    override def contains(value: Any): Boolean =
      value match
        case entry: Map.Entry[?, ?] =>
          val thatKey = entry.getKey()
          containsKey(thatKey) && same(self.get(thatKey), entry.getValue())
        case _ =>
          false

    override def remove(value: Any): Boolean =
      value match
        case entry: Map.Entry[?, ?] =>
          val thatKey = entry.getKey()
          if containsKey(thatKey) && same(self.get(thatKey), entry.getValue()) then
            self.remove(thatKey)
            true
          else
            false
        case _ =>
          false

    override def clear(): Unit =
      inner.clear()

object IdentityHashMap:
  private final class IdentityBox[+K](val inner: K):
    override def equals(other: Any): Boolean =
      other match
        case other: IdentityBox[?] =>
          same(inner, other.inner)
        case _ =>
          false

    override def hashCode(): Int =
      System.identityHashCode(inner)

  @inline private def same(v1: Any, v2: Any): Boolean =
    v1.asInstanceOf[AnyRef] eq v2.asInstanceOf[AnyRef]

  private def findSame(elem: Any, c: Collection[?]): Boolean =
    val iter = c.iterator()
    while iter.hasNext() do
      if same(elem, iter.next()) then
        return true
    false

  private final class MapEntry[K, V](entry: Map.Entry[IdentityBox[K], V]) extends Map.Entry[K, V]:
    override def equals(other: Any): Boolean =
      other match
        case other: Map.Entry[?, ?] =>
          same(getKey(), other.getKey()) && same(getValue(), other.getValue())
        case _ =>
          false

    def getKey(): K =
      entry.getKey().inner

    def getValue(): V =
      entry.getValue()

    override def hashCode(): Int =
      entry.getKey().hashCode() ^ System.identityHashCode(entry.getValue())

    def setValue(value: V): V =
      entry.setValue(value)

    override def toString(): String =
      "" + getKey() + "=" + getValue()
