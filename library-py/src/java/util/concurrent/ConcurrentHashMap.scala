package java.util.concurrent

import java.io.Serializable
import java.util.function.{BiConsumer, Consumer}
import java.util.{AbstractCollection, AbstractMap, AbstractSet, Collection, Collections, Enumeration, HashMap, Iterator, Map, NullRejectingHashMap, Objects, Set}
import java.util.Objects.requireNonNull

import scala.python.runtime.PyThreading

class ConcurrentHashMap[K, V] private (initialCapacity: Int, loadFactor: Float)
    extends AbstractMap[K, V] with ConcurrentMap[K, V] with Serializable:
  self =>

  private val inner = new NullRejectingHashMap[K, V](initialCapacity, loadFactor)
  private val lock = PyThreading.newRLock()

  def this() =
    this(HashMap.DEFAULT_INITIAL_CAPACITY, HashMap.DEFAULT_LOAD_FACTOR)

  def this(initialCapacity: Int) =
    this(initialCapacity, HashMap.DEFAULT_LOAD_FACTOR)

  def this(initialMap: java.util.Map[_ <: K, _ <: V]) =
    this(initialMap.size())
    putAll(initialMap)

  def this(initialCapacity: Int, loadFactor: Float, concurrencyLevel: Int) =
    this(initialCapacity, loadFactor)

  override def size(): Int =
    lock.acquire()
    try inner.size()
    finally lock.release()

  override def isEmpty(): Boolean =
    lock.acquire()
    try inner.isEmpty()
    finally lock.release()

  override def get(key: Any): V =
    lock.acquire()
    try inner.get(key)
    finally lock.release()

  override def containsKey(key: Any): Boolean =
    lock.acquire()
    try inner.containsKey(key)
    finally lock.release()

  override def containsValue(value: Any): Boolean =
    lock.acquire()
    try inner.containsValue(value)
    finally lock.release()

  override def put(key: K, value: V): V =
    lock.acquire()
    try inner.put(key, value)
    finally lock.release()

  override def remove(key: Any): V =
    lock.acquire()
    try inner.remove(key)
    finally lock.release()

  override def clear(): Unit =
    lock.acquire()
    try inner.clear()
    finally lock.release()

  override def putAll(m: Map[_ <: K, _ <: V]): Unit =
    lock.acquire()
    try inner.putAll(m)
    finally lock.release()

  override def keySet(): ConcurrentHashMap.KeySetView[K, V] =
    new ConcurrentHashMap.KeySetView[K, V](this, null.asInstanceOf[V])

  def keySet(mappedValue: V): ConcurrentHashMap.KeySetView[K, V] =
    new ConcurrentHashMap.KeySetView[K, V](this, requireNonNull(mappedValue))

  override def values(): Collection[V] =
    new ValuesView

  override def entrySet(): Set[Map.Entry[K, V]] =
    new EntrySetView

  override def putIfAbsent(key: K, value: V): V =
    lock.acquire()
    try inner.putIfAbsent(key, value)
    finally lock.release()

  override def remove(key: Any, value: Any): Boolean =
    lock.acquire()
    try inner.remove(key, value)
    finally lock.release()

  override def replace(key: K, oldValue: V, newValue: V): Boolean =
    lock.acquire()
    try inner.replace(key, oldValue, newValue)
    finally lock.release()

  override def replace(key: K, value: V): V =
    lock.acquire()
    try inner.replace(key, value)
    finally lock.release()

  def forEach(parallelismThreshold: Long, action: BiConsumer[_ >: K, _ >: V]): Unit =
    val iter = snapshotEntries().iterator()
    while iter.hasNext() do
      val entry = iter.next()
      action.accept(entry.getKey(), entry.getValue())

  def forEachKey(parallelismThreshold: Long, action: Consumer[_ >: K]): Unit =
    val iter = newKeyIterator()
    while iter.hasNext() do
      action.accept(iter.next())

  def forEachValue(parallelismThreshold: Long, action: Consumer[_ >: V]): Unit =
    val iter = newValueIterator()
    while iter.hasNext() do
      action.accept(iter.next())

  def contains(value: Any): Boolean =
    containsValue(value)

  def keys(): Enumeration[K] =
    Collections.enumeration(keySet())

  def elements(): Enumeration[V] =
    Collections.enumeration(values())

  private[concurrent] def removeKey(key: Any): Boolean =
    remove(key) != null

  private[concurrent] def snapshotKeys(): java.util.ArrayList[K] =
    val keys = new java.util.ArrayList[K]()
    val iter = snapshotEntries().iterator()
    while iter.hasNext() do
      keys.add(iter.next().getKey())
    keys

  private[concurrent] def newKeyIterator(): Iterator[K] =
    new SnapshotKeyIterator(snapshotKeys())

  private[concurrent] def newValueIterator(): Iterator[V] =
    new SnapshotValueIterator(snapshotEntries())

  private def newEntryIterator(): Iterator[Map.Entry[K, V]] =
    new SnapshotEntryIterator(snapshotEntries())

  private def snapshotEntries(): java.util.ArrayList[ConcurrentHashMap.EntrySnapshot[K, V]] =
    lock.acquire()
    try
      val snapshot = new java.util.ArrayList[ConcurrentHashMap.EntrySnapshot[K, V]](inner.size())
      val iter = inner.entrySet().iterator()
      while iter.hasNext() do
        val entry = iter.next()
        snapshot.add(new ConcurrentHashMap.EntrySnapshot[K, V](entry.getKey(), entry.getValue(), self))
      snapshot
    finally lock.release()

  private final class EntrySetView extends AbstractSet[Map.Entry[K, V]]:
    def iterator(): Iterator[Map.Entry[K, V]] =
      newEntryIterator()

    def size(): Int =
      self.size()

    override def contains(o: Any): Boolean =
      o match
        case entry: Map.Entry[?, ?] =>
          self.containsKey(entry.getKey()) &&
            Objects.equals(self.get(entry.getKey()), entry.getValue())
        case _ =>
          false

    override def remove(o: Any): Boolean =
      o match
        case entry: Map.Entry[?, ?] =>
          self.remove(entry.getKey(), entry.getValue())
        case _ =>
          false

    override def clear(): Unit =
      self.clear()

  private final class ValuesView extends AbstractCollection[V]:
    def iterator(): Iterator[V] =
      newValueIterator()

    def size(): Int =
      self.size()

    override def contains(value: Any): Boolean =
      self.containsValue(value)

    override def clear(): Unit =
      self.clear()

  private final class SnapshotEntryIterator(
      snapshot: java.util.ArrayList[ConcurrentHashMap.EntrySnapshot[K, V]]
  ) extends Iterator[Map.Entry[K, V]]:
    private val inner = snapshot.iterator()
    private var current: ConcurrentHashMap.EntrySnapshot[K, V] | Null = null

    def hasNext(): Boolean =
      inner.hasNext()

    def next(): Map.Entry[K, V] =
      val entry = inner.next()
      current = entry
      entry

    override def remove(): Unit =
      val entry = current
      if entry == null then
        throw new IllegalStateException()
      self.remove(entry.getKey())
      current = null

  private final class SnapshotKeyIterator(snapshot: java.util.ArrayList[K]) extends Iterator[K]:
    private val inner = snapshot.iterator()
    private var current: K | Null = null

    def hasNext(): Boolean =
      inner.hasNext()

    def next(): K =
      val key = inner.next()
      current = key
      key

    override def remove(): Unit =
      val key = current
      if key == null then
        throw new IllegalStateException()
      self.remove(key)
      current = null

  private final class SnapshotValueIterator(
      snapshot: java.util.ArrayList[ConcurrentHashMap.EntrySnapshot[K, V]]
  ) extends Iterator[V]:
    private val inner = snapshot.iterator()
    private var current: ConcurrentHashMap.EntrySnapshot[K, V] | Null = null

    def hasNext(): Boolean =
      inner.hasNext()

    def next(): V =
      val entry = inner.next()
      current = entry
      entry.getValue()

    override def remove(): Unit =
      val entry = current
      if entry == null then
        throw new IllegalStateException()
      self.remove(entry.getKey())
      current = null

object ConcurrentHashMap:
  final class EntrySnapshot[K, V] private[concurrent] (
      private val key0: K,
      private var value0: V,
      private val owner: ConcurrentHashMap[K, V]
  ) extends Map.Entry[K, V] with Serializable:
    def getKey(): K =
      key0

    def getValue(): V =
      value0

    def setValue(value: V): V =
      val oldValue = owner.put(key0, value)
      value0 = value
      oldValue

    override def equals(o: Any): Boolean =
      o match
        case other: Map.Entry[?, ?] =>
          Objects.equals(key0, other.getKey()) &&
            Objects.equals(value0, other.getValue())
        case _ =>
          false

    override def hashCode(): Int =
      Objects.hashCode(key0) ^ Objects.hashCode(value0)

    override def toString(): String =
      "" + key0 + "=" + value0

  class KeySetView[K, V] private[concurrent] (
      owner: ConcurrentHashMap[K, V],
      defaultValue: V
  ) extends AbstractSet[K] with Serializable:
    def getMappedValue(): V =
      defaultValue

    def iterator(): Iterator[K] =
      owner.newKeyIterator()

    def size(): Int =
      owner.size()

    override def isEmpty(): Boolean =
      owner.isEmpty()

    override def contains(o: Any): Boolean =
      owner.containsKey(o)

    override def remove(o: Any): Boolean =
      owner.removeKey(o)

    override def add(e: K): Boolean =
      if defaultValue == null then
        throw new UnsupportedOperationException()
      owner.putIfAbsent(e, defaultValue) == null

    override def addAll(c: Collection[_ <: K]): Boolean =
      val iter = c.iterator()
      var changed = false
      while iter.hasNext() do
        changed = add(iter.next()) || changed
      changed

    override def clear(): Unit =
      owner.clear()

  def newKeySet[K](): KeySetView[K, Boolean] =
    newKeySet[K](HashMap.DEFAULT_INITIAL_CAPACITY)

  def newKeySet[K](initialCapacity: Int): KeySetView[K, Boolean] =
    new ConcurrentHashMap[K, Boolean](initialCapacity).keySet(true)
