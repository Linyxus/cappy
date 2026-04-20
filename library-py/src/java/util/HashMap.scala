/*
 * Adapted from Scala.js, but backed by a small Python dict of bucket heads
 * instead of a manually-sized bucket array.
 */

package java.util

import java.lang.Cloneable
import java.lang.BoundsChecks
import scala.python.runtime.PyDict

class HashMap[K, V](initialCapacity: Int, loadFactor: Float)
    extends AbstractMap[K, V] with Serializable with Cloneable {
  self =>

  import HashMap._

  BoundsChecks.checkCapacity(initialCapacity)
  if loadFactor <= 0.0f then
    throw new IllegalArgumentException("loadFactor <= 0.0")

  private var buckets = PyDict.empty[Int, Node[K, V]]()
  private var contentSize = 0
  private[util] var eldest: Node[K, V] | Null = null
  private[util] var youngest: Node[K, V] | Null = null

  def this() =
    this(HashMap.DEFAULT_INITIAL_CAPACITY, HashMap.DEFAULT_LOAD_FACTOR)

  def this(initialCapacity: Int) =
    this(initialCapacity, HashMap.DEFAULT_LOAD_FACTOR)

  def this(m: Map[_ <: K, _ <: V]) =
    this(m.size())
    putAll(m)

  protected[util] def newNode(
      key: K,
      hash: Int,
      value: V,
      nextInBucket: HashMap.Node[K, V] | Null
  ): HashMap.Node[K, V] =
    new HashMap.Node[K, V](key, hash, value, nextInBucket)

  protected[util] def nodeWasAccessed(node: HashMap.Node[K, V]): Unit = ()

  protected[util] def nodeWasAdded(node: HashMap.Node[K, V]): Unit =
    appendOrdered(node)

  protected[util] def nodeWasRemoved(node: HashMap.Node[K, V]): Unit =
    unlinkOrdered(node)

  private[util] final def moveToTail(node: HashMap.Node[K, V]): Unit =
    if (node.asInstanceOf[AnyRef] ne youngest.asInstanceOf[AnyRef]) then
      unlinkOrdered(node)
      appendOrdered(node)

  private[util] final def eldestNode(): HashMap.Node[K, V] | Null =
    eldest

  private[util] final def removeNode(node: HashMap.Node[K, V] | Null): Unit =
    if node != null then
      removeKnownNode(node)

  override def size(): Int =
    contentSize

  override def isEmpty(): Boolean =
    contentSize == 0

  override def containsKey(key: Any): Boolean =
    findNode(key) != null

  override def containsValue(value: Any): Boolean =
    var node = eldest
    while node != null do
      if Objects.equals(value, node.value) then
        return true
      node = node.younger
    false

  override def get(key: Any): V =
    val node = findNode(key)
    if node == null then
      null.asInstanceOf[V]
    else
      nodeWasAccessed(node)
      node.value

  override def put(key: K, value: V): V =
    put0(key, value, ifAbsent = false)

  override def putIfAbsent(key: K, value: V): V =
    put0(key, value, ifAbsent = true)

  override def remove(key: Any): V =
    val removed = remove0(key)
    if removed == null then null.asInstanceOf[V] else removed.value

  override def clear(): Unit =
    buckets.clear()
    contentSize = 0
    eldest = null
    youngest = null

  override def keySet(): Set[K] =
    new KeySet

  override def values(): Collection[V] =
    new Values

  override def entrySet(): Set[Map.Entry[K, V]] =
    new EntrySet

  override def clone(): AnyRef =
    new HashMap[K, V](this)

  private def put0(key: K, value: V, ifAbsent: Boolean): V =
    val hash = computeHash(key)
    var node = buckets.get(hash)
    while node != null do
      if node.hash == hash && Objects.equals(key, node.key) then
        nodeWasAccessed(node)
        val oldValue = node.value
        if !ifAbsent || oldValue == null then
          node.value = value
        return oldValue
      node = node.nextInBucket

    val head = buckets.get(hash)
    val created = newNode(key, hash, value, head)
    buckets.update(hash, created)
    contentSize += 1
    nodeWasAdded(created)
    null.asInstanceOf[V]

  private def findNode(key: Any): HashMap.Node[K, V] | Null =
    val hash = computeHash(key)
    var node = buckets.get(hash)
    while node != null do
      if node.hash == hash && Objects.equals(key, node.key) then
        return node
      node = node.nextInBucket
    null

  private def remove0(key: Any): HashMap.Node[K, V] | Null =
    val hash = computeHash(key)
    var previous: HashMap.Node[K, V] | Null = null
    var node = buckets.get(hash)
    while node != null do
      if node.hash == hash && Objects.equals(key, node.key) then
        unlinkBucket(hash, previous, node)
        nodeWasRemoved(node)
        contentSize -= 1
        node.nextInBucket = null
        node.older = null
        node.younger = null
        return node
      previous = node
      node = node.nextInBucket
    null

  private def removeKnownNode(target: HashMap.Node[K, V]): Unit =
    val hash = target.hash
    var previous: HashMap.Node[K, V] | Null = null
    var node = buckets.get(hash)
    while node != null do
      if node.asInstanceOf[AnyRef] eq target.asInstanceOf[AnyRef] then
        unlinkBucket(hash, previous, node)
        nodeWasRemoved(node)
        contentSize -= 1
        node.nextInBucket = null
        node.older = null
        node.younger = null
        return
      previous = node
      node = node.nextInBucket

  private def unlinkBucket(
      hash: Int,
      previous: HashMap.Node[K, V] | Null,
      current: HashMap.Node[K, V]
  ): Unit =
    if previous == null then
      if current.nextInBucket == null then
        buckets.remove(hash)
      else
        buckets.update(hash, current.nextInBucket.asInstanceOf[HashMap.Node[K, V]])
    else
      previous.nextInBucket = current.nextInBucket

  private def appendOrdered(node: HashMap.Node[K, V]): Unit =
    val older = youngest
    if older == null then
      eldest = node
    else
      older.younger = node
    node.older = older
    node.younger = null
    youngest = node

  private def unlinkOrdered(node: HashMap.Node[K, V]): Unit =
    val older = node.older
    val younger = node.younger
    if older == null then
      eldest = younger
    else
      older.younger = younger
    if younger == null then
      youngest = older
    else
      younger.older = older

  private abstract class AbstractHashMapIterator[A] extends Iterator[A]:
    private var nextNode = eldest
    private var lastNode: HashMap.Node[K, V] | Null = null

    protected def extract(node: HashMap.Node[K, V]): A

    def hasNext(): Boolean =
      nextNode != null

    def next(): A =
      if !hasNext() then
        throw new NoSuchElementException("next on empty iterator")
      val node = nextNode.asInstanceOf[HashMap.Node[K, V]]
      lastNode = node
      nextNode = node.younger
      extract(node)

    override def remove(): Unit =
      if lastNode == null then
        throw new IllegalStateException("next must be called before remove")
      val node = lastNode
      lastNode = null
      removeNode(node)

  private final class NodeIterator extends AbstractHashMapIterator[HashMap.Node[K, V]]:
    protected def extract(node: HashMap.Node[K, V]): HashMap.Node[K, V] =
      node

  private final class KeyIterator extends AbstractHashMapIterator[K]:
    protected def extract(node: HashMap.Node[K, V]): K =
      node.key

  private final class ValueIterator extends AbstractHashMapIterator[V]:
    protected def extract(node: HashMap.Node[K, V]): V =
      node.value

  private final class EntrySet extends AbstractSet[Map.Entry[K, V]]:
    def iterator(): Iterator[Map.Entry[K, V]] =
      new NodeIterator().asInstanceOf[Iterator[Map.Entry[K, V]]]

    def size(): Int =
      contentSize

    override def clear(): Unit =
      self.clear()

  private final class KeySet extends AbstractSet[K]:
    def iterator(): Iterator[K] =
      new KeyIterator

    def size(): Int =
      contentSize

    override def contains(value: Any): Boolean =
      self.containsKey(value)

    override def remove(value: Any): Boolean =
      val hadKey = self.containsKey(value)
      if hadKey then
        self.remove(value)
      hadKey

    override def clear(): Unit =
      self.clear()

  private final class Values extends AbstractCollection[V]:
    def iterator(): Iterator[V] =
      new ValueIterator

    def size(): Int =
      contentSize

    override def contains(value: Any): Boolean =
      self.containsValue(value)

    override def clear(): Unit =
      self.clear()
}

object HashMap:
  private[util] final val DEFAULT_INITIAL_CAPACITY = 16
  private[util] final val DEFAULT_LOAD_FACTOR = 0.75f

  @inline private def improveHash(originalHash: Int): Int =
    originalHash ^ (originalHash >>> 16)

  @inline private[util] def unimproveHash(improvedHash: Int): Int =
    improveHash(improvedHash)

  @inline private[util] def computeHash(key: Any): Int =
    if key == null then 0 else improveHash(Objects.hashCode(key))

  class Node[K, V](
      val key: K,
      val hash: Int,
      var value: V,
      var nextInBucket: HashMap.Node[K, V] | Null
  )
      extends Map.Entry[K, V]:
    private[util] var older: HashMap.Node[K, V] | Null = null
    private[util] var younger: HashMap.Node[K, V] | Null = null

    def getKey(): K =
      key

    def getValue(): V =
      value

    def setValue(newValue: V): V =
      val oldValue = value
      value = newValue
      oldValue

    override def equals(other: Any): Boolean =
      other match
        case entry: Map.Entry[?, ?] =>
          Objects.equals(getKey(), entry.getKey()) &&
          Objects.equals(getValue(), entry.getValue())
        case _ =>
          false

    override def hashCode(): Int =
      HashMap.unimproveHash(hash) ^ Objects.hashCode(value)

    override def toString(): String =
      "" + getKey() + "=" + getValue()
