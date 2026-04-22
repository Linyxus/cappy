package java.util

import java.lang.Cloneable

class HashSet[E] private[util] (inner: HashMap[E, AnyRef])
    extends AbstractSet[E] with Set[E] with Cloneable with Serializable:

  private val innerKeySet = inner.keySet()

  def this(initialCapacity: Int, loadFactor: Float) =
    this(new HashMap[E, AnyRef](initialCapacity, loadFactor))

  def this(initialCapacity: Int) =
    this(new HashMap[E, AnyRef](initialCapacity))

  def this() =
    this(new HashMap[E, AnyRef]())

  def this(c: Collection[_ <: E]) =
    this(c.size())
    addAll(c)

  override def contains(value: Any): Boolean =
    inner.containsKey(value)

  override def remove(value: Any): Boolean =
    inner.remove(value) != null

  override def containsAll(c: Collection[_]): Boolean =
    innerKeySet.containsAll(c)

  override def removeAll(c: Collection[_]): Boolean =
    innerKeySet.removeAll(c)

  override def retainAll(c: Collection[_]): Boolean =
    innerKeySet.retainAll(c)

  override def add(value: E): Boolean =
    inner.put(value, HashSet.Present) == null

  override def addAll(c: Collection[_ <: E]): Boolean =
    val iter = c.iterator()
    var changed = false
    while iter.hasNext() do
      if add(iter.next()) then
        changed = true
    changed

  override def clear(): Unit =
    inner.clear()

  override def size(): Int =
    inner.size()

  def iterator(): Iterator[E] =
    innerKeySet.iterator()

object HashSet:
  private object Present
