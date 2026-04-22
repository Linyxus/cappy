package java.util.concurrent

import java.io.Serializable
import java.lang.{BoundsChecks, Cloneable}
import java.lang.{reflect => jlr}
import java.util.function.{Predicate, UnaryOperator}
import java.util.{ArrayList, Collection, Comparator, Iterator, List, ListIterator, Objects, RandomAccess}

import scala.python.runtime.PyThreading

class CopyOnWriteArrayList[E] private (private var inner: ArrayList[E])
    extends java.util.AbstractList[E] with RandomAccess with Cloneable with Serializable:

  private val lock = PyThreading.newRLock()

  def this() =
    this(new ArrayList[E]())

  def this(initialCapacity: Int) =
    this(new ArrayList[E](initialCapacity))

  def this(c: Collection[_ <: E]) =
    this(new ArrayList[E](c))

  def this(toCopyIn: Array[E]) =
    this(jlr.Array.getLength(toCopyIn))
    val length = jlr.Array.getLength(toCopyIn)
    var i = 0
    while i < length do
      inner.add(jlr.Array.get(toCopyIn, i).asInstanceOf[E])
      i += 1

  override def clone(): AnyRef =
    new CopyOnWriteArrayList[E](snapshot())

  def size(): Int =
    snapshot().size()

  override def isEmpty(): Boolean =
    snapshot().isEmpty()

  override def contains(o: Any): Boolean =
    snapshot().contains(o)

  def get(index: Int): E =
    snapshot().get(index)

  override def set(index: Int, element: E): E =
    lock.acquire()
    try
      val copy = new ArrayList[E](inner)
      val result = copy.set(index, element)
      inner = copy
      result
    finally
      lock.release()

  override def add(e: E): Boolean =
    lock.acquire()
    try
      val copy = new ArrayList[E](inner)
      copy.add(e)
      inner = copy
      true
    finally
      lock.release()

  override def add(index: Int, element: E): Unit =
    lock.acquire()
    try
      val copy = new ArrayList[E](inner)
      copy.add(index, element)
      inner = copy
    finally
      lock.release()

  override def remove(index: Int): E =
    lock.acquire()
    try
      val copy = new ArrayList[E](inner)
      val result = copy.remove(index)
      inner = copy
      result
    finally
      lock.release()

  override def remove(o: Any): Boolean =
    lock.acquire()
    try
      val copy = new ArrayList[E](inner)
      val index = copy.indexOf(o)
      if index < 0 then
        false
      else
        copy.remove(index)
        inner = copy
        true
    finally
      lock.release()

  override def clear(): Unit =
    lock.acquire()
    try
      inner = new ArrayList[E]()
    finally
      lock.release()

  override def indexOf(o: Any): Int =
    snapshot().indexOf(o)

  override def lastIndexOf(o: Any): Int =
    snapshot().lastIndexOf(o)

  def addIfAbsent(e: E): Boolean =
    lock.acquire()
    try
      val copy = new ArrayList[E](inner)
      if copy.contains(e) then
        false
      else
        copy.add(e)
        inner = copy
        true
    finally
      lock.release()

  override def containsAll(c: Collection[_]): Boolean =
    snapshot().containsAll(c)

  override def removeAll(c: Collection[_]): Boolean =
    lock.acquire()
    try
      val copy = new ArrayList[E](inner)
      var changed = false
      var index = 0
      while index < copy.size() do
        if c.contains(copy.get(index)) then
          copy.remove(index)
          changed = true
        else
          index += 1
      if changed then
        inner = copy
      changed
    finally
      lock.release()

  override def retainAll(c: Collection[_]): Boolean =
    lock.acquire()
    try
      val copy = new ArrayList[E](inner)
      var changed = false
      var index = 0
      while index < copy.size() do
        if !c.contains(copy.get(index)) then
          copy.remove(index)
          changed = true
        else
          index += 1
      if changed then
        inner = copy
      changed
    finally
      lock.release()

  def addAllAbsent(c: Collection[_ <: E]): Int =
    lock.acquire()
    try
      val copy = new ArrayList[E](inner)
      var added = 0
      val iter = c.iterator()
      while iter.hasNext() do
        val value = iter.next()
        if !copy.contains(value) then
          copy.add(value)
          added += 1
      if added > 0 then
        inner = copy
      added
    finally
      lock.release()

  override def addAll(c: Collection[_ <: E]): Boolean =
    addAll(size(), c)

  override def addAll(index: Int, c: Collection[_ <: E]): Boolean =
    lock.acquire()
    try
      val copy = new ArrayList[E](inner)
      val result = copy.addAll(index, c)
      if result then
        inner = copy
      result
    finally
      lock.release()

  override def removeIf(filter: Predicate[_ >: E]): Boolean =
    lock.acquire()
    try
      val copy = new ArrayList[E](inner)
      var changed = false
      var index = 0
      while index < copy.size() do
        if filter.test(copy.get(index)) then
          copy.remove(index)
          changed = true
        else
          index += 1
      if changed then
        inner = copy
      changed
    finally
      lock.release()

  override def replaceAll(operator: UnaryOperator[E]): Unit =
    lock.acquire()
    try
      val copy = new ArrayList[E](inner)
      var index = 0
      while index < copy.size() do
        copy.set(index, operator.apply(copy.get(index)))
        index += 1
      inner = copy
    finally
      lock.release()

  override def sort(c: Comparator[_ >: E]): Unit =
    lock.acquire()
    try
      val copy = new ArrayList[E](inner)
      copy.sort(c)
      inner = copy
    finally
      lock.release()

  override def iterator(): Iterator[E] =
    listIterator()

  override def listIterator(): ListIterator[E] =
    listIterator(0)

  override def listIterator(index: Int): ListIterator[E] =
    val snap = snapshot()
    BoundsChecks.checkIndexInclusive(index, snap.size())
    new SnapshotListIterator(snap, index)

  override def subList(fromIndex: Int, toIndex: Int): List[E] =
    val snap = snapshot()
    BoundsChecks.checkStartEnd(fromIndex, toIndex, snap.size())
    snap.subList(fromIndex, toIndex)

  override def equals(o: Any): Boolean =
    snapshot().equals(o)

  override def hashCode(): Int =
    snapshot().hashCode()

  override def toString(): String =
    snapshot().toString()

  private def snapshot(): ArrayList[E] =
    inner

  private final class SnapshotListIterator(snapshot: ArrayList[E], startIndex: Int) extends ListIterator[E]:
    private var index = startIndex

    def hasNext(): Boolean =
      index < snapshot.size()

    def next(): E =
      if !hasNext() then
        throw new java.util.NoSuchElementException()
      val value = snapshot.get(index)
      index += 1
      value

    def hasPrevious(): Boolean =
      index > 0

    def previous(): E =
      if !hasPrevious() then
        throw new java.util.NoSuchElementException()
      index -= 1
      snapshot.get(index)

    def nextIndex(): Int =
      index

    def previousIndex(): Int =
      index - 1

    override def remove(): Unit =
      throw new UnsupportedOperationException()

    def set(e: E): Unit =
      throw new UnsupportedOperationException()

    def add(e: E): Unit =
      throw new UnsupportedOperationException()
