package java.util.concurrent

import java.io.Serializable
import java.lang.Cloneable
import java.util.{AbstractSet, ArrayList, Collection, Comparator, Iterator, NavigableSet, NoSuchElementException, SortedSet}
import java.util.Objects.requireNonNull

import scala.python.runtime.PyThreading

class ConcurrentSkipListSet[E] private (
    private val internalComparator: Comparator[_ >: E],
    private val exposedComparator: Comparator[_ >: E] | Null
) extends AbstractSet[E] with NavigableSet[E] with Cloneable with Serializable:
  self =>

  private val lock = PyThreading.newRLock()
  private val inner = new ArrayList[E]()

  def this() =
    this(ConcurrentSkipListSet.defaultComparator[E](), null)

  def this(comparator: Comparator[_ >: E]) =
    this(ConcurrentSkipListSet.selectComparator(comparator), comparator)

  def this(collection: Collection[_ <: E]) =
    this()
    addAll(collection)

  def this(sortedSet: SortedSet[E]) =
    this(sortedSet.comparator())
    addAll(sortedSet)

  override def clone(): ConcurrentSkipListSet[E] =
    val cloned = new ConcurrentSkipListSet[E](internalComparator, exposedComparator)
    val snapshot = snapshotValues()
    var index = 0
    while index < snapshot.size() do
      cloned.inner.add(snapshot.get(index))
      index += 1
    cloned

  def size(): Int =
    lock.acquire()
    try inner.size()
    finally lock.release()

  override def isEmpty(): Boolean =
    lock.acquire()
    try inner.isEmpty()
    finally lock.release()

  override def contains(o: Any): Boolean =
    if o == null then false
    else
      lock.acquire()
      try findIndex(o) >= 0
      finally lock.release()

  override def add(e: E): Boolean =
    val value = requireNonNull(e)
    lock.acquire()
    try
      val index = findIndex(value)
      if index >= 0 then
        false
      else
        inner.add(insertionPoint(index), value)
        true
    finally lock.release()

  override def remove(o: Any): Boolean =
    val value = requireNonNull(o)
    lock.acquire()
    try
      val index = findIndex(value)
      if index >= 0 then
        inner.remove(index)
        true
      else
        false
    finally lock.release()

  override def clear(): Unit =
    lock.acquire()
    try inner.clear()
    finally lock.release()

  def iterator(): Iterator[E] =
    new SnapshotIterator(snapshotValues(), descending = false)

  def descendingIterator(): Iterator[E] =
    new SnapshotIterator(snapshotValues(), descending = true)

  override def removeAll(c: Collection[_]): Boolean =
    lock.acquire()
    try
      var changed = false
      var index = 0
      while index < inner.size() do
        if c.contains(inner.get(index)) then
          inner.remove(index)
          changed = true
        else
          index += 1
      changed
    finally lock.release()

  def lower(e: E): E =
    lock.acquire()
    try elementAtOrNull(lowerIndex(e))
    finally lock.release()

  def floor(e: E): E =
    lock.acquire()
    try elementAtOrNull(floorIndex(e))
    finally lock.release()

  def ceiling(e: E): E =
    lock.acquire()
    try elementAtOrNull(ceilingIndex(e))
    finally lock.release()

  def higher(e: E): E =
    lock.acquire()
    try elementAtOrNull(higherIndex(e))
    finally lock.release()

  def pollFirst(): E =
    lock.acquire()
    try
      if inner.isEmpty() then null.asInstanceOf[E]
      else inner.remove(0)
    finally lock.release()

  def pollLast(): E =
    lock.acquire()
    try
      if inner.isEmpty() then null.asInstanceOf[E]
      else inner.remove(inner.size() - 1)
    finally lock.release()

  def comparator(): Comparator[_ >: E] =
    exposedComparator.asInstanceOf[Comparator[_ >: E]]

  def first(): E =
    lock.acquire()
    try
      if inner.isEmpty() then
        throw new NoSuchElementException()
      inner.get(0)
    finally lock.release()

  def last(): E =
    lock.acquire()
    try
      if inner.isEmpty() then
        throw new NoSuchElementException()
      inner.get(inner.size() - 1)
    finally lock.release()

  def subSet(fromElement: E, fromInclusive: Boolean, toElement: E, toInclusive: Boolean): NavigableSet[E] =
    val snapshot = new ConcurrentSkipListSet[E](internalComparator, exposedComparator)
    lock.acquire()
    try
      var index = 0
      while index < inner.size() do
        val value = inner.get(index)
        if isAtLeast(value, fromElement, fromInclusive) && isAtMost(value, toElement, toInclusive) then
          snapshot.inner.add(value)
        index += 1
      snapshot
    finally lock.release()

  def headSet(toElement: E, inclusive: Boolean): NavigableSet[E] =
    val snapshot = new ConcurrentSkipListSet[E](internalComparator, exposedComparator)
    lock.acquire()
    try
      var index = 0
      while index < inner.size() do
        val value = inner.get(index)
        if isAtMost(value, toElement, inclusive) then
          snapshot.inner.add(value)
        index += 1
      snapshot
    finally lock.release()

  def tailSet(fromElement: E, inclusive: Boolean): NavigableSet[E] =
    val snapshot = new ConcurrentSkipListSet[E](internalComparator, exposedComparator)
    lock.acquire()
    try
      var index = 0
      while index < inner.size() do
        val value = inner.get(index)
        if isAtLeast(value, fromElement, inclusive) then
          snapshot.inner.add(value)
        index += 1
      snapshot
    finally lock.release()

  def subSet(fromElement: E, toElement: E): SortedSet[E] =
    subSet(fromElement, true, toElement, false)

  def headSet(toElement: E): SortedSet[E] =
    headSet(toElement, false)

  def tailSet(fromElement: E): SortedSet[E] =
    tailSet(fromElement, true)

  def descendingSet(): NavigableSet[E] =
    val reversedComparator = internalComparator.asInstanceOf[Comparator[E]].reversed()
    val exposedReversed =
      if exposedComparator == null then reversedComparator
      else exposedComparator.asInstanceOf[Comparator[E]].reversed()
    val snapshot = new ConcurrentSkipListSet[E](reversedComparator, exposedReversed)
    lock.acquire()
    try
      var index = 0
      while index < inner.size() do
        snapshot.inner.add(inner.get(index))
        index += 1
      snapshot
    finally lock.release()

  private def snapshotValues(): ArrayList[E] =
    lock.acquire()
    try new ArrayList[E](inner)
    finally lock.release()

  private def findIndex(value: Any): Int =
    var low = 0
    var high = inner.size() - 1
    while low <= high do
      val mid = (low + high) >>> 1
      val cmp = compare(inner.get(mid), value)
      if cmp < 0 then
        low = mid + 1
      else if cmp > 0 then
        high = mid - 1
      else
        return mid
    -(low + 1)

  private def lowerIndex(value: E): Int =
    val index = findIndex(value)
    if index >= 0 then index - 1
    else insertionPoint(index) - 1

  private def floorIndex(value: E): Int =
    val index = findIndex(value)
    if index >= 0 then index
    else insertionPoint(index) - 1

  private def ceilingIndex(value: E): Int =
    val index = findIndex(value)
    if index >= 0 then index
    else insertionPoint(index)

  private def higherIndex(value: E): Int =
    val index = findIndex(value)
    if index >= 0 then index + 1
    else insertionPoint(index)

  private def insertionPoint(index: Int): Int =
    -index - 1

  private def elementAtOrNull(index: Int): E =
    if index < 0 || index >= inner.size() then null.asInstanceOf[E]
    else inner.get(index)

  private def compare(left: Any, right: Any): Int =
    internalComparator.asInstanceOf[Comparator[Any]].compare(left, right)

  private def isAtLeast(value: E, lowerBound: E, inclusive: Boolean): Boolean =
    val cmp = compare(value, lowerBound)
    if inclusive then cmp >= 0 else cmp > 0

  private def isAtMost(value: E, upperBound: E, inclusive: Boolean): Boolean =
    val cmp = compare(value, upperBound)
    if inclusive then cmp <= 0 else cmp < 0

  private final class SnapshotIterator(snapshot: ArrayList[E], descending: Boolean) extends Iterator[E]:
    private var index =
      if descending then snapshot.size() - 1
      else 0
    private var current: E | Null = null

    def hasNext(): Boolean =
      if descending then index >= 0
      else index < snapshot.size()

    def next(): E =
      if !hasNext() then
        throw new NoSuchElementException()
      val value = snapshot.get(index)
      current = value
      if descending then index -= 1
      else index += 1
      value

    override def remove(): Unit =
      val value = current
      if value == null then
        throw new IllegalStateException()
      self.remove(value)
      current = null

object ConcurrentSkipListSet:
  private object UniversalComparator extends Comparator[Any] with Serializable:
    def compare(o1: Any, o2: Any): Int =
      if o1.isInstanceOf[Int] && o2.isInstanceOf[Int] then
        val left = o1.asInstanceOf[Int]
        val right = o2.asInstanceOf[Int]
        if left < right then -1 else if left > right then 1 else 0
      else if o1.isInstanceOf[Long] && o2.isInstanceOf[Long] then
        val left = o1.asInstanceOf[Long]
        val right = o2.asInstanceOf[Long]
        if left < right then -1 else if left > right then 1 else 0
      else if o1.isInstanceOf[Short] && o2.isInstanceOf[Short] then
        val left = o1.asInstanceOf[Short]
        val right = o2.asInstanceOf[Short]
        if left < right then -1 else if left > right then 1 else 0
      else if o1.isInstanceOf[Byte] && o2.isInstanceOf[Byte] then
        val left = o1.asInstanceOf[Byte]
        val right = o2.asInstanceOf[Byte]
        if left < right then -1 else if left > right then 1 else 0
      else if o1.isInstanceOf[Char] && o2.isInstanceOf[Char] then
        val left = o1.asInstanceOf[Char]
        val right = o2.asInstanceOf[Char]
        if left < right then -1 else if left > right then 1 else 0
      else if o1.isInstanceOf[Float] && o2.isInstanceOf[Float] then
        java.lang.Float.compare(o1.asInstanceOf[Float], o2.asInstanceOf[Float])
      else if o1.isInstanceOf[Double] && o2.isInstanceOf[Double] then
        java.lang.Double.compare(o1.asInstanceOf[Double], o2.asInstanceOf[Double])
      else if o1.isInstanceOf[Boolean] && o2.isInstanceOf[Boolean] then
        java.lang.Boolean.compare(o1.asInstanceOf[Boolean], o2.asInstanceOf[Boolean])
      else if o1.isInstanceOf[String] && o2.isInstanceOf[String] then
        o1.asInstanceOf[String].compareTo(o2.asInstanceOf[String])
      else if o1.isInstanceOf[Comparable[Any @unchecked]] then
        o1.asInstanceOf[Comparable[Any]].compareTo(o2)
      else
        throw new ClassCastException()

  private def defaultComparator[E](): Comparator[E] =
    UniversalComparator.asInstanceOf[Comparator[E]]

  private def selectComparator[E](comparator: Comparator[_ >: E]): Comparator[_ >: E] =
    if comparator == null then defaultComparator[E]()
    else comparator
