package java.util

import java.{lang => jl}
import java.io.Serializable

import scala.python.{PyAny, extern, name, native}

private[util] object CollectionsPython:
  @extern("operator")
  private object PyOperator extends PyAny:
    @name("lt")
    def lt(left: Any, right: Any): Boolean = native

    @name("gt")
    def gt(left: Any, right: Any): Boolean = native

  def compare(left: Any, right: Any): Int =
    if Objects.equals(left, right) then 0
    else if PyOperator.lt(left, right) then -1
    else if PyOperator.gt(left, right) then 1
    else 0

object Collections {
  final val EMPTY_SET: Set[_] =
    new AbstractSet[Any] with Serializable {
      override def size(): Int = 0
      def iterator(): Iterator[Any] = emptyIterator[Any]()
    }

  final val EMPTY_LIST: List[_] =
    new AbstractList[Any] with RandomAccess with Serializable {
      override def size(): Int = 0
      def get(index: Int): Any =
        throw new IndexOutOfBoundsException(index.toString())
    }

  final val EMPTY_MAP: Map[_, _] =
    new AbstractMap[Any, Any] with Serializable {
      def entrySet(): Set[Map.Entry[Any, Any]] =
        emptySet[Map.Entry[Any, Any]]()
    }

  private val EmptyIteratorSingleton =
    new Iterator[Any] {
      def hasNext(): Boolean = false
      def next(): Any = throw new NoSuchElementException()
    }

  private val EmptyListIteratorSingleton =
    new ListIterator[Any] {
      def hasNext(): Boolean = false
      def next(): Any = throw new NoSuchElementException()
      def hasPrevious(): Boolean = false
      def previous(): Any = throw new NoSuchElementException()
      def nextIndex(): Int = 0
      def previousIndex(): Int = -1
      override def remove(): Unit = throw new UnsupportedOperationException()
      def set(e: Any): Unit = throw new UnsupportedOperationException()
      def add(e: Any): Unit = throw new UnsupportedOperationException()
    }

  private val EmptyEnumerationSingleton =
    new Enumeration[Any] {
      def hasMoreElements(): Boolean = false
      def nextElement(): Any = throw new NoSuchElementException()
    }

  def sort[T <: jl.Comparable[T]](list: List[T]): Unit = {
    var i = 1
    while i < list.size() do
      val value = list.get(i)
      var j = i
      while j > 0 && CollectionsPython.compare(value, list.get(j - 1)) < 0 do
        list.set(j, list.get(j - 1))
        j -= 1
      list.set(j, value)
      i += 1
  }

  def sort[T](list: List[T], c: Comparator[_ >: T]): Unit =
    list.sort(c)

  def reverse(list: List[_]): Unit =
    reverseImpl(list.asInstanceOf[List[AnyRef]])

  def shuffle(list: List[_]): Unit =
    shuffleImpl(list.asInstanceOf[List[AnyRef]], new Random())

  def shuffle[T](list: List[T], rnd: Random): Unit =
    shuffleImpl(list, rnd)

  def swap(list: List[_], i: Int, j: Int): Unit =
    swapImpl(list.asInstanceOf[List[AnyRef]], i, j)

  def min[T <: jl.Comparable[T]](coll: Collection[_ <: T]): T = {
    val iter = coll.iterator()
    if !iter.hasNext() then
      throw new NoSuchElementException()
    var best = iter.next()
    while iter.hasNext() do
      val next = iter.next()
      if CollectionsPython.compare(next, best) < 0 then
        best = next
    best
  }

  def min[T](coll: Collection[_ <: T], comp: Comparator[_ >: T]): T = {
    val iter = coll.iterator()
    if !iter.hasNext() then
      throw new NoSuchElementException()
    var best = iter.next()
    while iter.hasNext() do
      val next = iter.next()
      if comp.compare(next, best) < 0 then
        best = next
    best
  }

  def max[T <: jl.Comparable[T]](coll: Collection[_ <: T]): T = {
    val iter = coll.iterator()
    if !iter.hasNext() then
      throw new NoSuchElementException()
    var best = iter.next()
    while iter.hasNext() do
      val next = iter.next()
      if CollectionsPython.compare(next, best) > 0 then
        best = next
    best
  }

  def max[T](coll: Collection[_ <: T], comp: Comparator[_ >: T]): T = {
    val iter = coll.iterator()
    if !iter.hasNext() then
      throw new NoSuchElementException()
    var best = iter.next()
    while iter.hasNext() do
      val next = iter.next()
      if comp.compare(next, best) > 0 then
        best = next
    best
  }

  def emptyIterator[T](): Iterator[T] =
    EmptyIteratorSingleton.asInstanceOf[Iterator[T]]

  def emptyListIterator[T](): ListIterator[T] =
    EmptyListIteratorSingleton.asInstanceOf[ListIterator[T]]

  def emptyEnumeration[T](): Enumeration[T] =
    EmptyEnumerationSingleton.asInstanceOf[Enumeration[T]]

  def emptySet[T](): Set[T] =
    EMPTY_SET.asInstanceOf[Set[T]]

  def emptyList[T](): List[T] =
    EMPTY_LIST.asInstanceOf[List[T]]

  def emptyMap[K, V](): Map[K, V] =
    EMPTY_MAP.asInstanceOf[Map[K, V]]

  def singleton[T](o: T): Set[T] =
    new AbstractSet[T] with Serializable {
      override def size(): Int = 1
      def iterator(): Iterator[T] = new SingletonIterator[T](o)
    }

  def singletonList[T](o: T): List[T] =
    new AbstractList[T] with RandomAccess with Serializable {
      override def size(): Int = 1

      def get(index: Int): T =
        if index == 0 then o
        else throw new IndexOutOfBoundsException(index.toString())
    }

  def singletonMap[K, V](key: K, value: V): Map[K, V] =
    new AbstractMap[K, V] with Serializable {
      def entrySet(): Set[Map.Entry[K, V]] =
        singleton[Map.Entry[K, V]](new AbstractMap.SimpleImmutableEntry[K, V](key, value))
    }

  def enumeration[T](c: Collection[T]): Enumeration[T] =
    new IteratorEnumeration[T](c.iterator())

  def list[T](e: Enumeration[T]): ArrayList[T] = {
    val out = new ArrayList[T]()
    while e.hasMoreElements() do
      out.add(e.nextElement())
    out
  }

  def unmodifiableCollection[T](c: Collection[_ <: T]): Collection[T] =
    new UnmodifiableCollection[T](c.asInstanceOf[Collection[T]])

  def unmodifiableSet[T](s: Set[_ <: T]): Set[T] =
    new UnmodifiableSet[T](s.asInstanceOf[Set[T]])

  def unmodifiableList[T](list: List[_ <: T]): List[T] = {
    val underlying = list.asInstanceOf[List[T]]
    if underlying.isInstanceOf[RandomAccess] then
      new UnmodifiableList[T](underlying) with RandomAccess
    else
      new UnmodifiableList[T](underlying)
  }

  def unmodifiableMap[K, V](m: Map[_ <: K, _ <: V]): Map[K, V] =
    new UnmodifiableMap[K, V](m.asInstanceOf[Map[K, V]])

  def synchronizedCollection[T](c: Collection[T]): Collection[T] =
    newSynchronizedCollection(c)

  def synchronizedSet[T](s: Set[T]): Set[T] =
    newSynchronizedSet(s)

  def synchronizedList[T](list: List[T]): List[T] =
    newSynchronizedList(list)

  def synchronizedMap[K, V](m: Map[K, V]): Map[K, V] =
    newSynchronizedMap(m)

  private def reverseImpl[T](list: List[T]): Unit = {
    var i = 0
    var j = list.size() - 1
    while i < j do
      val tmp = list.get(i)
      list.set(i, list.get(j))
      list.set(j, tmp)
      i += 1
      j -= 1
  }

  private def shuffleImpl[T](list: List[T], rnd: Random): Unit = {
    var n = list.size()
    while n > 1 do
      swapImpl(list, n - 1, rnd.nextInt(n))
      n -= 1
  }

  private def swapImpl[T](list: List[T], i: Int, j: Int): Unit = {
    val tmp = list.get(i)
    list.set(i, list.get(j))
    list.set(j, tmp)
  }
}

private def newSynchronizedCollection[E](inner: Collection[E]): Collection[E] =
  new SynchronizedCollection[E](inner)

private def newSynchronizedCollection[E](inner: Collection[E], mutex: AnyRef): Collection[E] =
  new SynchronizedCollection[E](inner, mutex)

private def newSynchronizedSet[E](inner: Set[E]): Set[E] =
  new SynchronizedSet[E](inner)

private def newSynchronizedSet[E](inner: Set[E], mutex: AnyRef): Set[E] =
  new SynchronizedSet[E](inner, mutex)

private def newSynchronizedList[E](inner: List[E]): List[E] =
  if inner.isInstanceOf[RandomAccess] then
    new SynchronizedRandomAccessList[E](inner)
  else
    new SynchronizedList[E](inner)

private def newSynchronizedList[E](inner: List[E], mutex: AnyRef): List[E] =
  if inner.isInstanceOf[RandomAccess] then
    new SynchronizedRandomAccessList[E](inner, mutex)
  else
    new SynchronizedList[E](inner, mutex)

private def newSynchronizedMap[K, V](inner: Map[K, V]): Map[K, V] =
  new SynchronizedMap[K, V](inner)

private def newSynchronizedMap[K, V](inner: Map[K, V], mutex: AnyRef): Map[K, V] =
  new SynchronizedMap[K, V](inner, mutex)

private final class SingletonIterator[E](element: E) extends Iterator[E] {
  private var hasNext0 = true

  def hasNext(): Boolean = hasNext0

  def next(): E = {
    if !hasNext0 then
      throw new NoSuchElementException()
    hasNext0 = false
    element
  }
}

private final class IteratorEnumeration[E](inner: Iterator[E]) extends Enumeration[E] {
  def hasMoreElements(): Boolean = inner.hasNext()
  def nextElement(): E = inner.next()
}

private class UnmodifiableCollection[E](private val inner: Collection[E])
    extends AbstractCollection[E] {
  override def size(): Int = inner.size()
  override def isEmpty(): Boolean = inner.isEmpty()
  override def contains(o: Any): Boolean = inner.contains(o)
  override def toArray(): Array[AnyRef] = inner.toArray()
  override def toArray[T <: AnyRef](a: Array[T]): Array[T] = inner.toArray(a)

  def iterator(): Iterator[E] =
    new Iterator[E] {
      private val iter = inner.iterator()

      def hasNext(): Boolean = iter.hasNext()
      def next(): E = iter.next()
    }

  override def add(e: E): Boolean =
    throw new UnsupportedOperationException()

  override def remove(o: Any): Boolean =
    throw new UnsupportedOperationException()

  override def addAll(c: Collection[_ <: E]): Boolean =
    throw new UnsupportedOperationException()

  override def removeAll(c: Collection[_]): Boolean =
    throw new UnsupportedOperationException()

  override def retainAll(c: Collection[_]): Boolean =
    throw new UnsupportedOperationException()

  override def clear(): Unit =
    throw new UnsupportedOperationException()
}

private final class UnmodifiableSet[E](private val inner: Set[E]) extends AbstractSet[E] {
  override def size(): Int = inner.size()
  override def isEmpty(): Boolean = inner.isEmpty()
  override def contains(o: Any): Boolean = inner.contains(o)

  def iterator(): Iterator[E] =
    new Iterator[E] {
      private val iter = inner.iterator()

      def hasNext(): Boolean = iter.hasNext()
      def next(): E = iter.next()
    }

  override def add(e: E): Boolean =
    throw new UnsupportedOperationException()

  override def remove(o: Any): Boolean =
    throw new UnsupportedOperationException()

  override def addAll(c: Collection[_ <: E]): Boolean =
    throw new UnsupportedOperationException()

  override def removeAll(c: Collection[_]): Boolean =
    throw new UnsupportedOperationException()

  override def retainAll(c: Collection[_]): Boolean =
    throw new UnsupportedOperationException()

  override def clear(): Unit =
    throw new UnsupportedOperationException()
}

private final class UnmodifiableListIterator[E](private val inner: ListIterator[E])
    extends ListIterator[E] {
  def hasNext(): Boolean = inner.hasNext()
  def next(): E = inner.next()
  def hasPrevious(): Boolean = inner.hasPrevious()
  def previous(): E = inner.previous()
  def nextIndex(): Int = inner.nextIndex()
  def previousIndex(): Int = inner.previousIndex()
  override def remove(): Unit = throw new UnsupportedOperationException()
  def set(e: E): Unit = throw new UnsupportedOperationException()
  def add(e: E): Unit = throw new UnsupportedOperationException()
}

private class UnmodifiableList[E](private val inner: List[E]) extends AbstractList[E] {
  override def size(): Int = inner.size()
  override def isEmpty(): Boolean = inner.isEmpty()
  override def contains(o: Any): Boolean = inner.contains(o)
  def get(index: Int): E = inner.get(index)
  override def indexOf(o: Any): Int = inner.indexOf(o)
  override def lastIndexOf(o: Any): Int = inner.lastIndexOf(o)

  override def listIterator(index: Int): ListIterator[E] =
    new UnmodifiableListIterator[E](inner.listIterator(index))

  override def subList(fromIndex: Int, toIndex: Int): List[E] =
    Collections.unmodifiableList(inner.subList(fromIndex, toIndex))

  override def set(index: Int, element: E): E =
    throw new UnsupportedOperationException()

  override def add(index: Int, element: E): Unit =
    throw new UnsupportedOperationException()

  override def remove(index: Int): E =
    throw new UnsupportedOperationException()

  override def addAll(index: Int, c: Collection[_ <: E]): Boolean =
    throw new UnsupportedOperationException()

  override def clear(): Unit =
    throw new UnsupportedOperationException()
}

private final class UnmodifiableEntrySet[K, V](private val inner: Set[Map.Entry[K, V]])
    extends AbstractSet[Map.Entry[K, V]] {
  override def size(): Int = inner.size()
  override def isEmpty(): Boolean = inner.isEmpty()
  override def contains(o: Any): Boolean = inner.contains(o)

  def iterator(): Iterator[Map.Entry[K, V]] =
    new Iterator[Map.Entry[K, V]] {
      private val iter = inner.iterator()

      def hasNext(): Boolean = iter.hasNext()
      def next(): Map.Entry[K, V] =
        new AbstractMap.SimpleImmutableEntry[K, V](iter.next())
    }

  override def add(e: Map.Entry[K, V]): Boolean =
    throw new UnsupportedOperationException()

  override def remove(o: Any): Boolean =
    throw new UnsupportedOperationException()

  override def clear(): Unit =
    throw new UnsupportedOperationException()
}

private final class UnmodifiableMap[K, V](private val inner: Map[K, V]) extends AbstractMap[K, V] {
  override def size(): Int = inner.size()
  override def isEmpty(): Boolean = inner.isEmpty()
  override def containsKey(key: Any): Boolean = inner.containsKey(key)
  override def containsValue(value: Any): Boolean = inner.containsValue(value)
  override def get(key: Any): V = inner.get(key)

  def entrySet(): Set[Map.Entry[K, V]] =
    new UnmodifiableEntrySet[K, V](inner.entrySet())

  override def put(key: K, value: V): V =
    throw new UnsupportedOperationException()

  override def remove(key: Any): V =
    throw new UnsupportedOperationException()

  override def putAll(m: Map[_ <: K, _ <: V]): Unit =
    throw new UnsupportedOperationException()

  override def clear(): Unit =
    throw new UnsupportedOperationException()
}

private class SynchronizedCollection[E](private val innerCollection: Collection[E])
    extends Collection[E]
    with Serializable {
  protected var mutex: AnyRef = this

  def this(innerCollection: Collection[E], mutex: AnyRef) = {
    this(innerCollection)
    this.mutex = mutex
  }

  def size(): Int = mutex.synchronized {
    innerCollection.size()
  }

  def isEmpty(): Boolean = mutex.synchronized {
    innerCollection.isEmpty()
  }

  def contains(o: Any): Boolean = mutex.synchronized {
    innerCollection.contains(o)
  }

  def iterator(): Iterator[E] = mutex.synchronized {
    innerCollection.iterator()
  }

  def toArray(): Array[AnyRef] = mutex.synchronized {
    innerCollection.toArray()
  }

  def toArray[T <: AnyRef](a: Array[T]): Array[T] = mutex.synchronized {
    innerCollection.toArray(a)
  }

  def add(e: E): Boolean = mutex.synchronized {
    innerCollection.add(e)
  }

  def remove(o: Any): Boolean = mutex.synchronized {
    innerCollection.remove(o)
  }

  def containsAll(c: Collection[_]): Boolean = mutex.synchronized {
    innerCollection.containsAll(c)
  }

  def addAll(c: Collection[_ <: E]): Boolean = mutex.synchronized {
    innerCollection.addAll(c)
  }

  def removeAll(c: Collection[_]): Boolean = mutex.synchronized {
    innerCollection.removeAll(c)
  }

  def retainAll(c: Collection[_]): Boolean = mutex.synchronized {
    innerCollection.retainAll(c)
  }

  def clear(): Unit = mutex.synchronized {
    innerCollection.clear()
  }

  override def toString(): String = mutex.synchronized {
    innerCollection.toString()
  }
}

private final class SynchronizedSet[E](private val innerSet: Set[E])
    extends SynchronizedCollection[E](innerSet)
    with Set[E] {
  def this(innerSet: Set[E], mutex: AnyRef) = {
    this(innerSet)
    this.mutex = mutex
  }

  override def equals(other: Any): Boolean =
    if other.asInstanceOf[AnyRef] eq this then true
    else mutex.synchronized {
      innerSet.equals(other)
    }

  override def hashCode(): Int = mutex.synchronized {
    innerSet.hashCode()
  }
}

private class SynchronizedList[E](private val innerList: List[E])
    extends SynchronizedCollection[E](innerList)
    with List[E] {
  def this(innerList: List[E], mutex: AnyRef) = {
    this(innerList)
    this.mutex = mutex
  }

  def addAll(index: Int, c: Collection[_ <: E]): Boolean = mutex.synchronized {
    innerList.addAll(index, c)
  }

  def get(index: Int): E = mutex.synchronized {
    innerList.get(index)
  }

  def set(index: Int, element: E): E = mutex.synchronized {
    innerList.set(index, element)
  }

  def add(index: Int, element: E): Unit = mutex.synchronized {
    innerList.add(index, element)
  }

  def remove(index: Int): E = mutex.synchronized {
    innerList.remove(index)
  }

  def indexOf(o: Any): Int = mutex.synchronized {
    innerList.indexOf(o)
  }

  def lastIndexOf(o: Any): Int = mutex.synchronized {
    innerList.lastIndexOf(o)
  }

  override def listIterator(): ListIterator[E] = mutex.synchronized {
    innerList.listIterator()
  }

  def listIterator(index: Int): ListIterator[E] = mutex.synchronized {
    innerList.listIterator(index)
  }

  def subList(fromIndex: Int, toIndex: Int): List[E] = mutex.synchronized {
    newSynchronizedList(innerList.subList(fromIndex, toIndex), mutex)
  }

  override def equals(other: Any): Boolean =
    if other.asInstanceOf[AnyRef] eq this then true
    else mutex.synchronized {
      innerList.equals(other)
    }

  override def hashCode(): Int = mutex.synchronized {
    innerList.hashCode()
  }
}

private final class SynchronizedRandomAccessList[E](innerList: List[E])
    extends SynchronizedList[E](innerList)
    with RandomAccess {
  def this(innerList: List[E], mutex: AnyRef) = {
    this(innerList)
    this.mutex = mutex
  }
}

private final class SynchronizedMap[K, V](private val innerMap: Map[K, V])
    extends Map[K, V]
    with Serializable {
  private var mutex: AnyRef = this

  def this(innerMap: Map[K, V], mutex: AnyRef) = {
    this(innerMap)
    this.mutex = mutex
  }

  def size(): Int = mutex.synchronized {
    innerMap.size()
  }

  def isEmpty(): Boolean = mutex.synchronized {
    innerMap.isEmpty()
  }

  def containsKey(key: Any): Boolean = mutex.synchronized {
    innerMap.containsKey(key)
  }

  def containsValue(value: Any): Boolean = mutex.synchronized {
    innerMap.containsValue(value)
  }

  def get(key: Any): V = mutex.synchronized {
    innerMap.get(key)
  }

  def put(key: K, value: V): V = mutex.synchronized {
    innerMap.put(key, value)
  }

  def remove(key: Any): V = mutex.synchronized {
    innerMap.remove(key)
  }

  def putAll(m: Map[_ <: K, _ <: V]): Unit = mutex.synchronized {
    innerMap.putAll(m)
  }

  def clear(): Unit = mutex.synchronized {
    innerMap.clear()
  }

  def keySet(): Set[K] = mutex.synchronized {
    newSynchronizedSet(innerMap.keySet(), mutex)
  }

  def values(): Collection[V] = mutex.synchronized {
    newSynchronizedCollection(innerMap.values(), mutex)
  }

  def entrySet(): Set[Map.Entry[K, V]] = mutex.synchronized {
    newSynchronizedSet(innerMap.entrySet(), mutex)
  }

  override def equals(other: Any): Boolean =
    if other.asInstanceOf[AnyRef] eq this then true
    else mutex.synchronized {
      innerMap.equals(other)
    }

  override def hashCode(): Int = mutex.synchronized {
    innerMap.hashCode()
  }

  override def toString(): String = mutex.synchronized {
    innerMap.toString()
  }
}
