import java.util.{AbstractCollection, AbstractList, AbstractMap, AbstractQueue, AbstractSequentialList, AbstractSet, Collection, Iterator, List, ListIterator, Map, Objects, RandomAccess}

private def joinIterator[E](iterator: Iterator[E]): String =
  var result = ""
  var first = true
  while iterator.hasNext() do
    if !first then
      result += ","
    result += String.valueOf(iterator.next())
    first = false
  result

private def joinCollection[E](collection: Collection[E]): String =
  joinIterator(collection.iterator())

private def joinList[E](list: List[E]): String =
  joinIterator(list.iterator())

final class MutableStringCollection(initial: Array[String]) extends AbstractCollection[String]:
  private var data = new Array[String](16)
  private var size0 = 0

  copyFrom(initial)

  override def add(value: String): Boolean =
    ensureCapacity(size0 + 1)
    data(size0) = value
    size0 += 1
    true

  def size(): Int = size0

  def iterator(): Iterator[String] =
    new Iterator[String]:
      private var index = 0
      private var last = -1

      def hasNext(): Boolean = index < size0

      def next(): String =
        if !hasNext() then
          throw new java.util.NoSuchElementException()
        val value = data(index)
        last = index
        index += 1
        value

      override def remove(): Unit =
        if last < 0 then
          throw new IllegalStateException()
        removeAt(last)
        index = last
        last = -1

  private def copyFrom(values: Array[String]): Unit =
    var index = 0
    while index < values.length do
      add(values(index))
      index += 1

  private def ensureCapacity(required: Int): Unit =
    if required > data.length then
      val grown = new Array[String](required * 2)
      System.arraycopy(data, 0, grown, 0, size0)
      data = grown

  private def removeAt(index: Int): Unit =
    var cursor = index + 1
    while cursor < size0 do
      data(cursor - 1) = data(cursor)
      cursor += 1
    size0 -= 1
    data(size0) = null

final class MutableStringList(initial: Array[String]) extends AbstractList[String] with RandomAccess:
  private var data = new Array[String](16)
  private var size0 = 0

  copyFrom(initial)

  def size(): Int = size0

  def get(index: Int): String =
    checkExisting(index)
    data(index)

  override def set(index: Int, value: String): String =
    checkExisting(index)
    val previous = data(index)
    data(index) = value
    previous

  override def add(index: Int, value: String): Unit =
    checkInsert(index)
    ensureCapacity(size0 + 1)
    var cursor = size0
    while cursor > index do
      data(cursor) = data(cursor - 1)
      cursor -= 1
    data(index) = value
    size0 += 1

  override def remove(index: Int): String =
    checkExisting(index)
    val previous = data(index)
    var cursor = index + 1
    while cursor < size0 do
      data(cursor - 1) = data(cursor)
      cursor += 1
    size0 -= 1
    data(size0) = null
    previous

  private def copyFrom(values: Array[String]): Unit =
    var index = 0
    while index < values.length do
      add(size0, values(index))
      index += 1

  private def ensureCapacity(required: Int): Unit =
    if required > data.length then
      val grown = new Array[String](required * 2)
      System.arraycopy(data, 0, grown, 0, size0)
      data = grown

  private def checkExisting(index: Int): Unit =
    if index < 0 || index >= size0 then
      throw new IndexOutOfBoundsException(index.toString)

  private def checkInsert(index: Int): Unit =
    if index < 0 || index > size0 then
      throw new IndexOutOfBoundsException(index.toString)

final class MutableStringMap(initialKeys: Array[String], initialValues: Array[String])
    extends AbstractMap[String, String]:
  private var keys = new Array[String](16)
  private var storedValues = new Array[String](16)
  private var size0 = 0

  copyFrom(initialKeys, initialValues)

  override def put(key: String, value: String): String =
    val slot = findIndex(key)
    if slot >= 0 then
      val previous = storedValues(slot)
      storedValues(slot) = value
      previous
    else
      ensureCapacity(size0 + 1)
      keys(size0) = key
      storedValues(size0) = value
      size0 += 1
      null

  override def entrySet(): java.util.Set[Map.Entry[String, String]] =
    new AbstractSet[Map.Entry[String, String]]:
      def size(): Int = size0

      def iterator(): Iterator[Map.Entry[String, String]] =
        new Iterator[Map.Entry[String, String]]:
          private var index = 0
          private var last = -1

          def hasNext(): Boolean = index < size0

          def next(): Map.Entry[String, String] =
            if !hasNext() then
              throw new java.util.NoSuchElementException()
            val slot = index
            last = slot
            index += 1
            new Map.Entry[String, String]:
              def getKey(): String = keys(slot)
              def getValue(): String = storedValues(slot)

              def setValue(value: String): String =
                val previous = storedValues(slot)
                storedValues(slot) = value
                previous

              override def equals(other: Any): Boolean =
                other match
                  case entry: Map.Entry[?, ?] =>
                    Objects.equals(getKey(), entry.getKey()) &&
                    Objects.equals(getValue(), entry.getValue())
                  case _ =>
                    false

              override def hashCode(): Int =
                Objects.hashCode(getKey()) ^ Objects.hashCode(getValue())

              override def toString(): String =
                getKey() + "=" + getValue()

          override def remove(): Unit =
            if last < 0 then
              throw new IllegalStateException()
            removeAt(last)
            index = last
            last = -1

  private def copyFrom(initialKeys: Array[String], initialValues: Array[String]): Unit =
    var index = 0
    while index < initialKeys.length do
      put(initialKeys(index), initialValues(index))
      index += 1

  private def ensureCapacity(required: Int): Unit =
    if required > keys.length then
      val grownKeys = new Array[String](required * 2)
      val grownValues = new Array[String](required * 2)
      System.arraycopy(keys, 0, grownKeys, 0, size0)
      System.arraycopy(storedValues, 0, grownValues, 0, size0)
      keys = grownKeys
      storedValues = grownValues

  private def findIndex(key: String): Int =
    var index = 0
    while index < size0 do
      if Objects.equals(keys(index), key) then
        return index
      index += 1
    -1

  private def removeAt(index: Int): Unit =
    var cursor = index + 1
    while cursor < size0 do
      keys(cursor - 1) = keys(cursor)
      storedValues(cursor - 1) = storedValues(cursor)
      cursor += 1
    size0 -= 1
    keys(size0) = null
    storedValues(size0) = null

final class MutableStringQueue(initial: Array[String]) extends AbstractQueue[String]:
  private var data = new Array[String](8)
  private var size0 = 0

  var index = 0
  while index < initial.length do
    offer(initial(index))
    index += 1

  def size(): Int = size0

  def iterator(): Iterator[String] =
    new Iterator[String]:
      private var cursor = 0
      private var last = -1

      def hasNext(): Boolean = cursor < size0

      def next(): String =
        if !hasNext() then
          throw new java.util.NoSuchElementException()
        val value = data(cursor)
        last = cursor
        cursor += 1
        value

      override def remove(): Unit =
        if last < 0 then
          throw new IllegalStateException()
        removeAt(last)
        cursor = last
        last = -1

  def offer(value: String): Boolean =
    ensureCapacity(size0 + 1)
    data(size0) = value
    size0 += 1
    true

  def poll(): String =
    if size0 == 0 then
      null
    else
      removeAt(0)

  def peek(): String =
    if size0 == 0 then null else data(0)

  private def ensureCapacity(required: Int): Unit =
    if required > data.length then
      val grown = new Array[String](required * 2)
      System.arraycopy(data, 0, grown, 0, size0)
      data = grown

  private def removeAt(index: Int): String =
    val previous = data(index)
    var cursor = index + 1
    while cursor < size0 do
      data(cursor - 1) = data(cursor)
      cursor += 1
    size0 -= 1
    data(size0) = null
    previous

final class MutableIntSet(initial: Array[Int]) extends AbstractSet[Int]:
  private var data = new Array[Int](8)
  private var size0 = 0

  var index = 0
  while index < initial.length do
    add(initial(index))
    index += 1

  def size(): Int = size0

  override def contains(value: Any): Boolean =
    var cursor = 0
    while cursor < size0 do
      if data(cursor) == value.asInstanceOf[Int] then
        return true
      cursor += 1
    false

  override def add(value: Int): Boolean =
    if contains(value) then
      false
    else
      ensureCapacity(size0 + 1)
      data(size0) = value
      size0 += 1
      true

  def iterator(): Iterator[Int] =
    new Iterator[Int]:
      private var cursor = 0
      private var last = -1

      def hasNext(): Boolean = cursor < size0

      def next(): Int =
        if !hasNext() then
          throw new java.util.NoSuchElementException()
        val value = data(cursor)
        last = cursor
        cursor += 1
        value

      override def remove(): Unit =
        if last < 0 then
          throw new IllegalStateException()
        removeAt(last)
        cursor = last
        last = -1

  private def ensureCapacity(required: Int): Unit =
    if required > data.length then
      val grown = new Array[Int](required * 2)
      System.arraycopy(data, 0, grown, 0, size0)
      data = grown

  private def removeAt(index: Int): Unit =
    var cursor = index + 1
    while cursor < size0 do
      data(cursor - 1) = data(cursor)
      cursor += 1
    size0 -= 1

final class SequentialStringList(private val underlying: MutableStringList)
    extends AbstractSequentialList[String]:
  def size(): Int = underlying.size()

  override def listIterator(index: Int): ListIterator[String] =
    underlying.listIterator(index)

@main def javalibUtilAbstractCollections(): Unit =
  val collection = new MutableStringCollection(Array("keep", "drop", "stay"))
  collection.removeAll(new MutableStringCollection(Array("drop")))
  println(
    "abstractcollection:" + joinCollection(collection) + ":" +
      collection.contains("stay") + ":" + collection.size()
  )

  val list = new MutableStringList(Array("beta", "gamma"))
  list.add("delta")
  list.add(0, "alpha")
  val slice = list.subList(1, 3)
  println("abstractlist:" + joinList(list) + ":" + joinList(slice))

  val map = new MutableStringMap(Array("a", "b"), Array("1", "2"))
  map.put("c", "3")
  map.remove("b")
  println(
    "abstractmap:" + joinCollection(map.keySet()) + ":" +
      joinCollection(map.values()) + ":" + map.toString()
  )

  val queue = new MutableStringQueue(Array("first", "second"))
  val element = queue.element()
  val removed = queue.remove()
  val peeked = queue.peek()
  queue.clear()
  println("abstractqueue:" + element + ":" + removed + ":" + peeked + ":" + queue.isEmpty())

  val leftSet = new MutableIntSet(Array(1, 2, 3))
  leftSet.removeAll(new MutableIntSet(Array(2, 4)))
  val rightSet = new MutableIntSet(Array(1, 3))
  println("abstractset:" + leftSet.equals(rightSet) + ":" + joinCollection(leftSet))

  val sequential = new SequentialStringList(new MutableStringList(Array("one", "two")))
  sequential.add(2, "three")
  sequential.set(0, "one!")
  val removedSequential = sequential.remove(1)
  sequential.add(1, "two?")
  println(
    "abstractsequential:" + removedSequential + ":" +
      joinList(sequential) + ":" + sequential.get(2)
  )
