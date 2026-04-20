import java.util.{AbstractCollection, AbstractList, AbstractMap, AbstractQueue, AbstractSet, Collection, Comparator, Deque, Dictionary, Iterator, List, Map, NavigableMap, NavigableSet, Queue, RandomAccess, SequencedCollection, SequencedMap, SequencedSet, SortedMap, SortedSet}
import java.util.function.{BiConsumer, BiFunction, Function, Predicate, UnaryOperator}

private def joinIterator[E](iterator: Iterator[E], separator: String = ","): String =
  var result = ""
  var first = true
  while iterator.hasNext() do
    if !first then
      result += separator
    result += String.valueOf(iterator.next())
    first = false
  result

private def joinCollection[E](collection: Collection[E]): String =
  joinIterator(collection.iterator())

private def joinEntries(map: Map[String, String]): String =
  val parts = new java.lang.StringBuilder()
  map.forEach(
    new BiConsumer[String, String]:
      def accept(key: String, value: String): Unit =
        if parts.length() != 0 then
          parts.append(";")
        parts.append(key)
        parts.append("=")
        parts.append(value)
  )
  parts.toString()

private def compareKeys(left: String, right: String): Int =
  java.lang.Character.compare(left.charAt(0), right.charAt(0))

final class ContractStringCollection(initial: Array[String]) extends AbstractCollection[String]:
  private var data = new Array[String](8)
  private var size0 = 0

  private var index = 0
  while index < initial.length do
    add(initial(index))
    index += 1

  override def add(value: String): Boolean =
    ensureCapacity(size0 + 1)
    data(size0) = value
    size0 += 1
    true

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

final class ContractStringList(initial: Array[String]) extends AbstractList[String] with RandomAccess:
  private var data = new Array[String](8)
  private var size0 = 0

  private var index = 0
  while index < initial.length do
    add(size0, initial(index))
    index += 1

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

final class ContractStringMap(initialKeys: Array[String], initialValues: Array[String])
    extends AbstractMap[String, String] with NavigableMap[String, String]:
  private var keys = new Array[String](8)
  private var storedValues = new Array[String](8)
  private var size0 = 0

  private var index = 0
  while index < initialKeys.length do
    put(initialKeys(index), initialValues(index))
    index += 1

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
          private var cursor = 0
          private var last = -1

          def hasNext(): Boolean = cursor < size0

          def next(): Map.Entry[String, String] =
            if !hasNext() then
              throw new java.util.NoSuchElementException()
            val slot = cursor
            last = slot
            cursor += 1
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
                    java.util.Objects.equals(getKey(), entry.getKey()) &&
                    java.util.Objects.equals(getValue(), entry.getValue())
                  case _ =>
                    false

              override def hashCode(): Int =
                java.util.Objects.hashCode(getKey()) ^ java.util.Objects.hashCode(getValue())

              override def toString(): String =
                getKey() + "=" + getValue()

          override def remove(): Unit =
            if last < 0 then
              throw new IllegalStateException()
            removeAt(last)
            cursor = last
            last = -1

  def comparator(): Comparator[_ >: String] =
    new Comparator[String]:
      def compare(left: String, right: String): Int =
        compareKeys(left, right)

  def firstKey(): String = keys(0)

  def lastKey(): String = keys(size0 - 1)

  def lowerEntry(key: String): Map.Entry[String, String] =
    entryAt(relativeIndex(key) - 1)

  def lowerKey(key: String): String =
    keyAt(relativeIndex(key) - 1)

  def floorEntry(key: String): Map.Entry[String, String] =
    entryAt(relativeIndex(key))

  def floorKey(key: String): String =
    keyAt(relativeIndex(key))

  def ceilingEntry(key: String): Map.Entry[String, String] =
    entryAt(relativeIndex(key))

  def ceilingKey(key: String): String =
    keyAt(relativeIndex(key))

  def higherEntry(key: String): Map.Entry[String, String] =
    entryAt(relativeIndex(key) + 1)

  def higherKey(key: String): String =
    keyAt(relativeIndex(key) + 1)

  def firstEntry(): Map.Entry[String, String] = entryAt(0)

  def lastEntry(): Map.Entry[String, String] = entryAt(size0 - 1)

  def pollFirstEntry(): Map.Entry[String, String] = firstEntry()

  def pollLastEntry(): Map.Entry[String, String] = lastEntry()

  def descendingMap(): NavigableMap[String, String] = this

  def navigableKeySet(): NavigableSet[String] = throw new UnsupportedOperationException()

  def descendingKeySet(): NavigableSet[String] = throw new UnsupportedOperationException()

  def subMap(fromKey: String, fromInclusive: Boolean, toKey: String, toInclusive: Boolean): NavigableMap[String, String] =
    this

  def headMap(toKey: String, inclusive: Boolean): NavigableMap[String, String] =
    this

  def tailMap(fromKey: String, inclusive: Boolean): NavigableMap[String, String] =
    this

  def subMap(fromKey: String, toKey: String): SortedMap[String, String] =
    this

  def headMap(toKey: String): SortedMap[String, String] =
    this

  def tailMap(fromKey: String): SortedMap[String, String] =
    this

  private def findIndex(key: String): Int =
    var cursor = 0
    while cursor < size0 do
      if java.util.Objects.equals(keys(cursor), key) then
        return cursor
      cursor += 1
    -1

  private def relativeIndex(key: String): Int =
    val slot = findIndex(key)
    if slot >= 0 then slot else 0

  private def entryAt(index: Int): Map.Entry[String, String] =
    new AbstractMap.SimpleEntry[String, String](keys(index), storedValues(index))

  private def keyAt(index: Int): String =
    keys(index)

  private def ensureCapacity(required: Int): Unit =
    if required > keys.length then
      val grownKeys = new Array[String](required * 2)
      val grownValues = new Array[String](required * 2)
      System.arraycopy(keys, 0, grownKeys, 0, size0)
      System.arraycopy(storedValues, 0, grownValues, 0, size0)
      keys = grownKeys
      storedValues = grownValues

  private def removeAt(index: Int): Unit =
    var cursor = index + 1
    while cursor < size0 do
      keys(cursor - 1) = keys(cursor)
      storedValues(cursor - 1) = storedValues(cursor)
      cursor += 1
    size0 -= 1
    keys(size0) = null
    storedValues(size0) = null

final class ContractDeque(initial: Array[String]) extends AbstractQueue[String] with Deque[String]:
  private var data = new Array[String](8)
  private var size0 = 0

  private var index = 0
  while index < initial.length do
    addLast(initial(index))
    index += 1

  def size(): Int = size0

  def iterator(): Iterator[String] =
    new Iterator[String]:
      private var cursor = 0

      def hasNext(): Boolean = cursor < size0

      def next(): String =
        val value = data(cursor)
        cursor += 1
        value

  def descendingIterator(): Iterator[String] =
    new Iterator[String]:
      private var cursor = size0 - 1

      def hasNext(): Boolean = cursor >= 0

      def next(): String =
        val value = data(cursor)
        cursor -= 1
        value

  def offer(value: String): Boolean = offerLast(value)

  def offerFirst(value: String): Boolean =
    ensureCapacity(size0 + 1)
    var cursor = size0
    while cursor > 0 do
      data(cursor) = data(cursor - 1)
      cursor -= 1
    data(0) = value
    size0 += 1
    true

  def offerLast(value: String): Boolean =
    ensureCapacity(size0 + 1)
    data(size0) = value
    size0 += 1
    true

  def addFirst(value: String): Unit = offerFirst(value)

  def addLast(value: String): Unit = offerLast(value)

  def removeFirst(): String =
    val value = data(0)
    var cursor = 1
    while cursor < size0 do
      data(cursor - 1) = data(cursor)
      cursor += 1
    size0 -= 1
    data(size0) = null
    value

  def removeLast(): String =
    size0 -= 1
    val value = data(size0)
    data(size0) = null
    value

  def pollFirst(): String =
    if size0 == 0 then null else removeFirst()

  def pollLast(): String =
    if size0 == 0 then null else removeLast()

  def getFirst(): String = data(0)

  def getLast(): String = data(size0 - 1)

  def peekFirst(): String =
    if size0 == 0 then null else data(0)

  def peekLast(): String =
    if size0 == 0 then null else data(size0 - 1)

  def removeFirstOccurrence(value: Any): Boolean =
    remove(value)

  def removeLastOccurrence(value: Any): Boolean =
    var cursor = size0 - 1
    while cursor >= 0 do
      if java.util.Objects.equals(data(cursor), value) then
        removeAt(cursor)
        return true
      cursor -= 1
    false

  override def add(value: String): Boolean = {
    addLast(value)
    true
  }

  override def remove(): String = removeFirst()

  def poll(): String = pollFirst()

  override def element(): String = getFirst()

  def peek(): String = peekFirst()

  def push(value: String): Unit = addFirst(value)

  def pop(): String = removeFirst()

  override def remove(value: Any): Boolean =
    var cursor = 0
    while cursor < size0 do
      if java.util.Objects.equals(data(cursor), value) then
        removeAt(cursor)
        return true
      cursor += 1
    false

  override def contains(value: Any): Boolean =
    var cursor = 0
    while cursor < size0 do
      if java.util.Objects.equals(data(cursor), value) then
        return true
      cursor += 1
    false

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

final class ContractNavigableSet(initial: Array[Int]) extends AbstractSet[Int] with NavigableSet[Int]:
  private var data = new Array[Int](8)
  private var size0 = 0

  private var index = 0
  while index < initial.length do
    add(initial(index))
    index += 1

  def size(): Int = size0

  override def contains(value: Any): Boolean =
    if !value.isInstanceOf[Int] then
      false
    else
      val target = value.asInstanceOf[Int]
      var cursor = 0
      while cursor < size0 do
        if data(cursor) == target then
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

      def hasNext(): Boolean = cursor < size0

      def next(): Int =
        val value = data(cursor)
        cursor += 1
        value

  def descendingSet(): NavigableSet[Int] = this

  def descendingIterator(): Iterator[Int] =
    new Iterator[Int]:
      private var cursor = size0 - 1

      def hasNext(): Boolean = cursor >= 0

      def next(): Int =
        val value = data(cursor)
        cursor -= 1
        value

  def comparator(): Comparator[_ >: Int] =
    new Comparator[Int]:
      def compare(left: Int, right: Int): Int =
        java.lang.Integer.compare(left, right)

  def subSet(fromElement: Int, fromInclusive: Boolean, toElement: Int, toInclusive: Boolean): NavigableSet[Int] =
    this

  def headSet(toElement: Int, inclusive: Boolean): NavigableSet[Int] =
    this

  def tailSet(fromElement: Int, inclusive: Boolean): NavigableSet[Int] =
    this

  def subSet(fromElement: Int, toElement: Int): SortedSet[Int] =
    this

  def headSet(toElement: Int): SortedSet[Int] =
    this

  def tailSet(fromElement: Int): SortedSet[Int] =
    this

  def first(): Int = data(0)

  def last(): Int = data(size0 - 1)

  def lower(value: Int): Int = value - 1

  def floor(value: Int): Int = value

  def ceiling(value: Int): Int = value

  def higher(value: Int): Int = value + 1

  def pollFirst(): Int = data(0)

  def pollLast(): Int = data(size0 - 1)

  private def ensureCapacity(required: Int): Unit =
    if required > data.length then
      val grown = new Array[Int](required * 2)
      System.arraycopy(data, 0, grown, 0, size0)
      data = grown

final class ContractDictionary extends Dictionary[String, String]:
  private val storedKeys = new Array[String](2)
  private val storedValues = new Array[String](2)
  storedKeys(0) = "a"
  storedValues(0) = "one"
  storedKeys(1) = "b"
  storedValues(1) = "two"

  def size(): Int = 2

  def isEmpty(): Boolean = false

  def keys(): java.util.Enumeration[String] =
    new java.util.Enumeration[String]:
      private var index = 0

      def hasMoreElements(): Boolean = index < storedKeys.length

      def nextElement(): String =
        val value = storedKeys(index)
        index += 1
        value

  def elements(): java.util.Enumeration[String] =
    new java.util.Enumeration[String]:
      private var index = 0

      def hasMoreElements(): Boolean = index < storedValues.length

      def nextElement(): String =
        val value = storedValues(index)
        index += 1
        value

  def get(key: Any): String =
    if "a" == key then "one"
    else if "b" == key then "two"
    else null

  def put(key: String, value: String): String =
    null

  def remove(key: Any): String =
    if "a" == key then "one"
    else if "b" == key then "two"
    else null

@main def javalibUtilCollectionContracts(): Unit =
  val collection = new ContractStringCollection(Array("keep", "drop", "stay"))
  collection.removeIf(
    new Predicate[String]:
      def test(value: String): Boolean = value == "drop"
  )
  println("collection:" + joinCollection(collection))

  val list = new ContractStringList(Array("bbb", "a", "cc"))
  list.replaceAll(
    new UnaryOperator[String]:
      def apply(value: String): String = value + "!"
  )
  val replaced = joinCollection(list)
  list.sort(Comparator.comparingInt[String]((value: String) => value.length))
  println("list:" + replaced + ":" + joinCollection(list))

  val map = new ContractStringMap(Array("a"), Array("one"))
  val missing = map.getOrDefault("missing", "fallback")
  val existing = map.putIfAbsent("a", "uno?")
  val created = map.computeIfAbsent("b", new Function[String, String]:
    def apply(key: String): String = key + "!"
  )
  val present = map.computeIfPresent("b", new BiFunction[String, String, String]:
    def apply(key: String, value: String): String = value + "?"
  )
  map.replace("a", "one", "uno")
  val computed = map.compute("c", new BiFunction[String, String, String]:
    def apply(key: String, value: String): String =
      if value == null then "seed" else value + "!"
  )
  val merged = map.merge("c", "+", new BiFunction[String, String, String]:
    def apply(left: String, right: String): String = left + right
  )
  println(
    "map:" + missing + ":" + existing + ":" + created + ":" +
      present + ":" + computed + ":" + merged + ":" + joinEntries(map)
  )

  val queue: Queue[String] = new ContractDeque(Array("left", "right"))
  println("queue:" + queue.peek() + ":" + queue.poll() + ":" + queue.element())

  val deque = new ContractDeque(Array("center"))
  deque.addFirst("left")
  deque.addLast("right")
  println("deque:" + deque.removeLast() + ":" + deque.getFirst() + ":" + deque.getLast())

  val navigableMap = map.asInstanceOf[NavigableMap[String, String]]
  println("sortedmap:" + navigableMap.firstKey() + ":" + navigableMap.lastKey())
  println("navigablemap:" + navigableMap.ceilingKey("b") + ":" + navigableMap.higherKey("b"))

  val navigableSet = new ContractNavigableSet(Array(1, 2, 3))
  val sortedSet = navigableSet.asInstanceOf[SortedSet[Int]]
  println("sortedset:" + sortedSet.first() + ":" + sortedSet.last())
  println("navigableset:" + navigableSet.ceiling(2) + ":" + navigableSet.higher(2))

  val sequencedCollection = navigableSet.asInstanceOf[SequencedCollection[Int]]
  val sequencedMap = map.asInstanceOf[SequencedMap[String, String]]
  val sequencedSet = navigableSet.asInstanceOf[SequencedSet[Int]]
  println("sequencedcollection:" + sequencedCollection.size())
  println("sequencedmap:" + sequencedMap.get("a"))
  println("sequencedset:" + sequencedSet.contains(3))

  val dictionary = new ContractDictionary()
  println(
    "dictionary:" + dictionary.get("a") + ":" +
      dictionary.get("b") + ":" + String.valueOf(dictionary.get("z"))
  )
