import java.util.function.Consumer

final class MarkerIterator(private val data: Array[Int]) extends java.util.Iterator[Int]:
  private var index = 0

  def hasNext(): Boolean = index < data.length

  def next(): Int =
    val value = data(index)
    index += 1
    value

final class MarkerIterable(private val data: Array[Int]) extends java.lang.Iterable[Int]:
  def iterator(): java.util.Iterator[Int] =
    new MarkerIterator(data)

final class MarkerEnumeration(private val data: Array[String]) extends java.util.Enumeration[String]:
  private var index = 0

  def hasMoreElements(): Boolean = index < data.length

  def nextElement(): String =
    val value = data(index)
    index += 1
    value

final class MarkerListIterator(private val data: Array[Int]) extends java.util.ListIterator[Int]:
  private var cursor = 0
  private var lastReturned = -1

  def add(e: Int): Unit = ()

  def hasNext(): Boolean = cursor < data.length

  def next(): Int =
    val value = data(cursor)
    lastReturned = cursor
    cursor += 1
    value

  def hasPrevious(): Boolean = cursor > 0

  def previous(): Int =
    cursor -= 1
    lastReturned = cursor
    data(cursor)

  def previousIndex(): Int = cursor - 1

  def nextIndex(): Int = cursor

  override def remove(): Unit = ()

  def set(e: Int): Unit =
    if lastReturned >= 0 then
      data(lastReturned) = e

def removeThrowsUnsupported(iterator: java.util.Iterator[Int]): Boolean =
  try
    iterator.remove()
    false
  catch
    case _: UnsupportedOperationException => true

@main def javalibMarkersIterators(): Unit =
  var iterableDigits = 0
  val iterableConsumer: Consumer[Int] =
    value => iterableDigits = iterableDigits * 10 + value
  new MarkerIterable(Array(1, 2, 3)).forEach(iterableConsumer)
  println("iterable:" + iterableDigits)

  val iterator = new MarkerIterator(Array(4, 5, 6))
  val first = iterator.next()
  var remainingDigits = 0
  val iteratorConsumer: Consumer[Int] =
    value => remainingDigits = remainingDigits * 10 + value
  iterator.forEachRemaining(iteratorConsumer)
  println("iterator:" + first + ":" + remainingDigits)

  println("remove:" + removeThrowsUnsupported(new MarkerIterator(Array(9))))

  val enumeration = new MarkerEnumeration(Array("x", "y"))
  var elements = ""
  while enumeration.hasMoreElements() do
    elements += enumeration.nextElement()
  println("enumeration:" + elements + ":" + enumeration.hasMoreElements())

  val listIterator = new MarkerListIterator(Array(10, 20, 30))
  val start =
    listIterator.nextIndex().toString() + ":" +
      listIterator.previousIndex().toString() + ":" +
      listIterator.hasPrevious().toString()
  val firstValue = listIterator.next()
  val afterFirst =
    listIterator.nextIndex().toString() + ":" +
      listIterator.previousIndex().toString() + ":" +
      listIterator.hasPrevious().toString()
  val secondValue = listIterator.next()
  listIterator.set(21)
  val previousValue = listIterator.previous()
  val afterPrevious =
    listIterator.nextIndex().toString() + ":" +
      listIterator.previousIndex().toString() + ":" +
      listIterator.hasNext().toString()
  println(
    "list-iterator:" + start + ":" + firstValue + ":" + afterFirst + ":" +
      secondValue + ":" + previousValue + ":" + afterPrevious
  )
