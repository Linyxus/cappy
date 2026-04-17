final class MarkerListIterator extends java.util.ListIterator[Int]:
  private val data = Array(10, 20)
  private var cursor = 0

  def add(e: Int): Unit = ()

  def hasNext(): Boolean = cursor < data.length

  def next(): Int =
    val value = data(cursor)
    cursor += 1
    value

  def hasPrevious(): Boolean = cursor > 0

  def previous(): Int =
    cursor -= 1
    data(cursor)

  def previousIndex(): Int = cursor - 1

  def nextIndex(): Int = cursor

  override def remove(): Unit = ()

  def set(e: Int): Unit =
    data(cursor - 1) = e

@main def markersListIterator(): Unit =
  val iterator = new MarkerListIterator
  val first = iterator.next()
  iterator.set(11)
  val previous = iterator.previous()
  println("list-iterator:" + first + ":" + previous + ":" + iterator.nextIndex())
