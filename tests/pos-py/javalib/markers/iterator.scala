import java.util.function.Consumer

final class MarkerIterator extends java.util.Iterator[Int]:
  private val data = Array(1, 2, 3)
  private var index = 0

  def hasNext(): Boolean = index < data.length

  def next(): Int =
    val value = data(index)
    index += 1
    value

@main def markersIterator(): Unit =
  var total = 0
  val consumer: Consumer[Int] = value => total += value
  val iterator = new MarkerIterator
  iterator.forEachRemaining(consumer)
  println("iterator:" + total)
