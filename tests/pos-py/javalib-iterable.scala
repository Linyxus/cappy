import java.util.Iterator
import java.util.function.Consumer

final class MarkerIterable extends java.lang.Iterable[Int]:
  def iterator(): Iterator[Int] =
    new Iterator[Int]:
      private val data = Array(1, 2, 3)
      private var index = 0

      def hasNext(): Boolean = index < data.length

      def next(): Int =
        val value = data(index)
        index += 1
        value

@main def markersIterable(): Unit =
  var total = 0
  val consumer: Consumer[Int] = value => total += value
  new MarkerIterable().forEach(consumer)
  println("iterable:" + total)
