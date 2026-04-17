package java.lang

import java.util.Iterator
import java.util.function.Consumer

trait Iterable[T]:
  def iterator(): Iterator[T]

  def forEach(action: Consumer[_ >: T]): Unit =
    val iter = iterator()
    while iter.hasNext() do
      action.accept(iter.next())
