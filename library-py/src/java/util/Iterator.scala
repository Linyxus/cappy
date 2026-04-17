package java.util

import java.util.function.Consumer

trait Iterator[E]:
  def hasNext(): Boolean
  def next(): E

  def remove(): Unit =
    throw new UnsupportedOperationException("remove")

  def forEachRemaining(action: Consumer[_ >: E]): Unit =
    while hasNext() do
      action.accept(next())
