package java.util

trait Enumeration[E]:
  def hasMoreElements(): Boolean
  def nextElement(): E

  // JDK default method, added in Java 9.
  def asIterator(): Iterator[E] = new Iterator[E]:
    def hasNext(): Boolean = hasMoreElements()
    def next(): E = nextElement()
