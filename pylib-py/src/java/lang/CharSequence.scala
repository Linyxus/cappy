package java.lang

trait CharSequence:
  def length(): scala.Int
  def charAt(index: scala.Int): scala.Char
  def subSequence(start: scala.Int, end: scala.Int): CharSequence
  def toString(): String
  def isEmpty(): scala.Boolean = length() == 0

  // JDK default methods. Stdlib references these by name; no pos-py test
  // actually consumes the stream. Throwing default suffices for linking.
  def chars(): java.util.stream.IntStream =
    throw new UnsupportedOperationException("CharSequence.chars() is a link-time stub")

  def codePoints(): java.util.stream.IntStream =
    throw new UnsupportedOperationException("CharSequence.codePoints() is a link-time stub")

object CharSequence:
  def ofArray(array: Array[Char]): CharSequence =
    new CharSequence:
      def length(): scala.Int = array.length
      def charAt(index: scala.Int): scala.Char = array(index)
      def subSequence(start: scala.Int, end: scala.Int): CharSequence =
        val sliceLength = end - start
        val copied = new Array[Char](sliceLength)
        var i = 0
        while i < sliceLength do
          copied(i) = array(start + i)
          i += 1
        ofArray(copied)
      override def toString(): String =
        var result = ""
        var i = 0
        while i < array.length do
          result += array(i)
          i += 1
        result
