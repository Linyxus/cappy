package java.lang

trait CharSequence:
  def length(): scala.Int
  def charAt(index: scala.Int): scala.Char
  def subSequence(start: scala.Int, end: scala.Int): CharSequence
  def toString(): String
  def isEmpty(): scala.Boolean = length() == 0

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
