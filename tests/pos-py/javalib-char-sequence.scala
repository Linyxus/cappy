final class MarkerCharSequence(private val value: String) extends java.lang.CharSequence:
  def length(): Int = value.length

  def charAt(index: Int): Char = value.charAt(index)

  def subSequence(start: Int, end: Int): java.lang.CharSequence =
    new MarkerCharSequence(value.substring(start, end))

  override def toString(): String = value

@main def markersCharSequence(): Unit =
  val chars = new MarkerCharSequence("abcd")
  println(
    "char-sequence:" + chars.length() + ":" + chars.charAt(1) + ":" +
      chars.subSequence(1, 3).toString() + ":" + chars.isEmpty()
  )
