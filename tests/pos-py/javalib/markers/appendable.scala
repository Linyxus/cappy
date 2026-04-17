final class MarkerAppendable extends java.lang.Appendable:
  private var content = ""

  def append(c: Char): java.lang.Appendable =
    content += c
    this

  def append(csq: java.lang.CharSequence): java.lang.Appendable =
    content += csq.toString()
    this

  def append(csq: java.lang.CharSequence, start: Int, end: Int): java.lang.Appendable =
    content += csq.subSequence(start, end).toString()
    this

  override def toString(): String = content

final class MarkerAppendableSeq(private val value: String) extends java.lang.CharSequence:
  def length(): Int = value.length

  def charAt(index: Int): Char = value.charAt(index)

  def subSequence(start: Int, end: Int): java.lang.CharSequence =
    new MarkerAppendableSeq(value.substring(start, end))

  override def toString(): String = value

@main def markersAppendable(): Unit =
  val appendable = new MarkerAppendable
  val sequence = new MarkerAppendableSeq("abcd")
  appendable.append('Z').append(sequence, 0, 3)
  println("appendable:" + appendable.toString())
