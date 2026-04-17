final class MarkerCharSequence(private val value: String) extends java.lang.CharSequence:
  def length(): Int = value.length

  def charAt(index: Int): Char = value.charAt(index)

  def subSequence(start: Int, end: Int): java.lang.CharSequence =
    new MarkerCharSequence(value.substring(start, end))

  override def toString(): String = value

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

final class MarkerReadable extends java.lang.Readable:
  private var callCount = 0

  def read(cb: java.nio.CharBuffer): Int =
    callCount += 1
    if callCount == 1 then 7 else -1

  def calls(): Int = callCount

@main def javalibMarkersSequences(): Unit =
  val empty = new MarkerCharSequence("")
  val word = new MarkerCharSequence("scala")
  val middle = word.subSequence(1, 4)
  println(
    "char-sequence:" + empty.isEmpty() + ":" + word.isEmpty() + ":" +
      word.charAt(2) + ":" + middle.toString() + ":" + middle.isEmpty()
  )

  val appendable = new MarkerAppendable
  appendable.append('[').append(middle).append('|').append(word, 0, 2).append(']')
  val replay = new MarkerAppendable
  replay.append(new MarkerCharSequence(appendable.toString()).subSequence(1, 4))
  println("appendable:" + appendable.toString() + ":" + replay.toString())

  val readable = new MarkerReadable
  val asReadable: java.lang.Readable = readable
  val nullBuffer = null.asInstanceOf[java.nio.CharBuffer]
  println(
    "readable:" + asReadable.read(nullBuffer) + ":" +
      asReadable.read(nullBuffer) + ":" + readable.calls()
  )
