package java.util

class StringTokenizer(str: String, private var delim: String, returnDelims: Boolean)
    extends java.util.Enumeration[Object] {

  def this(str: String) = this(str, " \t\n\r\f", false)
  def this(str: String, delim: String) = this(str, delim, false)

  private var position = 0
  private val length = str.length

  def hasMoreTokens(): Boolean =
    position < length && (returnDelims || !remainingAreDelims())

  def nextToken(): String =
    ensureAvailable()

    if returnDelims && isDelimAt(position) then
      val ret = str.substring(position, position + 1)
      position += 1
      ret
    else
      while position < length && isDelimAt(position) do
        position += 1

      ensureAvailable()

      val start = position
      while position < length && !isDelimAt(position) do
        position += 1
      str.substring(start, position)

  def nextToken(delim: String): String =
    this.delim = delim
    nextToken()

  def hasMoreElements(): Boolean = hasMoreTokens()

  def nextElement(): Object = nextToken()

  def countTokens(): Int = {
    var count = 0
    var inToken = false
    var i = position
    while i < length do
      if isDelimAt(i) then
        if returnDelims then count += 1
        if inToken then
          count += 1
          inToken = false
      else
        inToken = true
      i += 1
    if inToken then count += 1
    count
  }

  private def ensureAvailable(): Unit =
    if position >= length then throw new NoSuchElementException()

  private def isDelimAt(index: Int): Boolean =
    delim.indexOf(str.substring(index, index + 1), 0) >= 0

  private def remainingAreDelims(): Boolean = {
    var i = position
    var restAreDelims = true
    while i < length && restAreDelims do
      if !isDelimAt(i) then restAreDelims = false
      i += 1
    restAreDelims
  }
}
