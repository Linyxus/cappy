final class MarkerEnumeration extends java.util.Enumeration[String]:
  private val data = Array("x", "y")
  private var index = 0

  def hasMoreElements(): Boolean = index < data.length

  def nextElement(): String =
    val value = data(index)
    index += 1
    value

@main def markersEnumeration(): Unit =
  val enumeration = new MarkerEnumeration
  var result = ""
  while enumeration.hasMoreElements() do
    result += enumeration.nextElement()
  println("enumeration:" + result)
