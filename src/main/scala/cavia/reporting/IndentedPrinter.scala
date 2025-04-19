package cavia.reporting

trait IndentedPrinter:
  val indentStr: String = "  "
  private var indentLevel: Int = 0
  private var justNewlined: Boolean = true
  var buf: StringBuilder = StringBuilder()

  def indented[T](op: => T): T =
    val oldLevel = indentLevel
    indentLevel += 1
    try op
    finally indentLevel = oldLevel

  def newline(): Unit =
    if !justNewlined then
      buf ++= "\n"
      justNewlined = true

  def print(s: String): Unit =
    if justNewlined then
      buf ++= indentStr * indentLevel
      justNewlined = false
    buf ++= s

  def println(s: String): Unit =
    print(s)
    newline()

  def result(): String = buf.result()
    