package java.nio.charset

final class CodingErrorAction private (private val label: String):
  override def toString(): String =
    label

object CodingErrorAction:
  val IGNORE = new CodingErrorAction("IGNORE")
  val REPLACE = new CodingErrorAction("REPLACE")
  val REPORT = new CodingErrorAction("REPORT")
