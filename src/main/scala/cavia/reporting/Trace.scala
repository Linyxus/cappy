package cavia.reporting

object trace:
  private var indentLevel: Int = 0
  def apply[T](desc: String)(op: => T): T =
    println(s"${"  " * indentLevel}==> $desc")
    indentLevel += 1
    try
      val result = op
      println(s"${"  " * (indentLevel - 1)}<== $desc = $result")
      result
    finally
      indentLevel -= 1
