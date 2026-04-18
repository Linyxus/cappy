package java.lang

private object StackTraceElementSupport:
  def hashOrZero(value: Any): Int =
    if value == null then 0 else value.hashCode()

/* The primary constructor, taking a `columnNumber`, is not part of the JDK
 * API. It is used internally in `java.lang.StackTrace`.
 */
final class StackTraceElement(
    declaringClass: String,
    methodName: String,
    fileName: String,
    lineNumber: Int,
    private[this] var columnNumber: Int = -1
) extends AnyRef with java.io.Serializable:

  def getFileName(): String = fileName
  def getLineNumber(): Int = lineNumber
  def getClassName(): String = declaringClass
  def getMethodName(): String = methodName
  def isNativeMethod(): scala.Boolean = false

  // Not part of the JDK API, accessible through reflection.
  def getColumnNumber(): Int = columnNumber

  // Not part of the JDK API, accessible through reflection.
  @deprecated("old internal API; use the constructor with a column number instead", "1.11.0")
  def setColumnNumber(columnNumber: Int): Unit =
    this.columnNumber = columnNumber

  override def equals(that: Any): scala.Boolean = that match
    case that: StackTraceElement =>
      (getFileName() == that.getFileName()) &&
      (getLineNumber() == that.getLineNumber()) &&
      (getColumnNumber() == that.getColumnNumber()) &&
      (getClassName() == that.getClassName()) &&
      (getMethodName() == that.getMethodName())
    case _ =>
      false

  override def toString(): String =
    var result = ""
    if declaringClass != "<pycode>" then
      result += declaringClass + "."
    result += methodName
    if fileName == null then
      if isNativeMethod() then
        result += "(Native Method)"
      else
        result += "(Unknown Source)"
    else
      result += "(" + fileName
      if lineNumber >= 0 then
        result += ":" + lineNumber
        if columnNumber >= 0 then
          result += ":" + columnNumber
      result += ")"
    result

  override def hashCode(): Int =
    StackTraceElementSupport.hashOrZero(declaringClass) ^
    StackTraceElementSupport.hashOrZero(methodName) ^
    StackTraceElementSupport.hashOrZero(fileName) ^
    lineNumber ^
    columnNumber
