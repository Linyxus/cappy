package java.lang

private[java] object ThrowablesSupport:
  def stringValueOf(value: Any): String =
    if value == null then "null" else value.toString()

  def indexMessage(prefix: String, detail: Any): String | Null =
    detail match
      case null        => null
      case index: Int  => prefix + index
      case index: scala.Long => prefix + index
      case other       => stringValueOf(other)

  def exceptionInInitializerMessage(detail: Any): String | Null =
    detail match
      case _: Throwable => null
      case other        => throwableMessage(other, null)

  def exceptionInInitializerCause(detail: Any): Throwable | Null =
    detail match
      case cause: Throwable => cause
      case _                => null

  // Sentinel used by `AssertionError`'s no-arg constructor to signal
  // that no detail message was supplied. The 1-arg ctor `AssertionError(null)`
  // must produce `getMessage() == "null"` (per JVM `String.valueOf((Object) null)`),
  // while the no-arg ctor must produce `getMessage() == null`. Auxiliary ctors
  // must chain into the primary, so we piggyback this sentinel through it
  // and unwrap in `assertionErrorMessage` / `assertionErrorCause`.
  object NoMessage

  def assertionErrorMessage(detail: Any): String | Null =
    detail match
      case NoMessage => null
      case other     => stringValueOf(other)

  def assertionErrorCause(detail: Any, explicitCause: Throwable | Null): Throwable | Null =
    if explicitCause != null then explicitCause
    else
      detail match
        case NoMessage        => null
        case cause: Throwable => cause
        case _                => null

  def throwableMessage(primary: Any, explicitCause: Throwable | Null): String | Null =
    if primary == null then
      if explicitCause == null then null else explicitCause.toString()
    else
      primary match
        case cause: Throwable if explicitCause == null => cause.toString()
        case other => stringValueOf(other)

  def throwableCause(primary: Any, explicitCause: Throwable | Null): Throwable | Null =
    if explicitCause != null then explicitCause
    else
      primary match
        case cause: Throwable => cause
        case _                => null

  def requireNonNull[T <: AnyRef](value: T | Null): T =
    if value == null then throw new NullPointerException()
    value.asInstanceOf[T]

  // The runtime-preamble `_scpy_require_monitor` delegates here when
  // the current thread does not own the monitor. Keeping the throw on
  // the Scala side makes `IllegalMonitorStateException` linker-
  // reachable wherever `ThrowablesSupport` is (i.e., everywhere — via
  // `requireNonNull` which every ported class uses). Fixes the
  // `javalib-object-wait-fresh.scala` regression where
  // `IllegalMonitorStateException` was undefined at the raise site.
  def throwIllegalMonitorState(): Unit =
    throw new IllegalMonitorStateException()

  def copyThrowableArray(values: Array[Throwable] | Null): Array[Throwable] =
    if values == null then
      new Array[Throwable](0)
    else
      val copied = new Array[Throwable](values.length)
      var i = 0
      while i < values.length do
        copied(i) = values(i)
        i += 1
      copied

  def copyStackTraceArray(values: Array[StackTraceElement] | Null): Array[StackTraceElement] =
    if values == null then
      new Array[StackTraceElement](0)
    else
      val copied = new Array[StackTraceElement](values.length)
      var i = 0
      while i < values.length do
        copied(i) = values(i)
        i += 1
      copied

class Throwable (
    primary: Any = null,
    private var e: Throwable | Null = null,
    enableSuppression: scala.Boolean = true,
    private val writableStackTrace: scala.Boolean = true
) extends Object with java.io.Serializable:

  def this() =
    this(null, null, true, true)

  // JDK-shaped ctor overloads. Scala's default-args don't generate
  // these as distinct PyIR signatures; stdlib (typechecked against JVM)
  // references them by JVM-style signatures like `<init>(Ljava_lang_String)`.
  // The `: Any` ascription on `message`/`cause` forces chained-call
  // resolution to the primary ctor rather than recursing.
  def this(message: String) = this(message: Any, null, true, true)
  def this(message: String, cause: Throwable) = this(message: Any, cause, true, true)
  def this(cause: Throwable) = this(null, cause, true, true)
  def this(message: String, cause: Throwable, enableSuppression: scala.Boolean, writableStackTrace: scala.Boolean) =
    this(message: Any, cause, enableSuppression, writableStackTrace)

  // Stored under a field name distinct from any auxiliary-ctor parameter
  // name. The aux ctors above accept a parameter spelled `message`; if the
  // field were also `message`, codegen for those aux ctors could shadow or
  // skip the field initializer, leaving `getMessage()` with no `self.message`
  // to read (see notes/issue-throwable-message-missing.md).
  private val msg: String | Null =
    ThrowablesSupport.throwableMessage(primary, e)

  private var capturedStackTrace: Any = _
  private var stackTrace: Array[StackTraceElement] | Null = _

  /* Use an Array so Throwable does not depend on Scala collections. */
  private var suppressed: Array[Throwable] | Null = _

  if writableStackTrace then
    fillInStackTrace()

  def initCause(cause: Throwable | Null): Throwable =
    e = cause
    this

  def getMessage(): String | Null = msg
  def getCause(): Throwable | Null = e
  def getLocalizedMessage(): String | Null = getMessage()

  def fillInStackTrace(): Throwable =
    if writableStackTrace then
      capturedStackTrace = StackTrace.capturePyError()
      stackTrace = null
    this

  private def resolveStackTrace(): Array[StackTraceElement] =
    if stackTrace == null then
      stackTrace =
        if writableStackTrace then StackTrace.extract(capturedStackTrace)
        else new Array[StackTraceElement](0)
    stackTrace.asInstanceOf[Array[StackTraceElement]]

  def getStackTrace(): Array[StackTraceElement] =
    ThrowablesSupport.copyStackTraceArray(resolveStackTrace())

  def setStackTrace(stackTrace: Array[StackTraceElement] | Null): Unit =
    if writableStackTrace then
      val checked = ThrowablesSupport.requireNonNull(stackTrace)
      var i = 0
      while i < checked.length do
        ThrowablesSupport.requireNonNull(checked(i))
        i += 1
      this.stackTrace = ThrowablesSupport.copyStackTraceArray(checked)
      this.capturedStackTrace = null

  def printStackTrace(): Unit =
    printStackTraceImpl()

  def printStackTrace(s: java.io.PrintStream): Unit =
    printStackTrace()

  def printStackTrace(s: java.io.PrintWriter): Unit =
    printStackTrace()

  private def printStackTraceImpl(): Unit =
    val currentTrace = resolveStackTrace()

    println(toString)

    if currentTrace.length != 0 then
      var i = 0
      while i < currentTrace.length do
        println("  at " + currentTrace(i))
        i += 1
    else
      println("  <no stack trace available>")

    var wCause: Throwable = this
    var nextCause = wCause.getCause()
    while nextCause != null && (wCause ne nextCause.asInstanceOf[Throwable]) do
      val parentTrace = wCause.resolveStackTrace()
      wCause = nextCause.asInstanceOf[Throwable]
      val thisTrace = wCause.resolveStackTrace()

      println("Caused by: " + wCause)

      if thisTrace.length != 0 then
        var sameFrameCount = 0
        while sameFrameCount < thisTrace.length &&
            sameFrameCount < parentTrace.length &&
            thisTrace(thisTrace.length - sameFrameCount - 1) ==
              parentTrace(parentTrace.length - sameFrameCount - 1)
        do
          sameFrameCount += 1

        if sameFrameCount > 0 then
          sameFrameCount -= 1

        val lengthToPrint = thisTrace.length - sameFrameCount
        var i = 0
        while i < lengthToPrint do
          println("  at " + thisTrace(i))
          i += 1

        if sameFrameCount > 0 then
          println("  ... " + sameFrameCount + " more")
      else
        println("  <no stack trace available>")

      nextCause = wCause.getCause()

  override def toString(): String =
    val className = getClass().getName()
    val currentMessage = getMessage()
    if currentMessage == null then className
    else className + ": " + currentMessage

  def addSuppressed(exception: Throwable): Unit =
    ThrowablesSupport.requireNonNull(exception)
    if exception eq this then
      throw new IllegalArgumentException(null)

    if enableSuppression then
      if suppressed == null then
        suppressed = new Array[Throwable](1)
        suppressed.asInstanceOf[Array[Throwable]](0) = exception
      else
        val currentSuppressed = suppressed.asInstanceOf[Array[Throwable]]
        val copied = new Array[Throwable](currentSuppressed.length + 1)
        var i = 0
        while i < currentSuppressed.length do
          copied(i) = currentSuppressed(i)
          i += 1
        copied(currentSuppressed.length) = exception
        suppressed = copied

  def getSuppressed(): Array[Throwable] =
    ThrowablesSupport.copyThrowableArray(suppressed)

class ThreadDeath() extends Error()

/* java.lang.*Error.java */

class AbstractMethodError(primary: Any = null) extends IncompatibleClassChangeError(primary)

class AssertionError(detailMessage: Any = null, cause: Throwable | Null = null)
    extends Error(
      ThrowablesSupport.assertionErrorMessage(detailMessage),
      ThrowablesSupport.assertionErrorCause(detailMessage, cause)
    ):
  // No-arg ctor must produce `getMessage() == null`, distinct from
  // `new AssertionError(null)` (which produces the string "null" per
  // `String.valueOf((Object) null)`). Auxiliary ctors must chain into
  // the primary, so the no-arg form passes the `NoMessage` sentinel
  // and `assertionErrorMessage` unwraps it back to `null`.
  def this() = this(ThrowablesSupport.NoMessage, null)
  def this(message: String) = this(message: Any, null)
  def this(message: Object) = this(message: Any, null)
  def this(message: scala.Boolean) = this(message: Any, null)
  def this(message: scala.Char) = this(message: Any, null)
  def this(message: scala.Int) = this(message: Any, null)
  def this(message: scala.Long) = this(message: Any, null)
  def this(message: scala.Float) = this(message: Any, null)
  def this(message: scala.Double) = this(message: Any, null)

class BootstrapMethodError(primary: Any = null, cause: Throwable | Null = null)
    extends LinkageError(primary, cause)

class ClassCircularityError(primary: Any = null) extends LinkageError(primary)

class ClassFormatError(primary: Any = null) extends LinkageError(primary)

class Error (
    primary: Any = null,
    cause: Throwable | Null = null,
    enableSuppression: scala.Boolean = true,
    writableStackTrace: scala.Boolean = true
) extends Throwable(primary, cause, enableSuppression, writableStackTrace)
:
  def this() =
    this(null, null, true, true)
  def this(message: String) = this(message: Any, null, true, true)
  def this(message: String, cause: Throwable) = this(message: Any, cause, true, true)
  def this(cause: Throwable) = this(null, cause, true, true)

class ExceptionInInitializerError(detail: Any = null)
    extends LinkageError(
      ThrowablesSupport.exceptionInInitializerMessage(detail),
      ThrowablesSupport.exceptionInInitializerCause(detail)
    ):
  def getException(): Throwable | Null =
    ThrowablesSupport.exceptionInInitializerCause(detail)

class IllegalAccessError(primary: Any = null) extends IncompatibleClassChangeError(primary)

class IncompatibleClassChangeError(primary: Any = null) extends LinkageError(primary)

class InstantiationError(primary: Any = null) extends IncompatibleClassChangeError(primary)

class InternalError(primary: Any = null) extends VirtualMachineError(primary)

class LinkageError(primary: Any = null, cause: Throwable | Null = null) extends Error(primary, cause)

class NoClassDefFoundError(primary: Any = null) extends LinkageError(primary)

class NoSuchFieldError(primary: Any = null) extends IncompatibleClassChangeError(primary)

class NoSuchMethodError(primary: Any = null) extends IncompatibleClassChangeError(primary)

class OutOfMemoryError(primary: Any = null) extends VirtualMachineError(primary):
  def this() = this(null: Any)
  def this(message: String) = this(message: Any)

class StackOverflowError(primary: Any = null) extends VirtualMachineError(primary):
  def this() = this(null: Any)
  def this(message: String) = this(message: Any)

class UnknownError(primary: Any = null) extends VirtualMachineError(primary)

class UnsatisfiedLinkError(primary: Any = null) extends LinkageError(primary)

class UnsupportedClassVersionError(primary: Any = null) extends ClassFormatError(primary)

class VerifyError(primary: Any = null) extends LinkageError(primary)

abstract class VirtualMachineError(primary: Any = null, cause: Throwable | Null = null)
    extends Error(primary, cause)

/* java.lang.*Exception.java */

class ArithmeticException(primary: Any = null) extends RuntimeException(primary):
  def this(message: String) = this(message: Any)

class ArrayIndexOutOfBoundsException(detail: Any = null)
    extends IndexOutOfBoundsException(ThrowablesSupport.indexMessage("Array index out of range: ", detail)):
  def this() = this(null: Any)
  def this(message: String) = this(message: Any)
  def this(index: scala.Int) = this(index: Any)

class ArrayStoreException(primary: Any = null) extends RuntimeException(primary)

class ClassCastException(primary: Any = null) extends RuntimeException(primary):
  def this(message: String) = this(message: Any)

class ClassNotFoundException(message: String | Null = null, private val exception: Throwable | Null = null)
    extends ReflectiveOperationException(message, exception):
  def getException(): Throwable | Null = exception
  override def getCause(): Throwable | Null = exception

class CloneNotSupportedException(primary: Any = null) extends Exception(primary)

class EnumConstantNotPresentException(e: Class[? <: Enum[?]], c: String)
    extends RuntimeException(e.getName() + "." + c):
  def enumType(): Class[? <: Enum[?]] = e
  def constantName(): String = c

class Exception (
    primary: Any = null,
    cause: Throwable | Null = null,
    enableSuppression: scala.Boolean = true,
    writableStackTrace: scala.Boolean = true
) extends Throwable(primary, cause, enableSuppression, writableStackTrace)
:
  def this() =
    this(null, null, true, true)
  def this(message: String) = this(message: Any, null, true, true)
  def this(message: String, cause: Throwable) = this(message: Any, cause, true, true)
  def this(cause: Throwable) = this(null, cause, true, true)

class IllegalAccessException(primary: Any = null) extends ReflectiveOperationException(primary)

class IllegalArgumentException(primary: Any = null, cause: Throwable | Null = null)
    extends RuntimeException(primary, cause)
:
  def this() =
    this(null, null)
  def this(message: String) = this(message: Any, null)
  def this(message: String, cause: Throwable) = this(message: Any, cause)
  def this(cause: Throwable) = this(null, cause)

class IllegalMonitorStateException(primary: Any = null) extends RuntimeException(primary)

class IllegalStateException(primary: Any = null, cause: Throwable | Null = null)
    extends RuntimeException(primary, cause)
:
  def this() =
    this(null, null)
  def this(message: String) = this(message: Any, null)
  def this(message: String, cause: Throwable) = this(message: Any, cause)
  def this(cause: Throwable) = this(null, cause)

class IllegalThreadStateException(primary: Any = null) extends IllegalArgumentException(primary)

class IndexOutOfBoundsException(detail: Any = null)
    extends RuntimeException(ThrowablesSupport.indexMessage("Index out of range: ", detail)):
  def this() = this(null: Any)
  def this(message: String) = this(message: Any)
  def this(index: scala.Int) = this(index: Any)
  def this(index: scala.Long) = this(index: Any)

class InstantiationException(primary: Any = null) extends ReflectiveOperationException(primary)

class InterruptedException(primary: Any = null) extends Exception(primary):
  def this(message: String) = this(message: Any)

class NegativeArraySizeException(primary: Any = null) extends RuntimeException(primary):
  def this(message: String) = this(message: Any)

class NoSuchFieldException(primary: Any = null) extends ReflectiveOperationException(primary)

class NoSuchMethodException(primary: Any = null) extends ReflectiveOperationException(primary)

class NullPointerException() extends RuntimeException():
  def this(message: String) = this()

class NumberFormatException(primary: Any = null) extends IllegalArgumentException(primary):
  def this(message: String) = this(message: Any)

class ReflectiveOperationException(primary: Any = null, cause: Throwable | Null = null)
    extends Exception(primary, cause)

class RuntimeException (
    primary: Any = null,
    cause: Throwable | Null = null,
    enableSuppression: scala.Boolean = true,
    writableStackTrace: scala.Boolean = true
) extends Exception(primary, cause, enableSuppression, writableStackTrace)
:
  def this() =
    this(null, null, true, true)
  def this(message: String) = this(message: Any, null, true, true)
  def this(message: String, cause: Throwable) = this(message: Any, cause, true, true)
  def this(cause: Throwable) = this(null, cause, true, true)
  def this(message: String, cause: Throwable, enableSuppression: scala.Boolean, writableStackTrace: scala.Boolean) =
    this(message: Any, cause, enableSuppression, writableStackTrace)

class SecurityException(primary: Any = null, cause: Throwable | Null = null)
    extends RuntimeException(primary, cause)
:
  def this() =
    this(null, null)
  def this(message: String) = this(message: Any, null)
  def this(message: String, cause: Throwable) = this(message: Any, cause)
  def this(cause: Throwable) = this(null, cause)

class StringIndexOutOfBoundsException(detail: Any = null)
    extends IndexOutOfBoundsException(ThrowablesSupport.indexMessage("String index out of range: ", detail))

class TypeNotPresentException(t: String, cause: Throwable)
    extends RuntimeException("Type " + t + " not present", cause):
  def typeName(): String = t

class UnsupportedOperationException(primary: Any = null, cause: Throwable | Null = null)
    extends RuntimeException(primary, cause)
:
  def this() =
    this(null, null)
  def this(message: String) = this(message: Any, null)
  def this(message: String, cause: Throwable) = this(message: Any, cause)
  def this(cause: Throwable) = this(null, cause)
