package java.util

import java.lang.ThrowablesSupport
import scala.python.{PyAny, extern, name, native}

private[util] object ThrowablesUtil:
  @extern("builtins")
  private object Builtins extends PyAny:
    @name("hex")
    def hex(value: Int): String = native

  def requireNonNullValue[T](value: T): T =
    if value == null then throw new NullPointerException()
    value

  def pyHex(value: Int): String =
    Builtins.hex(value)

class ServiceConfigurationError(message: String, cause: Throwable | Null = null) extends Error(message, cause)

class ConcurrentModificationException(message: String | Null = null) extends RuntimeException(message)

class DuplicateFormatFlagsException(f: String) extends IllegalFormatException():
  ThrowablesUtil.requireNonNullValue(f)

  def getFlags(): String = f
  override def getMessage(): String = "Flags = '" + f + "'"

class EmptyStackException extends RuntimeException()

class FormatFlagsConversionMismatchException(f: String, c: Char) extends IllegalFormatException():
  ThrowablesUtil.requireNonNullValue(f)

  def getFlags(): String = f
  def getConversion(): Char = c
  override def getMessage(): String = "Conversion = " + c + ", Flags = " + f

class FormatterClosedException extends IllegalStateException()

class IllegalFormatCodePointException(c: Int) extends IllegalFormatException():
  def getCodePoint(): Int = c
  override def getMessage(): String = "Code point = " + ThrowablesUtil.pyHex(c)

class IllegalFormatConversionException(c: Char, arg: Class[?]) extends IllegalFormatException():
  ThrowablesUtil.requireNonNullValue(arg)

  def getConversion(): Char = c
  def getArgumentClass(): Class[?] = arg

  override def getMessage(): String = c.toString() + " != " + arg.getName()

class IllegalFormatException private[util] () extends IllegalArgumentException()

class IllegalFormatFlagsException(f: String) extends IllegalFormatException():
  ThrowablesUtil.requireNonNullValue(f)

  def getFlags(): String = f
  override def getMessage(): String = "Flags = '" + f + "'"

class IllegalFormatPrecisionException(p: Int) extends IllegalFormatException():
  def getPrecision(): Int = p
  override def getMessage(): String = "" + p

class IllegalFormatWidthException(w: Int) extends IllegalFormatException():
  def getWidth(): Int = w
  override def getMessage(): String = "" + w

private[util] class IllegalFormatArgumentIndexException(msg: String)
    extends IllegalFormatException():
  override def getMessage(): String = msg

class IllformedLocaleException(s: String | Null = null, errorIndex: Int = -1)
    extends RuntimeException(
      ThrowablesSupport.stringValueOf(s) +
        (if errorIndex < 0 then "" else " [at index " + errorIndex + "]")
    ):
  def getErrorIndex(): Int = errorIndex

class InputMismatchException(s: String | Null = null) extends NoSuchElementException(s)

class InvalidPropertiesFormatException(primary: Any = null)
    extends java.io.IOException(
      primary match
        case cause: Throwable =>
          if cause == null then null.asInstanceOf[String]
          else ThrowablesSupport.stringValueOf(cause)
        case other =>
          other.asInstanceOf[String]
    ):
  primary match
    case cause: Throwable => initCause(cause)
    case _                => ()

class MissingFormatArgumentException(s: String) extends IllegalFormatException():
  ThrowablesUtil.requireNonNullValue(s)

  def getFormatSpecifier(): String = s
  override def getMessage(): String = "Format specifier '" + s + "'"

class MissingFormatWidthException(s: String) extends IllegalFormatException():
  ThrowablesUtil.requireNonNullValue(s)

  def getFormatSpecifier(): String = s
  override def getMessage(): String = s

class MissingResourceException private[util] (
    s: String,
    private var className: String,
    private var key: String,
    cause: Throwable | Null = null
) extends RuntimeException(s, cause):
  def getClassName(): String = className
  def getKey(): String = key

class NoSuchElementException(s: String | Null = null) extends RuntimeException(s: Any):
  def this() = this(null)
  def this(message: String, cause: Throwable) = this(null)  // discard; keeps JDK arity
  def this(cause: Throwable) = this(null)

class TooManyListenersException(s: String | Null = null) extends Exception(s)

class UnknownFormatConversionException(s: String) extends IllegalFormatException():
  ThrowablesUtil.requireNonNullValue(s)

  def getConversion(): String = s
  override def getMessage(): String = "Conversion = '" + s + "'"

class UnknownFormatFlagsException(f: String) extends IllegalFormatException():
  ThrowablesUtil.requireNonNullValue(f)

  def getFlags(): String = f
  override def getMessage(): String = "Flags = " + f
