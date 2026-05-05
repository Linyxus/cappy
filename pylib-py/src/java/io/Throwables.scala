package java.io

import java.lang.ThrowablesSupport

class IOException(primary: Any = null, cause: Throwable | Null = null)
    extends Exception(
      ThrowablesSupport.throwableMessage(primary, cause),
      ThrowablesSupport.throwableCause(primary, cause)
    ):
  // JDK-shaped ctor overloads. Scala's default-args primary doesn't
  // generate these as distinct PyIR signatures; stdlib (typechecked
  // against the JVM `IOException`) emits `<init>()` / `<init>(Ljava_dlang_dString)`
  // / `<init>(Ljava_dlang_dString,Ljava_dlang_dThrowable)` /
  // `<init>(Ljava_dlang_dThrowable)` references. Same pattern as
  // `java.lang.Exception` and friends.
  def this() = this(null, null)
  def this(message: String) = this(message: Any, null)
  def this(message: String, cause: Throwable) = this(message: Any, cause)
  def this(cause: Throwable) = this(null, cause)

class EOFException(message: String | Null = null) extends IOException(message)

class UTFDataFormatException(message: String | Null = null) extends IOException(message)

class UnsupportedEncodingException(message: String | Null = null) extends IOException(message)

abstract class ObjectStreamException protected (message: String | Null = null) extends IOException(message)

class NotSerializableException(message: String | Null = null) extends ObjectStreamException(message)
