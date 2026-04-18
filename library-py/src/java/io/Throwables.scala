package java.io

import java.lang.ThrowablesSupport

class IOException(primary: Any = null, cause: Throwable | Null = null)
    extends Exception(
      ThrowablesSupport.throwableMessage(primary, cause),
      ThrowablesSupport.throwableCause(primary, cause)
    )

class EOFException(message: String | Null = null) extends IOException(message)

class UTFDataFormatException(message: String | Null = null) extends IOException(message)

class UnsupportedEncodingException(message: String | Null = null) extends IOException(message)

abstract class ObjectStreamException protected (message: String | Null = null) extends IOException(message)

class NotSerializableException(message: String | Null = null) extends ObjectStreamException(message)
