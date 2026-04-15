package dotty.tools.backend.python.ir.pyir.serialization

/** Constants and exception types for the PyIR binary format. */
object PyIRFormat:

  /** Magic bytes 'P','Y','I','R' as a big-endian u32. */
  final val Magic: Int = 0x50594952

  /** Major version. Bumping is a breaking format change. */
  final val MajorVersion: Int = 1

  /** Minor version. Reader accepts older minors and rejects newer. */
  final val MinorVersion: Int = 0

  /** Packed `(major << 8) | minor`, written as a big-endian u16. */
  final val FormatVersion: Int = (MajorVersion << 8) | MinorVersion

  /** Sibling extension of the corresponding `.py` file. */
  final val FileExtension: String = ".pyir"

class PyIRException(message: String, cause: Throwable | Null)
    extends RuntimeException(message, cause):
  def this(message: String) = this(message, null)

class IncompatibleIRVersionException(message: String) extends PyIRException(message)

class CorruptIRException(message: String, cause: Throwable | Null)
    extends PyIRException(message, cause):
  def this(message: String) = this(message, null)
