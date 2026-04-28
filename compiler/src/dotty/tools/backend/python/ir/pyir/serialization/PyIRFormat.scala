package dotty.tools.backend.python.ir.pyir.serialization

/** Constants and exception types for the PyIR binary format. */
object PyIRFormat:

  /** Magic bytes 'P','Y','I','R' as a big-endian u32. */
  final val Magic: Int = 0x50594952

  /** Major version. Bumping is a breaking format change.
   *
   *  Bumped to 2 when `PyClosure` lost its `captureParams` /
   *  `captureValues` fields: the old layout would deserialize as
   *  truncated capture lists, so old `.pyir` files cannot be read.
   *
   *  Bumped to 3 when several never-produced IR variants were dropped
   *  from the format: `PyForEach`, `PyClassKind.AbstractClass`,
   *  `PyUndefinedType`, and a swathe of unary / binary opcodes
   *  (StringLength, CheckNotNull, GetClass, IdentityHashCode, Clone,
   *  WrapAsThrowable, UnwrapFromThrowable, Float/Double bit-cast ops,
   *  class-reflection unary/binary ops, `IntU*` / `LongU*` unsigned ops,
   *  `StringCharAt`). Old `.pyir` files containing these tags cannot be
   *  read by this version. The freed tag values are reserved.
   */
  final val MajorVersion: Int = 3

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
