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
   *
   *  Bumped to 4 when `PyFieldName` gained an `isPrivate` flag for
   *  owner-aware mangling at the encoding layer. Private fields now
   *  encode as `_scpy_f_<owner>__<simple>` to avoid colliding with
   *  same-named subclass fields; public fields keep their simple name.
   *  Old `.pyir` files lack the trailing flag byte and cannot be read.
   *
   *  Bumped to 5 when `PyApply` and `PyApplyStatically` were merged into
   *  a single `PyApply` node carrying a `PyDispatch` field. `TagPyApply`
   *  (0x30) now writes a dispatch byte after `flags`; `TagPyApplyStatically`
   *  (0x31) is retired. Old `.pyir` files cannot be read.
   */
  final val MajorVersion: Int = 5

  /** Minor version. Reader accepts older minors and rejects newer.
   *
   *  Bumped to 1 when `PyTupleValue` was added (tag `0x53`) so the
   *  Python backend can lower Scala tuples directly to Python tuples.
   *  Older readers will reject `.pyir` containing the new tag.
   *
   *  Bumped to 2 when `PyDictValue` was added (tag `0x54`) so the
   *  Python backend can lower `scala.python.PyMap` factory calls to
   *  Python dict literals.
   *
   *  Bumped to 3 when `PyClassDef.superClass` widened from
   *  `Option[PyClassName]` to `Option[PyClassSuper]`. The `superClass`
   *  encoding gained a third discriminator byte value (2 = extern
   *  module + dotted path) so a Scala class can extend an `@extern`
   *  Python class. Older readers will reject `.pyir` containing the
   *  new discriminator.
   */
  final val MinorVersion: Int = 3

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
