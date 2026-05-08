package dotty.tools.backend.python.ir.pyir.serialization

import dotty.tools.backend.python.ir.pyir.*

/** Stable byte tags for every PyIR enum / tree kind that crosses the
 *  serialization boundary.
 *
 *  All tag values are explicit. **Never** reuse `enum.ordinal` here:
 *  reordering a Scala enum case must not change `.pyir` files. Adding a
 *  new variant: append a new tag and bump `PyIRFormat.MinorVersion`.
 */
object PyIRTags:

  // ===============================================================
  //  PyTree tag table - 0x00..0x7F reserved for tree kinds.
  // ===============================================================

  // Statements (0x01..0x0F)
  final val TagPyVarDef:      Byte = 0x01
  final val TagPyAssign:      Byte = 0x02
  final val TagPyReturn:      Byte = 0x03
  final val TagPyWhile:       Byte = 0x04
  // 0x05 — reserved (formerly PyForEach; never produced by GenPython)
  final val TagPySkip:        Byte = 0x06

  // Control flow / blocks (0x10..0x1F)
  final val TagPyIf:          Byte = 0x10
  final val TagPyTryCatch:    Byte = 0x11
  final val TagPyTryFinally:  Byte = 0x12
  final val TagPyMatch:       Byte = 0x13
  final val TagPyBlock:       Byte = 0x14
  final val TagPyLabeled:     Byte = 0x15
  final val TagPyLabelReturn: Byte = 0x16
  // 0x17, 0x18 — reserved (formerly PyLocalFnDef / PyLocalFnCall)

  // References (0x20..0x2F)
  final val TagPyVarRef:       Byte = 0x20
  final val TagPyThis:         Byte = 0x21
  final val TagPySelect:       Byte = 0x22
  final val TagPySelectStatic: Byte = 0x23

  // Calls (0x30..0x3F)
  final val TagPyApply:           Byte = 0x30
  final val TagPyApplyStatically: Byte = 0x31
  final val TagPyApplyStatic:     Byte = 0x32
  final val TagPyApplyExternal:   Byte = 0x33
  final val TagPyExternalRef:     Byte = 0x34
  final val TagPyAttrAccess:      Byte = 0x35
  final val TagPyApplyDynamic:    Byte = 0x36

  // Construction (0x40..0x4F)
  final val TagPyNew:        Byte = 0x40
  final val TagPyLoadModule: Byte = 0x41

  // Type tests / casts (0x48..0x4F)
  final val TagPyIsInstanceOf: Byte = 0x48
  final val TagPyAsInstanceOf: Byte = 0x49

  // Arrays / tuples / dicts (0x50..0x57)
  final val TagPyNewArray:    Byte = 0x50
  final val TagPyArrayValue:  Byte = 0x51
  final val TagPyArraySelect: Byte = 0x52
  final val TagPyTupleValue:  Byte = 0x53
  final val TagPyDictValue:   Byte = 0x54

  // Operators (0x58..0x5F)
  final val TagPyUnaryOp:  Byte = 0x58
  final val TagPyBinaryOp: Byte = 0x59

  // Closures / misc (0x60..0x6F)
  final val TagPyClosure: Byte = 0x60
  final val TagPyClassOf: Byte = 0x61

  // Literals (0x70..0x7F)
  final val TagPyBooleanLit: Byte = 0x70
  final val TagPyCharLit:    Byte = 0x71
  final val TagPyByteLit:    Byte = 0x72
  final val TagPyShortLit:   Byte = 0x73
  final val TagPyIntLit:     Byte = 0x74
  final val TagPyLongLit:    Byte = 0x75
  final val TagPyFloatLit:   Byte = 0x76
  final val TagPyDoubleLit:  Byte = 0x77
  final val TagPyStringLit:  Byte = 0x78
  final val TagPyNullLit:    Byte = 0x79
  final val TagPyUnitLit:    Byte = 0x7A

  // ===============================================================
  //  PyType tag table - 0x80..0x8F.
  // ===============================================================

  final val TagPyAnyType:       Byte = 0x80.toByte
  final val TagPyVoidType:      Byte = 0x81.toByte
  final val TagPyNothingType:   Byte = 0x82.toByte
  final val TagPyNullType:      Byte = 0x83.toByte
  // 0x84 — reserved (formerly TagPyUndefinedType; never produced)
  final val TagPyBooleanType:   Byte = 0x85.toByte
  final val TagPyCharType:      Byte = 0x86.toByte
  final val TagPyByteType:      Byte = 0x87.toByte
  final val TagPyShortType:     Byte = 0x88.toByte
  final val TagPyIntType:       Byte = 0x89.toByte
  final val TagPyLongType:      Byte = 0x8A.toByte
  final val TagPyFloatType:     Byte = 0x8B.toByte
  final val TagPyDoubleType:    Byte = 0x8C.toByte
  final val TagPyStringType:    Byte = 0x8D.toByte
  final val TagPyArrayType:     Byte = 0x8E.toByte
  final val TagPyClassType:     Byte = 0x8F.toByte

  def pyTypeTag(tpe: PyType): Byte = tpe match
    case PyAnyType         => TagPyAnyType
    case PyVoidType        => TagPyVoidType
    case PyNothingType     => TagPyNothingType
    case PyNullType        => TagPyNullType
    case PyBooleanType     => TagPyBooleanType
    case PyCharType        => TagPyCharType
    case PyByteType        => TagPyByteType
    case PyShortType       => TagPyShortType
    case PyIntType         => TagPyIntType
    case PyLongType        => TagPyLongType
    case PyFloatType       => TagPyFloatType
    case PyDoubleType      => TagPyDoubleType
    case PyStringType      => TagPyStringType
    case PyArrayType       => TagPyArrayType
    case PyClassType(_)    => TagPyClassType

  def pyTypeFromTag(tag: Byte): PyType = tag match
    case TagPyAnyType       => PyAnyType
    case TagPyVoidType      => PyVoidType
    case TagPyNothingType   => PyNothingType
    case TagPyNullType      => PyNullType
    case TagPyBooleanType   => PyBooleanType
    case TagPyCharType      => PyCharType
    case TagPyByteType      => PyByteType
    case TagPyShortType     => PyShortType
    case TagPyIntType       => PyIntType
    case TagPyLongType      => PyLongType
    case TagPyFloatType     => PyFloatType
    case TagPyDoubleType    => PyDoubleType
    case TagPyStringType    => PyStringType
    case TagPyArrayType     => PyArrayType
    case _ => throw new CorruptIRException(
      s"Unknown PyType tag: 0x${(tag & 0xff).toHexString}")

  // ===============================================================
  //  PyTypeRef tag table.
  // ===============================================================

  final val TagPyPrimRef:  Byte = 0x01
  final val TagPyClassRef: Byte = 0x02
  final val TagPyArrayRef: Byte = 0x03

  // PyPrimRef sub-tag table: matches PyPrimRef.Tag textual encoding,
  // but explicit so reordering is harmless.
  final val TagPrimVoid:    Byte = 0x01
  final val TagPrimBoolean: Byte = 0x02
  final val TagPrimChar:    Byte = 0x03
  final val TagPrimByte:    Byte = 0x04
  final val TagPrimShort:   Byte = 0x05
  final val TagPrimInt:     Byte = 0x06
  final val TagPrimLong:    Byte = 0x07
  final val TagPrimFloat:   Byte = 0x08
  final val TagPrimDouble:  Byte = 0x09
  final val TagPrimNull:    Byte = 0x0A
  final val TagPrimNothing: Byte = 0x0B

  def primTag(tag: PyPrimRef.Tag): Byte = tag match
    case PyPrimRef.Tag.VoidRef    => TagPrimVoid
    case PyPrimRef.Tag.BooleanRef => TagPrimBoolean
    case PyPrimRef.Tag.CharRef    => TagPrimChar
    case PyPrimRef.Tag.ByteRef    => TagPrimByte
    case PyPrimRef.Tag.ShortRef   => TagPrimShort
    case PyPrimRef.Tag.IntRef     => TagPrimInt
    case PyPrimRef.Tag.LongRef    => TagPrimLong
    case PyPrimRef.Tag.FloatRef   => TagPrimFloat
    case PyPrimRef.Tag.DoubleRef  => TagPrimDouble
    case PyPrimRef.Tag.NullRef    => TagPrimNull
    case PyPrimRef.Tag.NothingRef => TagPrimNothing

  def primFromTag(tag: Byte): PyPrimRef.Tag = tag match
    case TagPrimVoid    => PyPrimRef.Tag.VoidRef
    case TagPrimBoolean => PyPrimRef.Tag.BooleanRef
    case TagPrimChar    => PyPrimRef.Tag.CharRef
    case TagPrimByte    => PyPrimRef.Tag.ByteRef
    case TagPrimShort   => PyPrimRef.Tag.ShortRef
    case TagPrimInt     => PyPrimRef.Tag.IntRef
    case TagPrimLong    => PyPrimRef.Tag.LongRef
    case TagPrimFloat   => PyPrimRef.Tag.FloatRef
    case TagPrimDouble  => PyPrimRef.Tag.DoubleRef
    case TagPrimNull    => PyPrimRef.Tag.NullRef
    case TagPrimNothing => PyPrimRef.Tag.NothingRef
    case _ => throw new CorruptIRException(
      s"Unknown PyPrimRef.Tag tag: 0x${(tag & 0xff).toHexString}")

  // ===============================================================
  //  PyClassKind tag table.
  // ===============================================================

  final val TagKindClass:         Byte = 0x01
  final val TagKindModuleClass:   Byte = 0x02
  final val TagKindInterface:     Byte = 0x03
  // 0x04 — reserved (formerly TagKindAbstractClass; abstractness is no
  // longer modeled at the PyIR level — abstract classes serialize as
  // PyClassKind.Class).

  def classKindTag(k: PyClassKind): Byte = k match
    case PyClassKind.Class         => TagKindClass
    case PyClassKind.ModuleClass   => TagKindModuleClass
    case PyClassKind.Interface     => TagKindInterface

  def classKindFromTag(tag: Byte): PyClassKind = tag match
    case TagKindClass         => PyClassKind.Class
    case TagKindModuleClass   => PyClassKind.ModuleClass
    case TagKindInterface     => PyClassKind.Interface
    case _ => throw new CorruptIRException(
      s"Unknown PyClassKind tag: 0x${(tag & 0xff).toHexString}")

  // ===============================================================
  //  PyMemberNamespace tag table.
  // ===============================================================

  final val TagNsPublic:            Byte = 0x01
  final val TagNsPublicStatic:      Byte = 0x02
  final val TagNsPrivate:           Byte = 0x03
  final val TagNsPrivateStatic:     Byte = 0x04
  final val TagNsConstructor:       Byte = 0x05
  final val TagNsStaticConstructor: Byte = 0x06

  def namespaceTag(ns: PyMemberNamespace): Byte = ns match
    case PyMemberNamespace.Public            => TagNsPublic
    case PyMemberNamespace.PublicStatic      => TagNsPublicStatic
    case PyMemberNamespace.Private           => TagNsPrivate
    case PyMemberNamespace.PrivateStatic     => TagNsPrivateStatic
    case PyMemberNamespace.Constructor       => TagNsConstructor
    case PyMemberNamespace.StaticConstructor => TagNsStaticConstructor

  def namespaceFromTag(tag: Byte): PyMemberNamespace = tag match
    case TagNsPublic            => PyMemberNamespace.Public
    case TagNsPublicStatic      => PyMemberNamespace.PublicStatic
    case TagNsPrivate           => PyMemberNamespace.Private
    case TagNsPrivateStatic     => PyMemberNamespace.PrivateStatic
    case TagNsConstructor       => PyMemberNamespace.Constructor
    case TagNsStaticConstructor => PyMemberNamespace.StaticConstructor
    case _ => throw new CorruptIRException(
      s"Unknown PyMemberNamespace tag: 0x${(tag & 0xff).toHexString}")

  // ===============================================================
  //  PyUnaryCode tag table.
  //
  //  Tag values are stable: append-only. Adding a new code requires a
  //  minor format bump.
  // ===============================================================

  import PyUnaryCode.*

  // Tag values 0x01..0x22 are stable, append-only. Values 0x1A, 0x1C..0x21,
  // 0x23..0x2A are reserved (formerly StringLength, CheckNotNull, GetClass,
  // IdentityHashCode, Clone, WrapAsThrowable, UnwrapFromThrowable,
  // FloatToBits, FloatFromBits, DoubleToBits, DoubleFromBits, ClassGetName,
  // ClassIsPrimitive, ClassIsInterface, ClassIsArray — none produced by
  // GenPython).
  private val unaryToTagArr: Array[Byte] = {
    val arr = new Array[Byte](PyUnaryCode.values.length)
    arr(BoolNot.ordinal)             = 0x01
    arr(IntNeg.ordinal)              = 0x02
    arr(LongNeg.ordinal)             = 0x03
    arr(FloatNeg.ordinal)            = 0x04
    arr(DoubleNeg.ordinal)           = 0x05
    arr(IntNot.ordinal)              = 0x06
    arr(LongNot.ordinal)             = 0x07
    arr(CharToInt.ordinal)           = 0x08
    arr(ByteToInt.ordinal)           = 0x09
    arr(ShortToInt.ordinal)          = 0x0A
    arr(IntToLong.ordinal)           = 0x0B
    arr(IntToFloat.ordinal)          = 0x0C
    arr(IntToDouble.ordinal)         = 0x0D
    arr(IntToChar.ordinal)           = 0x0E
    arr(IntToByte.ordinal)           = 0x0F
    arr(IntToShort.ordinal)          = 0x10
    arr(LongToInt.ordinal)           = 0x11
    arr(LongToFloat.ordinal)         = 0x12
    arr(LongToDouble.ordinal)        = 0x13
    arr(FloatToInt.ordinal)          = 0x14
    arr(FloatToLong.ordinal)         = 0x15
    arr(FloatToDouble.ordinal)       = 0x16
    arr(DoubleToInt.ordinal)         = 0x17
    arr(DoubleToLong.ordinal)        = 0x18
    arr(DoubleToFloat.ordinal)       = 0x19
    arr(ArrayLength.ordinal)         = 0x1B
    arr(Throw.ordinal)               = 0x22
    arr
  }

  private val unaryFromTagMap: Map[Byte, PyUnaryCode] =
    PyUnaryCode.values.iterator.map(c => unaryToTagArr(c.ordinal) -> c).toMap

  def unaryTag(op: PyUnaryCode): Byte = unaryToTagArr(op.ordinal)

  def unaryFromTag(tag: Byte): PyUnaryCode =
    unaryFromTagMap.getOrElse(
      tag,
      throw new CorruptIRException(
        s"Unknown PyUnaryCode tag: 0x${(tag & 0xff).toHexString}"))

  // ===============================================================
  //  PyBinaryCode tag table.
  // ===============================================================

  import PyBinaryCode.*

  // Tag values are stable, append-only. Values 0x21..0x26 (Int unsigned),
  // 0x41..0x46 (Long unsigned), 0x71 (StringCharAt), 0x75..0x78 (class
  // reflection) are reserved (formerly IntUDiv/IntURem/IntU*, LongUDiv/
  // LongURem/LongU*, StringCharAt, ClassIsInstance/ClassIsAssignableFrom/
  // ClassCast/ClassNewArray — none produced by GenPython).
  private val binaryToTagArr: Array[Byte] = {
    val arr = new Array[Byte](PyBinaryCode.values.length)
    arr(BoolEq.ordinal)  = 0x01
    arr(BoolNe.ordinal)  = 0x02
    arr(BoolOr.ordinal)  = 0x03
    arr(BoolAnd.ordinal) = 0x04

    arr(IntAdd.ordinal)  = 0x10
    arr(IntSub.ordinal)  = 0x11
    arr(IntMul.ordinal)  = 0x12
    arr(IntDiv.ordinal)  = 0x13
    arr(IntMod.ordinal)  = 0x14
    arr(IntOr.ordinal)   = 0x15
    arr(IntAnd.ordinal)  = 0x16
    arr(IntXor.ordinal)  = 0x17
    arr(IntShl.ordinal)  = 0x18
    arr(IntShr.ordinal)  = 0x19
    arr(IntUShr.ordinal) = 0x1A
    arr(IntEq.ordinal)   = 0x1B
    arr(IntNe.ordinal)   = 0x1C
    arr(IntLt.ordinal)   = 0x1D
    arr(IntLe.ordinal)   = 0x1E
    arr(IntGt.ordinal)   = 0x1F
    arr(IntGe.ordinal)   = 0x20

    arr(LongAdd.ordinal)  = 0x30
    arr(LongSub.ordinal)  = 0x31
    arr(LongMul.ordinal)  = 0x32
    arr(LongDiv.ordinal)  = 0x33
    arr(LongMod.ordinal)  = 0x34
    arr(LongOr.ordinal)   = 0x35
    arr(LongAnd.ordinal)  = 0x36
    arr(LongXor.ordinal)  = 0x37
    arr(LongShl.ordinal)  = 0x38
    arr(LongShr.ordinal)  = 0x39
    arr(LongUShr.ordinal) = 0x3A
    arr(LongEq.ordinal)   = 0x3B
    arr(LongNe.ordinal)   = 0x3C
    arr(LongLt.ordinal)   = 0x3D
    arr(LongLe.ordinal)   = 0x3E
    arr(LongGt.ordinal)   = 0x3F
    arr(LongGe.ordinal)   = 0x40

    arr(FloatAdd.ordinal) = 0x50
    arr(FloatSub.ordinal) = 0x51
    arr(FloatMul.ordinal) = 0x52
    arr(FloatDiv.ordinal) = 0x53
    arr(FloatMod.ordinal) = 0x54
    arr(FloatEq.ordinal)  = 0x55
    arr(FloatNe.ordinal)  = 0x56
    arr(FloatLt.ordinal)  = 0x57
    arr(FloatLe.ordinal)  = 0x58
    arr(FloatGt.ordinal)  = 0x59
    arr(FloatGe.ordinal)  = 0x5A

    arr(DoubleAdd.ordinal) = 0x60
    arr(DoubleSub.ordinal) = 0x61
    arr(DoubleMul.ordinal) = 0x62
    arr(DoubleDiv.ordinal) = 0x63
    arr(DoubleMod.ordinal) = 0x64
    arr(DoubleEq.ordinal)  = 0x65
    arr(DoubleNe.ordinal)  = 0x66
    arr(DoubleLt.ordinal)  = 0x67
    arr(DoubleLe.ordinal)  = 0x68
    arr(DoubleGt.ordinal)  = 0x69
    arr(DoubleGe.ordinal)  = 0x6A

    arr(StringConcat.ordinal) = 0x70
    arr(StringEq.ordinal)     = 0x72

    arr(RefEq.ordinal) = 0x73
    arr(RefNe.ordinal) = 0x74

    arr
  }

  private val binaryFromTagMap: Map[Byte, PyBinaryCode] =
    PyBinaryCode.values.iterator.map(c => binaryToTagArr(c.ordinal) -> c).toMap

  def binaryTag(op: PyBinaryCode): Byte = binaryToTagArr(op.ordinal)

  def binaryFromTag(tag: Byte): PyBinaryCode =
    binaryFromTagMap.getOrElse(
      tag,
      throw new CorruptIRException(
        s"Unknown PyBinaryCode tag: 0x${(tag & 0xff).toHexString}"))
