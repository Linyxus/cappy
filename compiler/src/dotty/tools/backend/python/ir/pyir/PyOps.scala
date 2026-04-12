package dotty.tools.backend.python.ir.pyir

// ===================================================================
//  Unary operation codes
// ===================================================================

/** Codes for `PyUnaryOp` tree nodes.
 *
 *  Modeled on sjsir's `UnaryOp.Code`. Each code has a fixed result
 *  type computed by `PyUnaryCode.resultTypeOf`. The operand type is
 *  implicit in the code - e.g. `IntNeg` takes a `PyIntType` operand
 *  and produces a `PyIntType` result.
 *
 *  Note: defined as a top-level enum (not inside a companion object)
 *  because the `PyUnaryOp` case class lives in `PyIR.scala` - it
 *  cannot share a companion across files.
 */
enum PyUnaryCode:
  // Boolean
  case BoolNot

  // Numeric negation
  case IntNeg, LongNeg, FloatNeg, DoubleNeg

  // Bitwise complement
  case IntNot, LongNot

  // Numeric coercions (erased at the Scala-to-Python boundary)
  case CharToInt
  case ByteToInt
  case ShortToInt
  case IntToLong
  case IntToFloat
  case IntToDouble
  case IntToChar
  case IntToByte
  case IntToShort
  case LongToInt
  case LongToFloat
  case LongToDouble
  case FloatToInt
  case FloatToLong
  case FloatToDouble
  case DoubleToInt
  case DoubleToLong
  case DoubleToFloat

  // Primitives & runtime
  case StringLength
  case ArrayLength
  case CheckNotNull
  case GetClass
  case IdentityHashCode
  case Clone

  // Exception wrapping
  case WrapAsThrowable
  case UnwrapFromThrowable
  case Throw

  // Float bit reinterpretation
  case FloatToBits, FloatFromBits
  case DoubleToBits, DoubleFromBits

  // Class reflection
  case ClassGetName
  case ClassIsPrimitive
  case ClassIsInterface
  case ClassIsArray

object PyUnaryCode:
  /** Result type of the given unary op code. */
  def resultTypeOf(op: PyUnaryCode): PyType = op match
    case BoolNot => PyBooleanType

    case IntNeg | IntNot
       | CharToInt | ByteToInt | ShortToInt
       | IntToChar | IntToByte | IntToShort
       | LongToInt | FloatToInt | DoubleToInt
       | StringLength | ArrayLength | IdentityHashCode
       | FloatToBits => PyIntType

    case LongNeg | LongNot
       | IntToLong | FloatToLong | DoubleToLong
       | DoubleToBits => PyLongType

    case FloatNeg | IntToFloat | LongToFloat
       | DoubleToFloat | FloatFromBits => PyFloatType

    case DoubleNeg | IntToDouble | LongToDouble
       | FloatToDouble | DoubleFromBits => PyDoubleType

    case CheckNotNull | Clone
       | WrapAsThrowable | UnwrapFromThrowable => PyAnyType

    case Throw => PyNothingType

    case GetClass => PyClassType(PyClassName.ClassClass)
    case ClassGetName => PyStringType
    case ClassIsPrimitive | ClassIsInterface | ClassIsArray => PyBooleanType

// ===================================================================
//  Binary operation codes
// ===================================================================

/** Codes for `PyBinaryOp` tree nodes. */
enum PyBinaryCode:
  // Boolean
  case BoolEq, BoolNe, BoolOr, BoolAnd

  // Int arithmetic
  case IntAdd, IntSub, IntMul, IntDiv, IntMod
  case IntOr, IntAnd, IntXor
  case IntShl, IntShr, IntUShr
  case IntEq, IntNe, IntLt, IntLe, IntGt, IntGe
  case IntUDiv, IntURem
  case IntULt, IntULe, IntUGt, IntUGe

  // Long arithmetic
  case LongAdd, LongSub, LongMul, LongDiv, LongMod
  case LongOr, LongAnd, LongXor
  case LongShl, LongShr, LongUShr
  case LongEq, LongNe, LongLt, LongLe, LongGt, LongGe
  case LongUDiv, LongURem
  case LongULt, LongULe, LongUGt, LongUGe

  // Float arithmetic (IEEE-754 single-precision)
  case FloatAdd, FloatSub, FloatMul, FloatDiv, FloatMod
  case FloatEq, FloatNe, FloatLt, FloatLe, FloatGt, FloatGe

  // Double arithmetic
  case DoubleAdd, DoubleSub, DoubleMul, DoubleDiv, DoubleMod
  case DoubleEq, DoubleNe, DoubleLt, DoubleLe, DoubleGt, DoubleGe

  // String
  case StringConcat, StringCharAt, StringEq

  // Reference identity
  case RefEq, RefNe

  // Class reflection
  case ClassIsInstance, ClassIsAssignableFrom, ClassCast, ClassNewArray

object PyBinaryCode:
  /** Result type of the given binary op code. */
  def resultTypeOf(op: PyBinaryCode): PyType = op match
    // Boolean results
    case BoolEq | BoolNe | BoolOr | BoolAnd
       | IntEq | IntNe | IntLt | IntLe | IntGt | IntGe
       | IntULt | IntULe | IntUGt | IntUGe
       | LongEq | LongNe | LongLt | LongLe | LongGt | LongGe
       | LongULt | LongULe | LongUGt | LongUGe
       | FloatEq | FloatNe | FloatLt | FloatLe | FloatGt | FloatGe
       | DoubleEq | DoubleNe | DoubleLt | DoubleLe | DoubleGt | DoubleGe
       | StringEq
       | RefEq | RefNe
       | ClassIsInstance | ClassIsAssignableFrom => PyBooleanType

    // Int results
    case IntAdd | IntSub | IntMul | IntDiv | IntMod
       | IntOr | IntAnd | IntXor
       | IntShl | IntShr | IntUShr
       | IntUDiv | IntURem => PyIntType

    // Long results
    case LongAdd | LongSub | LongMul | LongDiv | LongMod
       | LongOr | LongAnd | LongXor
       | LongShl | LongShr | LongUShr
       | LongUDiv | LongURem => PyLongType

    // Float / Double results
    case FloatAdd | FloatSub | FloatMul | FloatDiv | FloatMod => PyFloatType
    case DoubleAdd | DoubleSub | DoubleMul | DoubleDiv | DoubleMod => PyDoubleType

    // String
    case StringConcat => PyStringType
    case StringCharAt => PyCharType

    // Class
    case ClassCast => PyAnyType
    case ClassNewArray => PyArrayType

// ===================================================================
//  Bit-packed flag classes
// ===================================================================

/** Call-site flags for `PyApply*` nodes. */
final class PyApplyFlags(val bits: Int) extends AnyVal:
  def isPrivate: Boolean     = (bits & PyApplyFlags.PrivateBit) != 0
  def isConstructor: Boolean = (bits & PyApplyFlags.ConstructorBit) != 0

  def withPrivate(v: Boolean): PyApplyFlags =
    new PyApplyFlags(
      if v then bits | PyApplyFlags.PrivateBit
      else bits & ~PyApplyFlags.PrivateBit
    )

  def withConstructor(v: Boolean): PyApplyFlags =
    new PyApplyFlags(
      if v then bits | PyApplyFlags.ConstructorBit
      else bits & ~PyApplyFlags.ConstructorBit
    )

object PyApplyFlags:
  private[ir] inline val PrivateBit     = 1 << 0
  private[ir] inline val ConstructorBit = 1 << 1

  val empty: PyApplyFlags = new PyApplyFlags(0)

/** Namespace classifier for class members.
 *
 *  Encodes visibility + dispatch context in a single value. Maps to
 *  Python decorators / parameter-insertion decisions at the emitter.
 */
enum PyMemberNamespace:
  case Public            // instance method - `def name(self, ...)`
  case PublicStatic      // module-level / `@staticmethod`
  case Private           // private instance
  case PrivateStatic     // private static
  case Constructor       // `__init__`
  case StaticConstructor // module-level static initializer

  def isStatic: Boolean = this match
    case PublicStatic | PrivateStatic | StaticConstructor => true
    case _ => false

  def isInstance: Boolean = !isStatic

/** Member-definition flags: namespace + mutability, bit-packed. */
final class PyMemberFlags(val bits: Int) extends AnyVal:
  def namespace: PyMemberNamespace =
    PyMemberNamespace.fromOrdinal(bits & PyMemberFlags.NamespaceMask)

  def isMutable: Boolean = (bits & PyMemberFlags.MutableBit) != 0

  def withNamespace(ns: PyMemberNamespace): PyMemberFlags =
    new PyMemberFlags((bits & ~PyMemberFlags.NamespaceMask) | ns.ordinal)

  def withMutable(v: Boolean): PyMemberFlags =
    new PyMemberFlags(
      if v then bits | PyMemberFlags.MutableBit
      else bits & ~PyMemberFlags.MutableBit
    )

object PyMemberFlags:
  private[ir] inline val NamespaceMask = 0x7
  private[ir] inline val MutableBit    = 1 << 3

  val empty: PyMemberFlags = new PyMemberFlags(0)
