package dotty.tools.backend.python.ir.pyir

// ===================================================================
//  PyTypeRef - serializable signature identifiers
// ===================================================================

/** Serializable type identifier used inside `PyMethodName` signatures
 *  and as operand type tags on IR nodes (`PyIsInstanceOf.testType`,
 *  `PyNewArray.elemTypeRef`, etc.).
 *
 *  Equality is structural. Two `PyTypeRef` values for the same Scala
 *  type - regardless of where they are constructed - are equal.
 */
sealed abstract class PyTypeRef extends Product with Serializable:
  /** Short, stable string encoding of this type reference. Used to
   *  build `PyMethodName.encoded`. */
  def encoded: String

/** Primitive type reference. */
final case class PyPrimRef(tag: PyPrimRef.Tag) extends PyTypeRef:
  def encoded: String = tag.encoded

object PyPrimRef:
  enum Tag(val encoded: String):
    case VoidRef    extends Tag("V")
    case BooleanRef extends Tag("Z")
    case CharRef    extends Tag("C")
    case ByteRef    extends Tag("B")
    case ShortRef   extends Tag("S")
    case IntRef     extends Tag("I")
    case LongRef    extends Tag("J")
    case FloatRef   extends Tag("F")
    case DoubleRef  extends Tag("D")
    case NullRef    extends Tag("N")
    case NothingRef extends Tag("E")

  // Convenience constant instances
  val VoidRef:    PyPrimRef = PyPrimRef(Tag.VoidRef)
  val BooleanRef: PyPrimRef = PyPrimRef(Tag.BooleanRef)
  val CharRef:    PyPrimRef = PyPrimRef(Tag.CharRef)
  val ByteRef:    PyPrimRef = PyPrimRef(Tag.ByteRef)
  val ShortRef:   PyPrimRef = PyPrimRef(Tag.ShortRef)
  val IntRef:     PyPrimRef = PyPrimRef(Tag.IntRef)
  val LongRef:    PyPrimRef = PyPrimRef(Tag.LongRef)
  val FloatRef:   PyPrimRef = PyPrimRef(Tag.FloatRef)
  val DoubleRef:  PyPrimRef = PyPrimRef(Tag.DoubleRef)
  val NullRef:    PyPrimRef = PyPrimRef(Tag.NullRef)
  val NothingRef: PyPrimRef = PyPrimRef(Tag.NothingRef)

/** Reference to a class or interface by fully qualified name.
 *
 *  The FQN is rendered as a Python identifier by escaping the two
 *  characters whose naive treatment used to alias distinct names:
 *    - a literal `_` in any segment becomes `_u`
 *    - a `.` between segments becomes `_d`
 *  After escaping, every `_` in the encoded output is immediately
 *  followed by a fixed marker letter (`u` for a literal underscore,
 *  `d` for a segment break), so the mapping `nameString -> encoded`
 *  is injective. Previously `nameString.replace('.', '_')` collapsed
 *  `a_b.c.D` and `a.b_c.D` onto the same `La_b_c_D`. */
final case class PyClassRef(className: PyClassName) extends PyTypeRef:
  def encoded: String = "L" + PyClassRef.encodeFqn(className.nameString)

object PyClassRef:
  /** Injective encoding of a dot-separated FQN as a Python identifier.
   *
   *  Implementation: scan once and emit `_u` for each `_` and `_d`
   *  for each `.`; all other characters pass through unchanged.
   *  Doing the substitutions in a single pass (rather than chained
   *  `String.replace` calls) is what guarantees the `_` we emit when
   *  encoding `.` is not itself re-escaped. */
  private[pyir] def encodeFqn(fqn: String): String =
    val sb = new java.lang.StringBuilder(fqn.length + 4)
    var i = 0
    while i < fqn.length do
      val ch = fqn.charAt(i)
      if ch == '_' then sb.append("_u")
      else if ch == '.' then sb.append("_d")
      else sb.append(ch)
      i += 1
    sb.toString

/** Reference to an array type.
 *
 *  Dimensions are tracked for signature disambiguation: Scala's
 *  `Array[Array[Int]]` is distinct from `Array[Int]` for overload
 *  purposes, even though both erase to a Python list at runtime.
 */
final case class PyArrayRef(base: PyTypeRef, dims: Int) extends PyTypeRef:
  require(dims >= 1, s"PyArrayRef dims must be >= 1, got $dims")
  def encoded: String = ("A" * dims) + base.encoded

// ===================================================================
//  PyType - runtime-shape types attached to tree nodes
// ===================================================================

/** Runtime-shape type attached to every `PyTree` node.
 *
 *  Much smaller than sjsir's `Type` hierarchy because Python has no
 *  nominal type system at runtime: no nullability tracking, no exact-
 *  type flags, no record/closure types, no dimensionality on arrays.
 */
sealed abstract class PyType extends Product with Serializable

// Top / special
case object PyAnyType       extends PyType  // catch-all
case object PyVoidType      extends PyType  // statement-typed nodes
case object PyNothingType   extends PyType  // diverging (return, throw)
case object PyNullType      extends PyType  // None

// Primitive value types (distinct at IR level to drive numeric wrapping)
case object PyBooleanType extends PyType
case object PyCharType    extends PyType
case object PyByteType    extends PyType
case object PyShortType   extends PyType
case object PyIntType     extends PyType
case object PyLongType    extends PyType
case object PyFloatType   extends PyType
case object PyDoubleType  extends PyType
case object PyStringType  extends PyType

// Reference types
final case class PyClassType(className: PyClassName) extends PyType
case object PyArrayType extends PyType  // dimensionless - Python list

// ===================================================================
//  Type / TypeRef conversions
// ===================================================================

object PyTypes:

  /** Best-effort conversion of a `PyType` to a `PyTypeRef`.
   *
   *  This is lossy for `PyArrayType` (we don't know the element type
   *  at the runtime-type level) and for `PyAnyType` (we collapse to
   *  `ObjectRef`). Prefer constructing a `PyTypeRef` directly at the
   *  codegen level from the Scala source type - this helper is for
   *  fallback paths only.
   */
  def toRef(tpe: PyType): PyTypeRef = tpe match
    case PyAnyType       => PyClassRef(PyClassName.ObjectClass)
    case PyVoidType      => PyPrimRef.VoidRef
    case PyNothingType   => PyPrimRef.NothingRef
    case PyNullType      => PyPrimRef.NullRef
    case PyBooleanType   => PyPrimRef.BooleanRef
    case PyCharType      => PyPrimRef.CharRef
    case PyByteType      => PyPrimRef.ByteRef
    case PyShortType     => PyPrimRef.ShortRef
    case PyIntType       => PyPrimRef.IntRef
    case PyLongType      => PyPrimRef.LongRef
    case PyFloatType     => PyPrimRef.FloatRef
    case PyDoubleType    => PyPrimRef.DoubleRef
    case PyStringType    => PyClassRef(PyClassName.StringClass)
    case PyClassType(cn) => PyClassRef(cn)
    case PyArrayType     => PyArrayRef(PyClassRef(PyClassName.ObjectClass), 1)

  /** Convert a `PyTypeRef` to a `PyType`. Loses array element info. */
  def toType(ref: PyTypeRef): PyType = ref match
    case PyPrimRef(tag) => tag match
      case PyPrimRef.Tag.VoidRef    => PyVoidType
      case PyPrimRef.Tag.BooleanRef => PyBooleanType
      case PyPrimRef.Tag.CharRef    => PyCharType
      case PyPrimRef.Tag.ByteRef    => PyByteType
      case PyPrimRef.Tag.ShortRef   => PyShortType
      case PyPrimRef.Tag.IntRef     => PyIntType
      case PyPrimRef.Tag.LongRef    => PyLongType
      case PyPrimRef.Tag.FloatRef   => PyFloatType
      case PyPrimRef.Tag.DoubleRef  => PyDoubleType
      case PyPrimRef.Tag.NullRef    => PyNullType
      case PyPrimRef.Tag.NothingRef => PyNothingType
    case PyClassRef(cn)   => PyClassType(cn)
    case PyArrayRef(_, _) => PyArrayType
