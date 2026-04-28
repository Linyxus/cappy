package dotty.tools.backend.python.ir.pyir

// ===================================================================
//  PyIR - Python IR tree definitions
// ===================================================================
//
//  This file holds the full unified `PyTree` hierarchy: the root
//  `PyTree` class, marker traits (`PyAssignable`, `PyLiteral`,
//  `PyMatchableLiteral`), all top-level class/method/field definitions,
//  statements, expressions, and literals. Op codes live in PyOps.scala;
//  names in PyNames.scala; types in PyTypes.scala; positions in
//  PyPosition.scala.
//
//  All tree nodes carry a `tpe: PyType` (runtime value type) and a
//  `pos: PyPosition` (source location). Statement-typed nodes have
//  `tpe = PyVoidType`; diverging nodes have `tpe = PyNothingType`.
// ===================================================================

/** Base of all PyIR tree nodes.
 *
 *  `sealed` so exhaustiveness checking works - adding a new node kind
 *  is a breaking change that forces every pattern match to be updated.
 */
sealed abstract class PyTree:
  def tpe: PyType
  def pos: PyPosition

// --- Marker traits -------------------------------------------------

/** Nodes that can appear on the LHS of a `PyAssign`. */
sealed trait PyAssignable extends PyTree

/** Literal nodes. */
sealed trait PyLiteral extends PyTree

/** Literals usable as `PyMatch` pattern alternatives. */
sealed trait PyMatchableLiteral extends PyLiteral

// ===================================================================
//  Top-level class / method / field definitions
// ===================================================================

/** A class or module definition. The single top-level definition
 *  container in PyIR - there is no free-floating "module" / "file"
 *  node. All code in a compilation unit lives inside some
 *  `PyClassDef`. */
final case class PyClassDef(
    name:         PyClassName,
    originalName: PyOriginalName,
    kind:         PyClassKind,
    superClass:   Option[PyClassName],
    interfaces:   List[PyClassName],
    fields:       List[PyFieldDef],
    methods:      List[PyMethodDef],
    pos:          PyPosition
)

enum PyClassKind:
  case Class            // ordinary Scala class (also abstract classes — we
                        // don't track abstractness at the PyIR level)
  case ModuleClass      // Scala `object` - emitter creates singleton instance
  case Interface        // Scala trait (no direct instantiation)

final case class PyFieldDef(
    flags:        PyMemberFlags,
    name:         PyFieldName,
    originalName: PyOriginalName,
    ftpe:         PyType,
    pos:          PyPosition
)

final case class PyMethodDef(
    flags:        PyMemberFlags,
    name:         PyMethodName,
    originalName: PyOriginalName,
    args:         List[PyParamDef],   // does NOT include `self`
    resultType:   PyType,
    body:         Option[PyTree],     // None = abstract
    pos:          PyPosition
)

final case class PyParamDef(
    name:         PyLocalName,
    originalName: PyOriginalName,
    ptpe:         PyType,
    mutable:      Boolean,
    pos:          PyPosition
)

// ===================================================================
//  Statements (tpe = PyVoidType or PyNothingType)
// ===================================================================

final case class PyVarDef(
    name:         PyLocalName,
    originalName: PyOriginalName,
    vtpe:         PyType,
    mutable:      Boolean,
    rhs:          PyTree
)(val pos: PyPosition) extends PyTree:
  val tpe: PyType = PyVoidType

final case class PyAssign(lhs: PyAssignable, rhs: PyTree)
    (val pos: PyPosition) extends PyTree:
  val tpe: PyType = PyVoidType

final case class PyReturn(value: PyTree)(val pos: PyPosition) extends PyTree:
  val tpe: PyType = PyNothingType

final case class PyWhile(cond: PyTree, body: PyTree)
    (val pos: PyPosition) extends PyTree:
  val tpe: PyType = PyVoidType

final case class PySkip()(val pos: PyPosition) extends PyTree:
  val tpe: PyType = PyVoidType

// --- If / match / try / block / labeled ----------------------------
//  These can be statements (tpe = PyVoidType) or value-producing
//  expressions (tpe = non-void), distinguished by the `tpe` they carry.

final case class PyIf(cond: PyTree, thenp: PyTree, elsep: PyTree)
    (val tpe: PyType, val pos: PyPosition) extends PyTree

final case class PyTryCatch(
    block:              PyTree,
    errVar:             PyLocalName,
    errVarOriginalName: PyOriginalName,
    handler:            PyTree
)(val tpe: PyType, val pos: PyPosition) extends PyTree

final case class PyTryFinally(block: PyTree, finalizer: PyTree)
    (val pos: PyPosition) extends PyTree:
  def tpe: PyType = block.tpe

final case class PyMatch(
    selector: PyTree,
    cases:    List[(List[PyMatchableLiteral], PyTree)],
    default:  PyTree
)(val tpe: PyType, val pos: PyPosition) extends PyTree

final case class PyBlock(stats: List[PyTree], expr: PyTree)
    (val pos: PyPosition) extends PyTree:
  def tpe: PyType = expr.tpe

final case class PyLabeled(label: PyLabelName, body: PyTree)
    (val tpe: PyType, val pos: PyPosition) extends PyTree

final case class PyLabelReturn(label: PyLabelName, value: PyTree)
    (val pos: PyPosition) extends PyTree:
  val tpe: PyType = PyNothingType

// ===================================================================
//  References
// ===================================================================

final case class PyVarRef(name: PyLocalName)
    (val tpe: PyType, val pos: PyPosition) extends PyAssignable

final case class PyThis()(val tpe: PyType, val pos: PyPosition) extends PyTree

final case class PySelect(qualifier: PyTree, field: PyFieldName)
    (val tpe: PyType, val pos: PyPosition) extends PyAssignable

final case class PySelectStatic(field: PyFieldName)
    (val tpe: PyType, val pos: PyPosition) extends PyAssignable

// ===================================================================
//  Calls
// ===================================================================

// NOTE: Every `PyApply*` node carries a `flags: PyApplyFlags`. Today
// `GenPython` always passes `PyApplyFlags.empty`; the linker, emitter,
// and reachability passes never inspect the bits. The field is kept
// because the on-disk format already reserves space for it, but should
// either be wired (private/constructor metadata) or removed wholesale.
// See `PyApplyFlags` in `PyOps.scala` for the reserved bit layout.

/** Instance dispatch via virtual lookup.
 *
 *  The `className` field names the *static* receiver type - the class
 *  in whose hierarchy the linker starts its dispatch search. */
final case class PyApply(
    flags:     PyApplyFlags,
    receiver:  PyTree,
    className: PyClassName,
    method:    PyMethodName,
    args:      List[PyTree]
)(val tpe: PyType, val pos: PyPosition) extends PyTree

/** Statically dispatched call (super calls, final method calls).
 *
 *  `className` names the exact class in which the method is resolved. */
final case class PyApplyStatically(
    flags:     PyApplyFlags,
    receiver:  PyTree,
    className: PyClassName,
    method:    PyMethodName,
    args:      List[PyTree]
)(val tpe: PyType, val pos: PyPosition) extends PyTree

/** Module-level / static method call (no receiver). */
final case class PyApplyStatic(
    flags:     PyApplyFlags,
    className: PyClassName,
    method:    PyMethodName,
    args:      List[PyTree]
)(val tpe: PyType, val pos: PyPosition) extends PyTree

/** Call into a Python builtin / runtime helper / external library.
 *  Linker-opaque - no reachability info is harvested from `callee`. */
final case class PyApplyExternal(callee: PyExternalName, args: List[PyTree])
    (val tpe: PyType, val pos: PyPosition) extends PyTree

/** Reference to an external Python value reached by importing `module`
 *  and walking `path`.
 *
 *  Linker-opaque: the emitter renders this as an import alias followed by
 *  attribute accesses. */
final case class PyExternalRef(module: String, path: List[String])
    (val tpe: PyType, val pos: PyPosition) extends PyTree

/** Native Python attribute access `obj.name`.
 *
 *  Linker-opaque. Used for facade-owned selects whose receiver is not a
 *  `PyExternalRef` (e.g. local variables holding facade values, `this`
 *  expressions, or facade instances). This avoids emitting the noisier
 *  `getattr(obj, "name")` fallback when the attribute name is known at
 *  compile time. It also supports the LHS of `updateDynamic` attribute
 *  assignment. */
final case class PyAttrAccess(obj: PyTree, name: String)
    (val tpe: PyType, val pos: PyPosition) extends PyAssignable

/** Dynamic call on a Python value with positional + keyword arguments.
 *
 *  The `callee` can be an external reference, a `getattr(...)` result,
 *  or any other expression producing a callable Python value. */
final case class PyApplyDynamic(
    callee: PyTree,
    args:   List[PyTree],
    kwargs: List[(String, PyTree)]
)(val tpe: PyType, val pos: PyPosition) extends PyTree

// ===================================================================
//  Object construction
// ===================================================================

final case class PyNew(
    className: PyClassName,
    ctor:      PyMethodName,
    args:      List[PyTree]
)(val pos: PyPosition) extends PyTree:
  def tpe: PyType = PyClassType(className)

final case class PyLoadModule(className: PyClassName)
    (val pos: PyPosition) extends PyTree:
  def tpe: PyType = PyClassType(className)

// ===================================================================
//  Type tests / casts
// ===================================================================

final case class PyIsInstanceOf(expr: PyTree, testType: PyTypeRef)
    (val pos: PyPosition) extends PyTree:
  val tpe: PyType = PyBooleanType

final case class PyAsInstanceOf(expr: PyTree, override val tpe: PyType)
    (val pos: PyPosition) extends PyTree

// ===================================================================
//  Arrays
// ===================================================================

final case class PyNewArray(elemTypeRef: PyTypeRef, length: PyTree)
    (val pos: PyPosition) extends PyTree:
  val tpe: PyType = PyArrayType

final case class PyArrayValue(elemTypeRef: PyTypeRef, elems: List[PyTree])
    (val pos: PyPosition) extends PyTree:
  val tpe: PyType = PyArrayType

final case class PyArraySelect(array: PyTree, index: PyTree)
    (val tpe: PyType, val pos: PyPosition) extends PyAssignable

// ===================================================================
//  Operators
// ===================================================================

final case class PyUnaryOp(op: PyUnaryCode, lhs: PyTree)
    (val pos: PyPosition) extends PyTree:
  def tpe: PyType = PyUnaryCode.resultTypeOf(op)

final case class PyBinaryOp(op: PyBinaryCode, lhs: PyTree, rhs: PyTree)
    (val pos: PyPosition) extends PyTree:
  def tpe: PyType = PyBinaryCode.resultTypeOf(op)

// ===================================================================
//  Closures
// ===================================================================

/** Scala lambda.
 *
 *  Captures are NOT modeled here: Scala-erasure-phase lambdas already
 *  encode their captured values as the leading parameters of the
 *  target method. The PyIR-level closure node is a plain Python lambda
 *  whose body forwards to that method, and Python's lexical scope
 *  captures any free identifiers (e.g. `self` for instance-method
 *  targets) automatically. Earlier revisions carried `captureParams` /
 *  `captureValues` mirroring sjsir, but `GenPython.genClosure` always
 *  emitted them empty. Dropping the fields removes the foot-gun where
 *  the emitter ignored them. */
final case class PyClosure(
    params:     List[PyParamDef],
    resultType: PyType,
    body:       PyTree
)(val pos: PyPosition) extends PyTree:
  val tpe: PyType = PyAnyType

// ===================================================================
//  Misc
// ===================================================================

final case class PyClassOf(typeRef: PyTypeRef)(val pos: PyPosition) extends PyTree:
  def tpe: PyType = PyClassType(PyClassName.ClassClass)

// ===================================================================
//  Literals
// ===================================================================

final case class PyBooleanLit(value: Boolean)(val pos: PyPosition)
    extends PyMatchableLiteral:
  val tpe: PyType = PyBooleanType

final case class PyCharLit(value: Char)(val pos: PyPosition)
    extends PyMatchableLiteral:
  val tpe: PyType = PyCharType

final case class PyByteLit(value: Byte)(val pos: PyPosition)
    extends PyMatchableLiteral:
  val tpe: PyType = PyByteType

final case class PyShortLit(value: Short)(val pos: PyPosition)
    extends PyMatchableLiteral:
  val tpe: PyType = PyShortType

final case class PyIntLit(value: Int)(val pos: PyPosition)
    extends PyMatchableLiteral:
  val tpe: PyType = PyIntType

final case class PyLongLit(value: Long)(val pos: PyPosition)
    extends PyMatchableLiteral:
  val tpe: PyType = PyLongType

final case class PyFloatLit(value: Float)(val pos: PyPosition) extends PyLiteral:
  val tpe: PyType = PyFloatType

final case class PyDoubleLit(value: Double)(val pos: PyPosition) extends PyLiteral:
  val tpe: PyType = PyDoubleType

final case class PyStringLit(value: String)(val pos: PyPosition)
    extends PyMatchableLiteral:
  val tpe: PyType = PyStringType

final case class PyNullLit()(val pos: PyPosition) extends PyMatchableLiteral:
  val tpe: PyType = PyNullType

final case class PyUnitLit()(val pos: PyPosition) extends PyLiteral:
  val tpe: PyType = PyVoidType
