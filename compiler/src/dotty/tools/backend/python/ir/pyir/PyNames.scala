package dotty.tools.backend.python.ir.pyir

/** Root of the PyIR name hierarchy.
 *
 *  Every identifier in the IR is one of a small number of typed kinds
 *  (class, method, field, local, label, simple-field, simple-method,
 *  external). The wild stringly-typed `PyName(String)` of the legacy IR
 *  is gone - `PyExternalName` is the explicit escape hatch for Python
 *  builtins and runtime helpers.
 */
sealed abstract class PyName extends Serializable:
  /** Mangled Python identifier this name encodes.
   *
   *  `encoded` is for emission, NOT for identity comparison. Use
   *  `equals` for identity.
   */
  def encoded: String

// --- Class name ----------------------------------------------------

/** Dot-separated, fully qualified Scala class name.
 *
 *  `PyClassName` is the linker pivot: every reachability question
 *  reduces to "starting from a `Set[PyClassName]`, which classes can
 *  we reach transitively?".
 *
 *  Equality is on the underlying string. Two `PyClassName` instances
 *  produced from the same qualified name (regardless of when or where)
 *  compare equal.
 */
final class PyClassName private (val nameString: String) extends PyName:
  def encoded: String = nameString

  /** Package and class segments, split on `.`. Non-empty. */
  def segments: List[String] = nameString.split('.').toList

  /** The last segment of `segments` - the un-qualified class name. */
  def simpleName: String =
    val dot = nameString.lastIndexOf('.')
    if dot < 0 then nameString else nameString.substring(dot + 1)

  override def hashCode: Int = nameString.hashCode

  override def equals(other: Any): Boolean = other match
    case that: PyClassName => this.nameString == that.nameString
    case _ => false

  override def toString: String = s"PyClassName($nameString)"

object PyClassName:
  def apply(nameString: String): PyClassName =
    require(nameString.nonEmpty, "PyClassName must be non-empty")
    new PyClassName(nameString)

  // Well-known class names used by the runtime / linker
  val ObjectClass:             PyClassName = apply("java.lang.Object")
  val StringClass:             PyClassName = apply("java.lang.String")
  val ClassClass:              PyClassName = apply("java.lang.Class")
  val ThrowableClass:          PyClassName = apply("java.lang.Throwable")
  val RuntimeExceptionClass:   PyClassName = apply("java.lang.RuntimeException")
  val NullPointerExceptionClass: PyClassName = apply("java.lang.NullPointerException")
  val ArithmeticExceptionClass: PyClassName = apply("java.lang.ArithmeticException")
  val ClassCastExceptionClass: PyClassName = apply("java.lang.ClassCastException")

// --- Local and label names -----------------------------------------

/** Local variable or parameter name. Intra-method scope; no owner. */
final case class PyLocalName(name: String) extends PyName:
  def encoded: String = name

object PyLocalName:
  /** Sentinel for the receiver binding (`self` in Python). */
  val This: PyLocalName = PyLocalName(".this")

/** Label name for `PyLabeled` / `PyLabelReturn` nodes. */
final case class PyLabelName(name: String) extends PyName:
  def encoded: String = name

// --- Field names ---------------------------------------------------

/** Unqualified field name. */
final case class PySimpleFieldName(name: String) extends PyName:
  def encoded: String = name

/** Fully-owned field name.
 *
 *  The owner class is baked into identity so two fields named `x`
 *  declared in parent and subclass are distinct - matches JVM field
 *  shadowing semantics and frees the emitter from manual mangling of
 *  shadowed fields.
 *
 *  `encoded` mirrors JVM `resolveField`-by-declaring-class semantics:
 *  PRIVATE fields are stored under a per-owner mangled attribute so
 *  parent and subclass keep separate slots even when both ctors run on
 *  the same instance (otherwise the parent's initializer overwrites the
 *  subclass-set value). PUBLIC / PROTECTED fields keep their simple
 *  name so hand-written runtime classes (e.g. `BoxedUnit.UNIT`,
 *  `IntRef.elem`) interoperate with codegen by spelling.
 *
 *  The mangled form uses `PyClassRef.encodeFqn` for owner injectivity
 *  (`_` → `_u`, `.` → `_d`). The leading `_scpy_f_` prefix keeps the
 *  encoded name in the project-reserved namespace and ensures the
 *  second character is `s`, NOT `_` — even when the owner FQN starts
 *  with `_` (encoded as `_u…`), so Python's compile-time
 *  `__name`-mangling rule never rewrites the field at the call site.
 */
final case class PyFieldName(
    owner: PyClassName,
    simple: PySimpleFieldName,
    isPrivate: Boolean = false
) extends PyName:
  def encoded: String =
    if isPrivate then
      s"_scpy_f_${PyClassRef.encodeFqn(owner.nameString)}__${simple.name}"
    else
      simple.name

// --- Method names --------------------------------------------------

/** Unqualified method name (no signature). */
final case class PySimpleMethodName(name: String) extends PyName:
  def encoded: String = name
  def isConstructor: Boolean = this == PySimpleMethodName.Constructor
  def isStaticInit: Boolean  = this == PySimpleMethodName.StaticInit

object PySimpleMethodName:
  val Constructor: PySimpleMethodName = PySimpleMethodName("<init>")
  val StaticInit:  PySimpleMethodName = PySimpleMethodName("<clinit>")

/** Method name with full Scala signature.
 *
 *  Identity: `(simple, paramTypeRefs, resultTypeRef)`.
 *
 *  The signature is preserved even though Python has no runtime
 *  overload resolution. Reason: Scala source can overload methods, and
 *  we need distinct Python identifiers for each overload. Encoding to
 *  a single Python name happens in `encoded`, mirroring sjsir's
 *  `foo__I__V` style.
 *
 *  The owner class is NOT part of `PyMethodName` - it appears on call
 *  sites (`PyApply.className`, `PyApplyStatic.className`, etc.).
 */
final case class PyMethodName(
    simple:        PySimpleMethodName,
    paramTypeRefs: List[PyTypeRef],
    resultTypeRef: PyTypeRef
) extends PyName:

  /** Stable Python identifier.
   *
   *  Rules:
   *   1. Constructors (`<init>`) → `__init__`, no suffix.
   *   2. Static initializers (`<clinit>`) → `_scpy_clinit`, no suffix.
   *   3. Python dunder names (`__foo__` pattern, length ≥ 5) → bare
   *      simple name, no suffix. Used for user-mapped `__add__`,
   *      `__str__`, etc. The builder is responsible for avoiding
   *      dunder-name collisions (no two overloads of `__add__`).
   *   4. Otherwise → `<simple>__<paramRefs>__<resultRef>`, with
   *      `paramRefs` as `ref1_ref2_...` (underscore-joined; empty if
   *      no params). Distinct `PyMethodName` values produce distinct
   *      strings.
   *
   *  Injectivity of the joined `paramRefs`/`resultRef` form depends on
   *  the underlying ref encodings keeping every internal `_` followed
   *  by a fixed lowercase escape letter (`u` for literal underscore,
   *  `d` for a class FQN segment break — see `PyClassRef.encoded`).
   *  All current `PyTypeRef` flavours start with an uppercase tag
   *  letter (`L`, `A`, `Z`, `I`, ...), so the inter-ref `_` separator
   *  is unambiguous against ref-internal `_u`/`_d`.
   */
  def encoded: String =
    if simple.isConstructor then "__init__"
    else if simple.isStaticInit then "_scpy_clinit"
    else if PyMethodName.isDunder(simple.name) then simple.name
    else
      val raw =
        if paramTypeRefs.isEmpty then
          s"${simple.name}__${resultTypeRef.encoded}"
        else
          val paramPart = paramTypeRefs.map(_.encoded).mkString("_")
          s"${simple.name}__${paramPart}__${resultTypeRef.encoded}"
      // Python rewrites identifiers of the form `__name` (>=2 leading
      // underscores, <=1 trailing) inside class bodies to
      // `_<ClassName>__name`. Our raw form starts with `__` whenever the
      // simple name itself starts with `_` (e.g. a Scala member named
      // `$` sanitizes to `_`, then prefixes `__<sig>`). Prepend a stable
      // non-underscore guard so the emitted call site is never rewritten
      // by Python's compile-time mangling.
      if raw.startsWith("__") then s"_scpy_m$raw" else raw

object PyMethodName:
  /** Convenience for a nullary signature (void result, no params). */
  def noArgs(simpleName: String): PyMethodName =
    PyMethodName(
      PySimpleMethodName(simpleName),
      Nil,
      PyPrimRef.VoidRef
    )

  /** True if `name` looks like a Python dunder: starts with `__`,
   *  ends with `__`, and is at least 5 characters (so `____` is not
   *  a dunder). */
  private[ir] def isDunder(name: String): Boolean =
    name.length >= 5 && name.startsWith("__") && name.endsWith("__")

// --- External names ------------------------------------------------

/** Opaque identifier for Python builtins, runtime helpers, external
 *  library symbols. The linker treats these as black boxes - no
 *  reachability information is extracted from them.
 *
 *  Use for: `print`, `len`, `int`, `range`, `isinstance`, `Exception`,
 *  `_scpy_i32`, `_scpy_mod_Predef`, etc.
 */
final case class PyExternalName(name: String) extends PyName:
  def encoded: String = name

// --- Original names ------------------------------------------------

/** Optional "original source name" annotation for a definition.
 *
 *  Preserves the Scala source identifier before mangling so diagnostics
 *  and comments can refer to the user-visible name. NOT part of
 *  identity - two definitions differing only in `originalName` should
 *  be considered equal by the linker.
 */
final case class PyOriginalName(value: Option[String]):
  def isDefined: Boolean = value.isDefined
  def isEmpty: Boolean = value.isEmpty
  def getOrElse(default: => String): String = value.getOrElse(default)

object PyOriginalName:
  val NoOriginalName: PyOriginalName = PyOriginalName(None)
  def fromString(s: String): PyOriginalName = PyOriginalName(Some(s))
