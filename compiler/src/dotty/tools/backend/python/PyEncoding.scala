package dotty.tools.backend.python

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.*
import Annotations.Annotation
import Constants.*
import Contexts.*
import Flags.*
import Names.*
import NameOps.*
import Symbols.*
import Types.*

import dotty.tools.dotc.report

import dotty.tools.backend.python.ir.pyir.*

final case class ExternBinding(module: String, path: List[String])

/** Translates Scala symbols and types into PyIR names and type references.
 *
 *  This is the boundary between the compiler's `Symbol`/`Type` world and
 *  the IR's typed `PyName`/`PyTypeRef` world. Every identifier in the IR
 *  comes from here.
 */
class PyEncoding(using Context):
  private val pyDefn = PyDefinitions.pydefn
  private val reportedMalformedExterns = scala.collection.mutable.Set.empty[Symbol]

  // --- Class names ---------------------------------------------------

  def encodeClassName(sym: Symbol): PyClassName =
    // For Java-defined module classes, re-anchor to the companion class.
    val rewired =
      if sym.isAllOf(ModuleClass | JavaDefined) && sym.linkedClass.exists then
        sym.linkedClass
      else sym
    // Preserve any trailing `$` on a Scala module class so a companion
    // object and its companion class do not collide (the case class and
    // its companion module otherwise both encode to the same name and
    // the linker reports `Duplicate class`). `sanitizeName` later turns
    // each `$` into `_`.
    //
    // Scala source identifiers may THEMSELVES end with `$` (e.g.
    // `class abc$`). After plain `$ → _` sanitization, the encoded name
    // ends in `_` and is indistinguishable from a module class encoded
    // name. The `companionClassNameOf` relation then aliases this regular
    // class to the companion slot of the corresponding `object abc$`'s
    // encoded form (`abc__`), and the linker emits a `Duplicate class`
    // when both are present in the same CU. To break this collision by
    // construction, we route the LAST path segment through a
    // module-aware sanitization that escapes a non-module class's
    // trailing user `$` with the reserved `_scpy_d` marker. Module
    // classes keep the today's plain `$ → _` mapping (their auto-suffix
    // `$` is already the trailing `_`), so:
    //
    //   class Foo     -> Foo            object Foo     -> Foo_
    //   class Foo$    -> Foo_scpy_d     object Foo$    -> Foo__
    //
    // `companionClassNameOf` is updated to perform the inverse flip.
    val raw = rewired.javaClassName.toString
    val rawSegments = raw.split('.').toList
    val segments =
      if rawSegments.isEmpty then rawSegments
      else
        val initSegs = rawSegments.init.map(sanitizeName)
        val lastSeg  = sanitizeClassSimpleName(rawSegments.last, rewired.is(ModuleClass))
        initSegs :+ lastSeg
    PyClassName(segments.mkString("."))

  /** Sanitize a class's last path segment.
   *
   *  For module classes: identical to `sanitizeName`, since dotc has
   *  appended exactly one `$` auto-suffix and the resulting trailing
   *  `_` is the module marker we rely on.
   *
   *  For non-module classes: if the source name ends in `$`, escape that
   *  trailing `$` with the reserved `_scpy_d` marker instead of the
   *  plain `_`. This guarantees a non-module class's encoded name never
   *  ends in `_`, so it cannot be confused with a module class. */
  private def sanitizeClassSimpleName(name: String, isModule: Boolean): String =
    if !isModule && name.nonEmpty && name.last == '$' then
      // Replace the trailing user `$` with the reserved `_scpy_d`
      // escape; sanitize the rest under the plain `$ → _` rule.
      val prefix = sanitizeName(name.dropRight(1))
      prefix + PyEncoding.UserDollarSuffix
    else
      sanitizeName(name)

  // --- Method names --------------------------------------------------

  // NOTE: Scala operator methods (`$plus`, `$minus`, `$times`, ...) are
  // intentionally NOT mapped to Python dunder names (`__add__`, `__sub__`,
  // ...). Mapping them was cosmetic but unsafe: Scala can have multiple
  // overloads of `+` (e.g. `MapOps.+(kv)` and `MapOps.+(e1, e2, elems*)`)
  // and `PyMethodName.encoded` drops the signature suffix for dunder
  // names, so all overloads collapsed onto the same Python identifier
  // and the synthesized forwarders became tautological self-calls
  // (`return self.__add__(kv)`). See
  // `notes/issue-treemap-plus-self-recursive.md`. The codegen never emits
  // Python `+`/`-`/... infix syntax for user classes (primitives use
  // `PyBinaryOp`; Python facade interop uses `PyApplyDynamic`), so user
  // classes don't need dunder names at all. Methods like Scala `equals`
  // and `hashCode`, which Python's runtime DOES invoke through `__eq__`
  // / `__hash__`, are still rerouted by `specialMethodNameOf` below; they
  // are nullary or single-arg with no Scala-level overloads, so they
  // can't collide.

  def encodeMethodName(sym: Symbol): PyMethodName =
    val simpleName =
      if sym.isClassConstructor then PySimpleMethodName.Constructor
      else
        val rawName = sym.name.mangledString
        PySimpleMethodName(
          specialMethodNameOf(sym, rawName).getOrElse(sanitizeName(rawName))
        )
    // dotc reports the `info.resultType` of a constructor (class `<init>`
    // or trait `$init$`, post-`Mixin`) as the enclosing class, even
    // though bodies and call sites act as if it were Unit. Patch to
    // VoidRef so the encoded signature is stable across def site and
    // call site (mirrors SJS's `patchedResultType` in
    // `JSEncoding.scala`). Use the broad `isConstructor` test, which
    // returns true for both `CONSTRUCTOR` and `TRAIT_CONSTRUCTOR`.
    val resultRef =
      if sym.isConstructor then PyPrimRef.VoidRef
      else encodeTypeRef(sym.info.finalResultType)
    PyMethodName(simpleName, paramTypeRefsOf(sym), resultRef)

  private def specialMethodNameOf(sym: Symbol, rawName: String): Option[String] =
    rawName match
      case "hashCode" if sym.info.paramInfoss.flatten.isEmpty =>
        Some("__hash__")
      case "equals" if isEqualsAnyOverload(sym) =>
        // Only the canonical `equals(Any)` (post-erasure: `equals(Object)`)
        // is rerouted to Python's `__eq__`. Typed overloads such as
        // `BigDecimal.equals(that: BigDecimal)` keep their mangled name so
        // they do not clobber the `__eq__` slot — Python invokes `__eq__`
        // for ANY comparand, and a typed overload's body can assume the
        // narrow type and crash when called with something else (e.g.
        // `BigDecimal == None`). See `notes/issue-nonetype-value-class-accessor.md`.
        Some("__eq__")
      case _ =>
        None

  private def isEqualsAnyOverload(sym: Symbol): Boolean =
    sym.info.paramInfoss.flatten match
      case paramInfo :: Nil =>
        val ps = paramInfo.typeSymbol
        ps == defn.AnyClass || ps == defn.ObjectClass
      case _ =>
        false

  private def paramTypeRefsOf(sym: Symbol): List[PyTypeRef] =
    sym.info.paramInfoss.flatten.map(encodeTypeRef)

  // --- Field names ---------------------------------------------------

  def encodeFieldName(sym: Symbol): PyFieldName =
    // Owner-mangle fields whose user-facing access is genuinely
    // private — i.e. no public/protected accessor exposes them. This
    // matches JVM `resolveField`-by-declaring-class semantics for the
    // private case (so a subclass `val msg` and a parent's private
    // `val msg` don't alias to a single Python attribute), while
    // keeping simple names for fields that participate in the public
    // API (so JDK-typed code can reach pylib `final val`s by spelling,
    // and hand-written runtime classes like `BoxedUnit.UNIT` /
    // `IntRef.elem` interoperate by simple name).
    //
    // A `final val x` in a Scala object generates a private backing
    // field PLUS a public accessor `def x`. dotc reports the field
    // sym as `is(Private)`, but the user-source-level name `x` is
    // public — readers go through the accessor or, for JDK-typed
    // cross-language references, name `x` directly. We therefore
    // consult the accessor's privacy when one exists, falling back
    // to the symbol's own privacy when there's no accessor (true
    // private storage like `Throwable.msg`).
    val getter = sym.getter
    val isUserPrivate =
      if getter.exists then getter.is(Private)
      else sym.is(Private)
    PyFieldName(
      encodeClassName(sym.owner),
      PySimpleFieldName(sanitizeName(sym.name.mangledString)),
      isPrivate = isUserPrivate
    )

  // --- Locals / labels -----------------------------------------------
  //
  // Per-method scoping: two distinct symbols can mangle to the same Scala
  // string (e.g. `x$1` from an outer anonfun and `x$1` from an inner
  // anonfun lifted into the same static helper). Without scoping, both
  // would emit as the same Python identifier — Python rejects duplicate
  // parameter names, and a function body would silently shadow.
  //
  // Mirrors Scala.js's `withNewLocalNameScope` + `freshName` mechanism
  // (`inbox/scala-js/.../JSEncoding.scala:67-156`). The scope is a
  // `(usedNames, symToName)` pair pushed by `withLocalScope` and consumed
  // by `encodeLocalName` / `encodeLabelName`. When no scope is active
  // we fall back to the raw mangled name (used by call sites that build
  // synthetic locals during forwarder synthesis, etc.).

  private case class LocalScope(
      usedLocals:     scala.collection.mutable.Set[String]               = scala.collection.mutable.Set.empty,
      localBySymbol:  scala.collection.mutable.Map[Symbol, PyLocalName]  = scala.collection.mutable.Map.empty,
      usedLabels:     scala.collection.mutable.Set[String]               = scala.collection.mutable.Set.empty,
      labelBySymbol:  scala.collection.mutable.Map[Symbol, PyLabelName]  = scala.collection.mutable.Map.empty
  )
  private var currentLocalScope: LocalScope | Null = null

  def withLocalScope[A](body: => A): A =
    val saved = currentLocalScope
    currentLocalScope = LocalScope()
    try body
    finally currentLocalScope = saved

  /** Reserve a name in the current local scope so subsequent
   *  `encodeLocalName` calls for new symbols pick a unique alternative.
   *
   *  This is used to model Python's function-scope rule: any local
   *  assignment to name `X` makes every other reference to `X` in the
   *  same `def` resolve to the local — the module-scope binding is
   *  shadowed for the whole function. So when a method body reads a
   *  module-scope identifier (e.g. a top-level class) and we are about
   *  to encode locals for that method, we must reserve those module
   *  identifiers as if they were already locals. The
   *  `encodeLocalName`'s `freshUnique` machinery then routes any
   *  colliding user local through a `_2` suffix instead of silently
   *  shadowing the module binding and producing
   *  `UnboundLocalError` / `NameError` at runtime.
   *
   *  No-op when no local scope is active (the encoder still emits raw
   *  names in that case). */
  def reserveLocalName(name: String): Unit =
    currentLocalScope match
      case null  => ()
      case scope => scope.usedLocals += name

  /** The bare Python identifier that the emitter will use for class
   *  `cn` at module scope.
   *
   *  Mirrors the non-runtime branch of
   *  `PyIREmitter.Emitter.classIdentifier`: every non-runtime class is
   *  rendered as its mangled FQN (`segments.map(sanitizeIdent)
   *  .mkString("_")`). Runtime-provided classes and emitter
   *  reserved-short-names (e.g. `java.lang.Exception` ->
   *  `_scpy_java_Exception`) are remapped at emit time via tables this
   *  encoder doesn't own; reservation against those is handled
   *  elsewhere or already starts with `_scpy_` (which is itself
   *  reserved by `sanitizeName`).
   *
   *  Used by `reserveLocalName` callers that want to seed the local
   *  scope with class identifiers a method body will read at module
   *  scope. */
  def classIdentifierOf(cn: PyClassName): String =
    cn.segments.map(sanitizeName).mkString("_")

  private def freshUnique(base: String, used: scala.collection.mutable.Set[String]): String =
    if !used.contains(base) then
      used += base; base
    else
      var n = 2
      while used.contains(s"${base}_${n}") do n += 1
      val out = s"${base}_${n}"
      used += out
      out

  def encodeLocalName(sym: Symbol): PyLocalName =
    val raw = sanitizeName(sym.name.mangledString)
    currentLocalScope match
      case null => PyLocalName(raw)
      case scope =>
        scope.localBySymbol.getOrElseUpdate(sym, PyLocalName(freshUnique(raw, scope.usedLocals)))

  def encodeLabelName(sym: Symbol): PyLabelName =
    val raw = sanitizeName(sym.name.mangledString)
    currentLocalScope match
      case null => PyLabelName(raw)
      case scope =>
        scope.labelBySymbol.getOrElseUpdate(sym, PyLabelName(freshUnique(raw, scope.usedLabels)))

  // --- Type encoding -------------------------------------------------

  /** Convert a post-erasure Scala type to a `PyTypeRef` (for method
   *  signatures and IsInstanceOf test types).
   *
   *  Post-erasure, `Array[T]` is a `JavaArrayType(elem)`, not
   *  `AppliedType(ArrayClass, ...)`. Match accordingly.
   */
  def encodeTypeRef(tp: Type): PyTypeRef =
    tp match
      case JavaArrayType(elem) =>
        encodeTypeRef(elem) match
          case PyArrayRef(base, dims) => PyArrayRef(base, dims + 1)
          case other                  => PyArrayRef(other, 1)
      case _ =>
        val sym = tp.typeSymbol
        if isFacadeSymbol(sym) then PyClassRef(PyClassName.ObjectClass)
        else if sym == defn.IntClass then PyPrimRef.IntRef
        else if sym == defn.LongClass then PyPrimRef.LongRef
        else if sym == defn.FloatClass then PyPrimRef.FloatRef
        else if sym == defn.DoubleClass then PyPrimRef.DoubleRef
        else if sym == defn.BooleanClass then PyPrimRef.BooleanRef
        else if sym == defn.CharClass then PyPrimRef.CharRef
        else if sym == defn.ByteClass then PyPrimRef.ByteRef
        else if sym == defn.ShortClass then PyPrimRef.ShortRef
        else if sym == defn.UnitClass then PyPrimRef.VoidRef
        else if sym == defn.NothingClass then PyPrimRef.NothingRef
        else if sym == defn.NullClass then PyPrimRef.NullRef
        else if sym == defn.StringClass then PyClassRef(PyClassName.StringClass)
        else if sym.exists && sym.isClass then PyClassRef(encodeClassName(sym))
        else PyClassRef(PyClassName.ObjectClass)

  /** Convert a post-erasure Scala type to a runtime `PyType` (attached to
   *  `PyTree` nodes). */
  def encodeType(tp: Type): PyType =
    tp match
      case _: JavaArrayType => PyArrayType
      case _ =>
        val sym = tp.typeSymbol
        if isFacadeSymbol(sym) then PyAnyType
        else if sym == defn.IntClass then PyIntType
        else if sym == defn.LongClass then PyLongType
        else if sym == defn.FloatClass then PyFloatType
        else if sym == defn.DoubleClass then PyDoubleType
        else if sym == defn.BooleanClass then PyBooleanType
        else if sym == defn.CharClass then PyCharType
        else if sym == defn.ByteClass then PyByteType
        else if sym == defn.ShortClass then PyShortType
        else if sym == defn.UnitClass then PyVoidType
        else if sym == defn.NothingClass then PyNothingType
        else if sym == defn.NullClass then PyNullType
        else if sym == defn.StringClass then PyStringType
        else if sym.exists && sym.isClass then PyClassType(encodeClassName(sym))
        else PyAnyType

  /** Original source name for diagnostics. */
  def originalNameOf(sym: Symbol): PyOriginalName =
    PyOriginalName.fromString(sym.name.unexpandedName.toString)

  /** JVM-style dotted full name for a class symbol (e.g. `pkg.Outer$Inner`,
   *  `Outer$$anon$1`).  Used by codegen to populate
   *  `PyClassDef.originalName` for class definitions so the runtime can
   *  return JVM-shaped output from `Class.getName()` rather than the
   *  Python-encoded form (`_` for `$`, `_scpy_d` for trailing `$`, …).
   *  This is the same name that the JVM backend uses for class files. */
  def jvmClassNameOf(sym: Symbol): String =
    val rewired =
      if sym.isAllOf(ModuleClass | JavaDefined) && sym.linkedClass.exists then
        sym.linkedClass
      else sym
    rewired.javaClassName.toString

  // --- Python interop annotations -----------------------------------

  def externBindingOf(sym: Symbol): Option[ExternBinding] =
    annotationCarrierSymbols(sym).iterator.collectFirst(Function.unlift(readExternBinding))

  def externMemberNameOf(sym: Symbol): String =
    annotationCarrierSymbols(sym).iterator
      .collectFirst(Function.unlift(readNameOverride))
      .orElse(sym.allOverriddenSymbols.collectFirst(Function.unlift(readNameOverride)))
      .getOrElse(sanitizeName(sym.name.mangledString))

  def isFacadeOwner(sym: Symbol): Boolean =
    sym.exists && (
      externBindingOf(sym).isDefined ||
      sym.allOverriddenSymbols.exists(overridden => externBindingOf(overridden).isDefined)
    )

  /** True iff `sym` or one of its annotation carriers has an `@extern`
   *  annotation, valid or malformed. Used by codegen to short-circuit
   *  bodies of `@extern`-looking classes even when the annotation args
   *  are rejected by `externBindingOf`, so the user sees the facade
   *  diagnostic without also seeing downstream linker errors from the
   *  inline `native` body. */
  def hasExternAnnotation(sym: Symbol): Boolean =
    sym.exists && annotationCarrierSymbols(sym).exists(carrier =>
      carrier.annotations.exists(isExternAnnotation))

  def isFacadeSymbol(sym: Symbol): Boolean =
    sym.exists && sym.isClass && {
      val pyAny = pyDefn.PyAnyClass
      sym == pyAny || sym.asClass.baseClasses.contains(pyAny)
    }

  // --- isXxxType helpers (used by GenPython's primitive dispatch) ----

  def isIntType(tp: Type): Boolean =
    val sym = tp.typeSymbol
    sym == defn.IntClass || sym == defn.ByteClass ||
    sym == defn.ShortClass || sym == defn.CharClass

  def isLongType(tp: Type): Boolean = tp.typeSymbol == defn.LongClass
  def isFloatType(tp: Type): Boolean = tp.typeSymbol == defn.FloatClass
  def isDoubleType(tp: Type): Boolean = tp.typeSymbol == defn.DoubleClass
  def isBooleanType(tp: Type): Boolean = tp.typeSymbol == defn.BooleanClass
  def isStringType(tp: Type): Boolean = tp.typeSymbol == defn.StringClass
  def isCharType(tp: Type): Boolean =
    // Matches both primitive `scala.Char` and boxed `java.lang.Character`
    // (the latter is common in post-erasure `String + Char` trees).
    val sym = tp.typeSymbol
    sym == defn.CharClass || sym == defn.BoxedCharClass

  // --- Utilities -----------------------------------------------------

  /** Python reserved words that must be escaped in identifiers. */
  private val pythonKeywords = Set(
    "False", "None", "True", "and", "as", "assert", "async", "await",
    "break", "class", "continue", "def", "del", "elif", "else", "except",
    "finally", "for", "from", "global", "if", "import", "in", "is",
    "lambda", "nonlocal", "not", "or", "pass", "raise", "return",
    "try", "while", "with", "yield"
  )

  private val pyIdentifierRegex = "^[A-Za-z_][A-Za-z0-9_]*$".r

  /** True iff `name` can legally appear on the RHS of `.` in Python
   *  source - i.e. it is a valid identifier and not a reserved word.
   *  Names that fail this check must be accessed via `getattr` / `setattr`. */
  def isValidPyAttrName(name: String): Boolean =
    pyIdentifierRegex.matches(name) && !pythonKeywords.contains(name)

  /** Python identifiers that aren't keywords but DO clash with our
   *  emission conventions: `self` is the implicit instance receiver in
   *  every instance method / constructor, `cls` is the convention for
   *  classmethods. A user param named `self` (e.g. value-class accessors
   *  after erasure: `def ->(self: A, ...)`) collides with the synthetic
   *  receiver, producing `def __init__(self, self)` which Python rejects.
   *  Rename them here so the conflict can never arise. */
  private val pythonReservedConventions = Set("self", "cls")

  private def sanitizeName(name: String): String =
    val cleaned = name.replace('$', '_')
    if cleaned.startsWith("_scpy_") then
      report.warning(
        s"Scala identifier '$name' maps to Python name '$cleaned' which uses " +
        s"the reserved '_scpy_' prefix. This may collide with compiler-generated names.")
    // Python identifiers cannot start with a digit. Top-level defs in a
    // file with a numeric basename (e.g. `tests/run/16405.scala`) produce
    // a synthetic package class `16405$package$` whose first segment
    // begins with `1`. Prepend the reserved `_scpy_n` ("numeric") prefix
    // so the emitted Python identifier is well-formed. The `_scpy_`
    // namespace is reserved (line above warns on user collisions), so
    // this preserves injectivity vs every legal Scala identifier.
    val digitGuarded =
      if cleaned.nonEmpty && cleaned.head.isDigit then s"_scpy_n$cleaned"
      else cleaned
    if pythonKeywords.contains(digitGuarded) || pythonReservedConventions.contains(digitGuarded) then digitGuarded + "_"
    else digitGuarded

  private def annotationCarrierSymbols(sym: Symbol): List[Symbol] =
    List(
      sym,
      if sym.is(Module) then sym.moduleClass else NoSymbol,
      if sym.is(ModuleClass) then sym.sourceModule else NoSymbol
    ).filter(_.exists).distinct

  private def readExternBinding(sym: Symbol): Option[ExternBinding] =
    sym.annotations.find(isExternAnnotation) match
      case None => None
      case Some(annot) =>
        readStringArgs(annot) match
          case Some(module :: path) => Some(ExternBinding(module, path))
          case _ =>
            if reportedMalformedExterns.add(sym) then
              report.error(
                "`@extern` annotation requires at least one string literal argument " +
                  "(the Python module name); computed or non-literal arguments are not supported",
                sym.srcPos
              )
            None

  private def readNameOverride(sym: Symbol): Option[String] =
    sym.annotations.find(isNameAnnotation).flatMap(_.argumentConstantString(0))

  private def readStringArgs(annot: Annotation): Option[List[String]] =
    // Annotations for `@extern(module: String, path: String*)` arrive as a
    // list of argument trees: the first entry is the module and the rest
    // (typically wrapped in one vararg sequence tree) is the path.
    //
    // We require every argument to be a literal string; computed or
    // non-literal arguments are rejected so that the positional structure
    // cannot silently collapse.
    annot.arguments match
      case Nil =>
        None
      case first :: rest =>
        literalStringOf(first) match
          case None => None
          case Some(module) =>
            readVarargStrings(rest).map(module :: _)

  private def readVarargStrings(trees: List[Tree]): Option[List[String]] =
    trees match
      case Nil => Some(Nil)
      case varargTree :: Nil =>
        unwrapVarargs(varargTree).flatMap { elems =>
          val lits = elems.map(literalStringOf)
          if lits.forall(_.isDefined) then Some(lits.flatten)
          else None
        }
      case _ =>
        // Multiple post-first args without a vararg wrapper - treat each as literal.
        val lits = trees.map(literalStringOf)
        if lits.forall(_.isDefined) then Some(lits.flatten)
        else None

  private def literalStringOf(tree: Tree): Option[String] =
    tree match
      case Literal(const) if const.tag == StringTag => Some(const.stringValue)
      case Typed(inner, _)                         => literalStringOf(inner)
      case Inlined(_, Nil, expr)                   => literalStringOf(expr)
      case Block(Nil, expr)                        => literalStringOf(expr)
      case _                                       => None

  private def unwrapVarargs(tree: Tree): Option[List[Tree]] =
    tree match
      case seq: JavaSeqLiteral    => Some(seq.elems)
      case SeqLiteral(elems, _)   => Some(elems)
      case Typed(inner, _)        => unwrapVarargs(inner)
      case Inlined(_, Nil, expr)  => unwrapVarargs(expr)
      case Block(Nil, expr)       => unwrapVarargs(expr)
      case _                      => None

  // `pyDefn.ExternAnnotClass` and `pyDefn.NameAnnotClass` are validated to be
  // real (non-stub) classpath entries by `PyDefinitions.force()` at the start
  // of `GenPython`, so identity comparison is sufficient here — a `showFullName`
  // string fallback would be dead code under `-scalapy`.

  private def isExternAnnotation(annot: Annotation)(using Context): Boolean =
    annot.symbol eq pyDefn.ExternAnnotClass

  private def isNameAnnotation(annot: Annotation)(using Context): Boolean =
    annot.symbol eq pyDefn.NameAnnotClass

object PyEncoding:
  // --- Companion-class name convention -------------------------------
  //
  // Scala module classes encode with a trailing `_` (post-`sanitizeName`
  // mapping of the JVM `$` suffix). The companion class on the other side
  // of the relation has the same qualified name without that suffix —
  // EXCEPT when the user's source name itself ends in `$` (e.g.
  // `class abc$`), in which case the encoder escapes that trailing user
  // `$` with the reserved `_scpy_d` marker so the non-module class's
  // encoded name cannot be aliased to a module class slot. The
  // companion-flip rule below mirrors that escape.
  //
  // The two helpers below are the SINGLE source of truth for this
  // convention. Anywhere in the backend that needs to bridge the
  // `Foo` ⇄ `Foo_` relation (linker DCE, reachability, GenPython
  // forwarder synthesis) MUST go through them, never re-implement the
  // suffix flip inline.

  private val ModuleSuffix: String = "_"

  /** Reserved marker used to escape a non-module class's trailing user
   *  `$` so its encoded name never ends in `_` (the module marker).
   *  Lives in the `_scpy_*` reserved namespace; `sanitizeName` warns on
   *  user identifiers that intrude on this namespace. */
  private[python] val UserDollarSuffix: String = "_scpy_d"

  /** True iff `name` is a Scala module class encoded with the trailing
   *  `_` convention (e.g. `scala.Predef_`).
   *
   *  A regular class whose source name ends in `$` (and thus whose
   *  encoded name ends in `_scpy_d`) is intentionally NOT classified as
   *  a module class — that is precisely what the `_scpy_d` escape buys. */
  def isModuleClassName(name: PyClassName): Boolean =
    val s = name.nameString
    s.endsWith(ModuleSuffix) && !s.endsWith(UserDollarSuffix)

  /** The companion class on the other side of the underscore convention.
   *  Returns the suffix-flipped name regardless of direction:
   *  - `Foo`         -> `Foo_`
   *  - `Foo_`        -> `Foo`
   *  - `Foo_scpy_d`  -> `Foo__`        (class abc$ -> object abc$)
   *  - `Foo__`       -> `Foo_scpy_d`   (object abc$ -> class abc$)
   *
   *  Note this is purely a name-level operation; the caller is
   *  responsible for verifying that the returned name is actually
   *  bundled as a `PyClassDef`. */
  def companionClassNameOf(name: PyClassName): PyClassName =
    val s = name.nameString
    if s.endsWith(UserDollarSuffix) then
      // Non-module class whose source name ends in `$`. The companion
      // module class has its trailing-user-`$` encoded as plain `_`
      // (because module-class sanitize is uniform `$ -> _`) and an
      // additional `_` for the auto-suffix.
      PyClassName(s.dropRight(UserDollarSuffix.length) + "__")
    else if s.endsWith(ModuleSuffix) then
      // Module class. Drop the auto-suffix `_`. If the now-trailing char
      // is `_`, that came from a user `$` in the source name and the
      // companion class encodes it with the `_scpy_d` escape.
      val base = s.dropRight(ModuleSuffix.length)
      if base.endsWith(ModuleSuffix) then
        PyClassName(base.dropRight(ModuleSuffix.length) + UserDollarSuffix)
      else
        PyClassName(base)
    else
      // Plain non-module class. Companion module appends `_`.
      PyClassName(s + ModuleSuffix)

  /** Field on the companion class with the same simple name, produced via
   *  `companionClassNameOf`. Convenience for DCE passes that need to know
   *  whether a read of `Foo.x` should also keep `Foo_.x` alive (or vice
   *  versa). */
  def companionFieldOf(field: PyFieldName): PyFieldName =
    PyFieldName(companionClassNameOf(field.owner), field.simple, field.isPrivate)
