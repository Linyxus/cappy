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
    val raw = rewired.javaClassName.toString
    val segments = raw.split('.').toList.map(sanitizeName)
    PyClassName(segments.mkString("."))

  // --- Method names --------------------------------------------------

  /** Map from Scala operator-mangled names to Python dunder names.
   *  Purely cosmetic - these still go through `PyMethodName.encoded` with
   *  dunder special-casing so the signature suffix is dropped. */
  private val operatorMap: Map[String, String] = Map(
    "$plus"      -> "__add__",
    "$minus"     -> "__sub__",
    "$times"     -> "__mul__",
    "$div"       -> "__truediv__",
    "$percent"   -> "__mod__",
    "$less"      -> "__lt__",
    "$greater"   -> "__gt__",
    "$amp"       -> "__and__",
    "$bar"       -> "__or__",
    "$up"        -> "__xor__",
    "$tilde"     -> "__invert__",
    "$eq$eq"     -> "__eq__",
    "$bang$eq"   -> "__ne__",
    "$less$eq"   -> "__le__",
    "$greater$eq" -> "__ge__",
    "$hash$hash" -> "__hash__"
  )

  def encodeMethodName(sym: Symbol): PyMethodName =
    if sym.isClassConstructor then
      // dotc reports a constructor's `info.resultType` as the enclosing
      // class, not Unit. Patch to VoidRef so the method identity is
      // consistent with sjsir and so the encoded name collapses to `__init__`.
      PyMethodName(
        PySimpleMethodName.Constructor,
        paramTypeRefsOf(sym),
        PyPrimRef.VoidRef
      )
    else
      val rawName = sym.name.mangledString
      val mapped =
        specialMethodNameOf(sym, rawName)
          .orElse(operatorMap.get(rawName))
          .getOrElse(sanitizeName(rawName))
      PyMethodName(
        PySimpleMethodName(mapped),
        paramTypeRefsOf(sym),
        encodeTypeRef(sym.info.finalResultType)
      )

  private def specialMethodNameOf(sym: Symbol, rawName: String): Option[String] =
    rawName match
      case "toString" if sym.info.paramInfoss.flatten.isEmpty =>
        Some("__str__")
      case "hashCode" if sym.info.paramInfoss.flatten.isEmpty =>
        Some("__hash__")
      case "equals" if sym.info.paramInfoss.flatten.length == 1 =>
        Some("__eq__")
      case _ =>
        None

  private def paramTypeRefsOf(sym: Symbol): List[PyTypeRef] =
    sym.info.paramInfoss.flatten.map(encodeTypeRef)

  // --- Field names ---------------------------------------------------

  def encodeFieldName(sym: Symbol): PyFieldName =
    // The owner is baked in so a subclass field shadowing a parent field
    // produces a distinct PyFieldName. Python's attribute access uses only
    // the simple name, so shadowing works via normal Python semantics.
    PyFieldName(
      encodeClassName(sym.owner),
      PySimpleFieldName(sanitizeName(sym.name.mangledString))
    )

  // --- Locals / labels -----------------------------------------------

  def encodeLocalName(sym: Symbol): PyLocalName =
    PyLocalName(sanitizeName(sym.name.mangledString))

  def encodeLabelName(sym: Symbol): PyLabelName =
    PyLabelName(sanitizeName(sym.name.mangledString))

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

  private def sanitizeName(name: String): String =
    val cleaned = name.replace('$', '_')
    if cleaned.startsWith("_scpy_") then
      report.warning(
        s"Scala identifier '$name' maps to Python name '$cleaned' which uses " +
        s"the reserved '_scpy_' prefix. This may collide with compiler-generated names.")
    if pythonKeywords.contains(cleaned) then cleaned + "_"
    else cleaned

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

  private def isExternAnnotation(annot: Annotation)(using Context): Boolean =
    annot.symbol == pyDefn.ExternAnnotClass || annot.symbol.showFullName == "scala.python.extern"

  private def isNameAnnotation(annot: Annotation)(using Context): Boolean =
    annot.symbol == pyDefn.NameAnnotClass || annot.symbol.showFullName == "scala.python.name"
