package dotty.tools.backend.python

import dotty.tools.dotc.core.*
import Contexts.*
import Flags.*
import Names.*
import NameOps.*
import Symbols.*
import Types.*

import dotty.tools.dotc.report

import dotty.tools.backend.python.ir.pyir.*

/** Translates Scala symbols and types into PyIR names and type references.
 *
 *  This is the boundary between the compiler's `Symbol`/`Type` world and
 *  the IR's typed `PyName`/`PyTypeRef` world. Every identifier in the IR
 *  comes from here.
 */
class PyEncoding(using Context):

  // --- Class names ---------------------------------------------------

  def encodeClassName(sym: Symbol): PyClassName =
    // For Java-defined module classes, re-anchor to the companion class.
    val rewired =
      if sym.isAllOf(ModuleClass | JavaDefined) && sym.linkedClass.exists then
        sym.linkedClass
      else sym
    val raw = rewired.javaClassName.toString
    val clean =
      if rewired.is(ModuleClass) && raw.endsWith("$") then raw.dropRight(1)
      else raw
    val segments = clean.split('.').toList.map(sanitizeName)
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
    "$hash$hash" -> "__hash__",
    "toString"   -> "__str__",
    "hashCode"   -> "__hash__",
    "equals"     -> "__eq__"
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
      val mapped = operatorMap.getOrElse(rawName, sanitizeName(rawName))
      PyMethodName(
        PySimpleMethodName(mapped),
        paramTypeRefsOf(sym),
        encodeTypeRef(sym.info.finalResultType)
      )

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
        if sym == defn.IntClass then PyPrimRef.IntRef
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
        if sym == defn.IntClass then PyIntType
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

  // --- Utilities -----------------------------------------------------

  /** Python reserved words that must be escaped in identifiers. */
  private val pythonKeywords = Set(
    "False", "None", "True", "and", "as", "assert", "async", "await",
    "break", "class", "continue", "def", "del", "elif", "else", "except",
    "finally", "for", "from", "global", "if", "import", "in", "is",
    "lambda", "nonlocal", "not", "or", "pass", "raise", "return",
    "try", "while", "with", "yield"
  )

  private def sanitizeName(name: String): String =
    val cleaned = name.replace('$', '_')
    if cleaned.startsWith("_scpy_") then
      report.warning(
        s"Scala identifier '$name' maps to Python name '$cleaned' which uses " +
        s"the reserved '_scpy_' prefix. This may collide with compiler-generated names.")
    if pythonKeywords.contains(cleaned) then cleaned + "_"
    else cleaned
