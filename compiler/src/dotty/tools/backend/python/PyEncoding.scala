package dotty.tools.backend.python

import dotty.tools.dotc.core.*
import Contexts.*
import Flags.*
import Names.*
import Symbols.*
import Types.*
import Denotations.*

import dotty.tools.backend.python.ir.*

import scala.collection.mutable

/** Translates Scala symbols and types into Python IR names and type annotations. */
class PyEncoding(using Context):

  // ─── Fresh name generation ─────────────────────────────────────────

  private val nameCounters = mutable.Map.empty[String, Int]

  def freshName(base: String): PyName =
    val count = nameCounters.getOrElseUpdate(base, 0)
    nameCounters(base) = count + 1
    if count == 0 then PyName(base) else PyName(s"${base}_$count")

  def resetLocalNames(): Unit =
    nameCounters.clear()

  // ─── Class names ───────────────────────────────────────────────────

  def encodeClassName(sym: Symbol): QualName =
    val name = sym.javaClassName.toString
    // Strip trailing $ from module classes
    val clean = if sym.is(ModuleClass) && name.endsWith("$") then name.dropRight(1) else name
    QualName(clean.split('.').toList)

  def encodeSimpleClassName(sym: Symbol): PyName =
    val parts = encodeClassName(sym).parts
    PyName(parts.last)

  // ─── Method names ──────────────────────────────────────────────────

  private val operatorMap: Map[String, String] = Map(
    "$plus"     -> "__add__",
    "$minus"    -> "__sub__",
    "$times"    -> "__mul__",
    "$div"      -> "__truediv__",
    "$percent"  -> "__mod__",
    "$less"     -> "__lt__",
    "$greater"  -> "__gt__",
    "$amp"      -> "__and__",
    "$bar"      -> "__or__",
    "$up"       -> "__xor__",
    "$tilde"    -> "__invert__",
    "$eq$eq"    -> "__eq__",
    "$bang$eq"  -> "__ne__",
    "$less$eq"  -> "__le__",
    "$greater$eq" -> "__ge__",
    "$hash$hash" -> "__hash__",
    "toString"  -> "__str__",
    "hashCode"  -> "__hash__",
    "equals"    -> "__eq__",
  )

  def encodeMethodName(sym: Symbol): PyName =
    if sym.isClassConstructor then PyName("__init__")
    else
      val name = sym.name.mangledString
      PyName(operatorMap.getOrElse(name, sanitizeName(name)))

  // ─── Field names ───────────────────────────────────────────────────

  def encodeFieldName(sym: Symbol): PyName =
    val name = sym.name.mangledString
    if sym.is(Private) then
      val ownerName = sym.owner.name.mangledString.stripSuffix("$")
      PyName(s"_${ownerName}__${sanitizeName(name)}")
    else
      PyName(sanitizeName(name))

  // ─── Local variable names ──────────────────────────────────────────

  def encodeLocalName(sym: Symbol): PyName =
    PyName(sanitizeName(sym.name.mangledString))

  def encodeLabelName(sym: Symbol): PyName =
    PyName(sanitizeName(sym.name.mangledString))

  // ─── Type annotations ─────────────────────────────────────────────

  def toPyType(tp: Type): Option[PyTypeAnnot] =
    val sym = tp.typeSymbol
    if sym == defn.IntClass then Some(PyTypeAnnot.Named("int"))
    else if sym == defn.LongClass then Some(PyTypeAnnot.Named("int"))
    else if sym == defn.FloatClass then Some(PyTypeAnnot.Named("float"))
    else if sym == defn.DoubleClass then Some(PyTypeAnnot.Named("float"))
    else if sym == defn.BooleanClass then Some(PyTypeAnnot.Named("bool"))
    else if sym == defn.StringClass then Some(PyTypeAnnot.Named("str"))
    else if sym == defn.UnitClass then Some(PyTypeAnnot.Named("None"))
    else if sym == defn.CharClass then Some(PyTypeAnnot.Named("int"))
    else if sym == defn.ByteClass then Some(PyTypeAnnot.Named("int"))
    else if sym == defn.ShortClass then Some(PyTypeAnnot.Named("int"))
    else if sym == defn.NothingClass then Some(PyTypeAnnot.Named("None"))
    else if sym == defn.NullClass then Some(PyTypeAnnot.Named("None"))
    else if sym == defn.ObjectClass then Some(PyTypeAnnot.Any)
    else if sym == defn.ArrayClass then
      tp match
        case AppliedType(_, List(elemTp)) =>
          toPyType(elemTp).map(et => PyTypeAnnot.Parameterized(PyTypeAnnot.Named("list"), List(et)))
        case _ => Some(PyTypeAnnot.Named("list"))
    else if sym.exists && !sym.isAbstractOrParamType then
      Some(PyTypeAnnot.Qualified(encodeClassName(sym)))
    else Some(PyTypeAnnot.Any)

  /** Determine whether an erased Scala type corresponds to a Python int (needs wrapping). */
  def isIntType(tp: Type): Boolean =
    val sym = tp.typeSymbol
    sym == defn.IntClass || sym == defn.ByteClass || sym == defn.ShortClass || sym == defn.CharClass

  def isLongType(tp: Type): Boolean =
    tp.typeSymbol == defn.LongClass

  def isFloatType(tp: Type): Boolean =
    tp.typeSymbol == defn.FloatClass

  def isDoubleType(tp: Type): Boolean =
    tp.typeSymbol == defn.DoubleClass

  def isBooleanType(tp: Type): Boolean =
    tp.typeSymbol == defn.BooleanClass

  def isStringType(tp: Type): Boolean =
    tp.typeSymbol == defn.StringClass

  // ─── Utilities ─────────────────────────────────────────────────────

  /** Python reserved words that must be escaped. */
  private val pythonKeywords = Set(
    "False", "None", "True", "and", "as", "assert", "async", "await",
    "break", "class", "continue", "def", "del", "elif", "else", "except",
    "finally", "for", "from", "global", "if", "import", "in", "is",
    "lambda", "nonlocal", "not", "or", "pass", "raise", "return",
    "try", "while", "with", "yield"
  )

  private def sanitizeName(name: String): String =
    val cleaned = name.replace("$", "_")
    if pythonKeywords.contains(cleaned) then cleaned + "_"
    else cleaned
