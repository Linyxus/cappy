package dotty.tools.backend.python.ir.pyir.serialization

import dotty.tools.backend.python.PyIREmitter
import dotty.tools.backend.python.ir.pyir.*
import dotty.tools.tasty.{TastyBuffer, TastyHash}

import java.io.OutputStream
import scala.collection.mutable
import scala.compiletime.uninitialized

/** Binary serializer for a single PyIR compilation unit.
 *
 *  Writes the header, five constant pools, body, and trailing hash.
 *  See `notes/pyir-serialisation.md` and `PyIRFormat` for the format.
 */
object PyIRSerializer:

  /** Serialize `classes` + an optional main entry into `out`. */
  def serialize(
      classes:   List[PyClassDef],
      mainEntry: Option[PyIREmitter.MainEntry],
      out:       OutputStream
  ): Unit =
    val s = new Serializer
    s.run(classes, mainEntry)
    out.write(s.assembled, 0, s.assembledLength)

  /** Convenience: serialize to a fresh `Array[Byte]`. */
  def serializeToBytes(
      classes:   List[PyClassDef],
      mainEntry: Option[PyIREmitter.MainEntry]
  ): Array[Byte] =
    val baos = new java.io.ByteArrayOutputStream()
    serialize(classes, mainEntry, baos)
    baos.toByteArray

  // =================================================================
  //  Stateful serializer instance
  // =================================================================

  private final class Serializer:
    import PyIRTags.*

    // Pools - order of insertion is the on-disk index.
    private val strings     = mutable.LinkedHashMap.empty[String, Int]
    private val classNames  = mutable.LinkedHashMap.empty[String, Int]
    private val typeRefs    = mutable.LinkedHashMap.empty[PyTypeRef, Int]
    private val methodNames = mutable.LinkedHashMap.empty[PyMethodName, Int]
    private val positions   = mutable.LinkedHashMap.empty[PyPosition, Int]

    // Index 0 of the position pool is reserved for NoPosition and is
    // not materialized on disk.
    positions(PyPosition.NoPosition) = 0

    private val body = new TastyBuffer(4096)

    var assembled:       Array[Byte] = uninitialized
    var assembledLength: Int         = 0

    // -------------------------------------------------------------
    //  Pool interning
    // -------------------------------------------------------------

    private def internString(s: String): Int =
      strings.get(s) match
        case Some(i) => i
        case None =>
          val i = strings.size
          strings(s) = i
          i

    private def internClassName(cn: PyClassName): Int =
      classNames.get(cn.nameString) match
        case Some(i) => i
        case None =>
          internString(cn.nameString)
          val i = classNames.size
          classNames(cn.nameString) = i
          i

    private def internTypeRef(tr: PyTypeRef): Int =
      typeRefs.get(tr) match
        case Some(i) => i
        case None =>
          // Recursively intern dependencies before assigning the parent
          // index, so the on-disk array is bottom-up.
          tr match
            case PyPrimRef(_)        => ()
            case PyClassRef(cn)      => internClassName(cn)
            case PyArrayRef(base, _) => internTypeRef(base)
          val i = typeRefs.size
          typeRefs(tr) = i
          i

    private def internMethodName(mn: PyMethodName): Int =
      methodNames.get(mn) match
        case Some(i) => i
        case None =>
          internString(mn.simple.name)
          mn.paramTypeRefs.foreach(internTypeRef)
          internTypeRef(mn.resultTypeRef)
          val i = methodNames.size
          methodNames(mn) = i
          i

    private def internPosition(p: PyPosition): Int =
      positions.get(p) match
        case Some(i) => i
        case None =>
          internString(p.source)
          val i = positions.size
          positions(p) = i
          i

    // -------------------------------------------------------------
    //  Body byte writers (TastyBuffer wrappers)
    // -------------------------------------------------------------

    private inline def b: TastyBuffer = body

    private def writeNat(x: Int): Unit  = b.writeNat(x)
    private def writeInt(x: Int): Unit  = b.writeInt(x)
    private def writeLong(x: Long): Unit = b.writeLongInt(x)
    private def writeBool(v: Boolean): Unit = b.writeByte(if v then 1 else 0)
    private def writeByteRaw(v: Int): Unit = b.writeByte(v)

    private def writeString(s: String): Unit =
      writeNat(internString(s))

    private def writeOptionalString(s: PyOriginalName): Unit =
      s.value match
        case None    => writeByteRaw(0)
        case Some(v) =>
          writeByteRaw(1)
          writeString(v)

    private def writeClassNameRef(cn: PyClassName): Unit =
      writeNat(internClassName(cn))

    private def writeTypeRefRef(tr: PyTypeRef): Unit =
      writeNat(internTypeRef(tr))

    private def writeMethodNameRef(mn: PyMethodName): Unit =
      writeNat(internMethodName(mn))

    private def writePosition(p: PyPosition): Unit =
      writeNat(internPosition(p))

    private def writeType(t: PyType): Unit =
      writeByteRaw(pyTypeTag(t).toInt & 0xff)
      t match
        case PyClassType(cn) => writeClassNameRef(cn)
        case _               => ()

    private def writeFieldName(f: PyFieldName): Unit =
      writeClassNameRef(f.owner)
      writeString(f.simple.name)
      writeBool(f.isPrivate)

    // -------------------------------------------------------------
    //  Top-level entry
    // -------------------------------------------------------------

    def run(
        classes:   List[PyClassDef],
        mainEntry: Option[PyIREmitter.MainEntry]
    ): Unit =
      // 1. Build the body in `body`, populating pools as a side effect.
      mainEntry match
        case None =>
          writeByteRaw(0)
        case Some((cn, kind)) =>
          writeByteRaw(1)
          writeClassNameRef(cn)
          writeByteRaw(classKindTag(kind).toInt & 0xff)

      writeNat(classes.size)
      classes.foreach(writeClassDef)

      // 2. Assemble: header + pools + body, then trailer hash.
      val out = new TastyBuffer(body.length + 4096)
      writeHeader(out)
      writeStringPool(out)
      writeClassNamePool(out)
      writeTypeRefPool(out)
      writeMethodNamePool(out)
      writePositionPool(out)
      out.writeBytes(body.bytes, body.length)

      val hash = TastyHash.pjwHash64(out.bytes, out.length)
      out.writeUncompressedLong(hash)

      assembled       = out.bytes
      assembledLength = out.length

    // -------------------------------------------------------------
    //  Header + pools (written to the final buffer)
    // -------------------------------------------------------------

    private def writeHeader(out: TastyBuffer): Unit =
      writeBE32(out, PyIRFormat.Magic)
      writeBE16(out, PyIRFormat.FormatVersion)
      // Compiler hash: hash of dotc version string so cross-compiler
      // mismatches surface as a warning at read time.
      val versionBytes =
        dotty.tools.dotc.config.Properties.versionString.getBytes("UTF-8")
      val versionHash = TastyHash.pjwHash64(versionBytes)
      out.writeUncompressedLong(versionHash)
      writeBE16(out, 0) // Flags - reserved

    private def writeBE32(out: TastyBuffer, v: Int): Unit =
      out.writeByte((v >>> 24) & 0xff)
      out.writeByte((v >>> 16) & 0xff)
      out.writeByte((v >>> 8) & 0xff)
      out.writeByte(v & 0xff)

    private def writeBE16(out: TastyBuffer, v: Int): Unit =
      out.writeByte((v >>> 8) & 0xff)
      out.writeByte(v & 0xff)

    private def writeStringPool(out: TastyBuffer): Unit =
      out.writeNat(strings.size)
      for (s, _) <- strings do
        // writeUtf8 emits Nat byteLen + bytes
        out.writeUtf8(s)

    private def writeClassNamePool(out: TastyBuffer): Unit =
      out.writeNat(classNames.size)
      for (nameStr, _) <- classNames do
        out.writeNat(strings(nameStr))

    private def writeTypeRefPool(out: TastyBuffer): Unit =
      out.writeNat(typeRefs.size)
      for (tr, _) <- typeRefs do
        tr match
          case PyPrimRef(tag) =>
            out.writeByte(TagPyPrimRef.toInt & 0xff)
            out.writeByte(primTag(tag).toInt & 0xff)
          case PyClassRef(cn) =>
            out.writeByte(TagPyClassRef.toInt & 0xff)
            out.writeNat(classNames(cn.nameString))
          case PyArrayRef(base, dims) =>
            out.writeByte(TagPyArrayRef.toInt & 0xff)
            out.writeNat(typeRefs(base))
            out.writeNat(dims)

    private def writeMethodNamePool(out: TastyBuffer): Unit =
      out.writeNat(methodNames.size)
      for (mn, _) <- methodNames do
        out.writeNat(strings(mn.simple.name))
        out.writeNat(mn.paramTypeRefs.size)
        mn.paramTypeRefs.foreach(p => out.writeNat(typeRefs(p)))
        out.writeNat(typeRefs(mn.resultTypeRef))

    private def writePositionPool(out: TastyBuffer): Unit =
      out.writeNat(positions.size)
      // Skip index 0 (NoPosition).
      for (p, idx) <- positions if idx != 0 do
        out.writeNat(strings(p.source))
        out.writeNat(p.line)
        out.writeNat(p.column)

    // -------------------------------------------------------------
    //  Definition writers
    // -------------------------------------------------------------

    private def writeClassDef(cls: PyClassDef): Unit =
      writePosition(cls.pos)
      writeClassNameRef(cls.name)
      writeOptionalString(cls.originalName)
      writeByteRaw(classKindTag(cls.kind).toInt & 0xff)
      cls.superClass match
        case None     => writeByteRaw(0)
        case Some(sc) =>
          writeByteRaw(1)
          writeClassNameRef(sc)
      writeNat(cls.interfaces.size)
      cls.interfaces.foreach(writeClassNameRef)
      writeNat(cls.fields.size)
      cls.fields.foreach(writeFieldDef)
      writeNat(cls.methods.size)
      cls.methods.foreach(writeMethodDef)

    private def writeFieldDef(f: PyFieldDef): Unit =
      writePosition(f.pos)
      writeNat(f.flags.bits)
      writeFieldName(f.name)
      writeOptionalString(f.originalName)
      writeType(f.ftpe)

    private def writeMethodDef(m: PyMethodDef): Unit =
      writePosition(m.pos)
      writeNat(m.flags.bits)
      writeMethodNameRef(m.name)
      writeOptionalString(m.originalName)
      writeNat(m.args.size)
      m.args.foreach(writeParamDef)
      writeType(m.resultType)
      m.body match
        case None    => writeByteRaw(0)
        case Some(t) =>
          writeByteRaw(1)
          writeTree(t)

    private def writeParamDef(p: PyParamDef): Unit =
      writePosition(p.pos)
      writeString(p.name.name)
      writeOptionalString(p.originalName)
      writeType(p.ptpe)
      writeBool(p.mutable)

    // -------------------------------------------------------------
    //  Tree writer (the long one)
    // -------------------------------------------------------------

    private def writeTree(t: PyTree): Unit =
      // Common header: tag + posIdx
      val tag = treeTag(t)
      writeByteRaw(tag.toInt & 0xff)
      writePosition(t.pos)
      writeTreePayload(t)

    private def treeTag(t: PyTree): Byte = t match
      case _: PyVarDef          => TagPyVarDef
      case _: PyAssign          => TagPyAssign
      case _: PyReturn          => TagPyReturn
      case _: PyWhile           => TagPyWhile
      case _: PySkip            => TagPySkip
      case _: PyIf              => TagPyIf
      case _: PyTryCatch        => TagPyTryCatch
      case _: PyTryFinally      => TagPyTryFinally
      case _: PyMatch           => TagPyMatch
      case _: PyBlock           => TagPyBlock
      case _: PyLabeled         => TagPyLabeled
      case _: PyLabelReturn     => TagPyLabelReturn
      case _: PyVarRef          => TagPyVarRef
      case _: PyThis            => TagPyThis
      case _: PySelect          => TagPySelect
      case _: PySelectStatic    => TagPySelectStatic
      case _: PyApply           => TagPyApply
      case _: PyApplyStatically => TagPyApplyStatically
      case _: PyApplyStatic     => TagPyApplyStatic
      case _: PyApplyExternal   => TagPyApplyExternal
      case _: PyExternalRef     => TagPyExternalRef
      case _: PyAttrAccess      => TagPyAttrAccess
      case _: PyApplyDynamic    => TagPyApplyDynamic
      case _: PyNew             => TagPyNew
      case _: PyLoadModule      => TagPyLoadModule
      case _: PyIsInstanceOf    => TagPyIsInstanceOf
      case _: PyAsInstanceOf    => TagPyAsInstanceOf
      case _: PyNewArray        => TagPyNewArray
      case _: PyArrayValue      => TagPyArrayValue
      case _: PyArraySelect     => TagPyArraySelect
      case _: PyUnaryOp         => TagPyUnaryOp
      case _: PyBinaryOp        => TagPyBinaryOp
      case _: PyClosure         => TagPyClosure
      case _: PyClassOf         => TagPyClassOf
      case _: PyBooleanLit      => TagPyBooleanLit
      case _: PyCharLit         => TagPyCharLit
      case _: PyByteLit         => TagPyByteLit
      case _: PyShortLit        => TagPyShortLit
      case _: PyIntLit          => TagPyIntLit
      case _: PyLongLit         => TagPyLongLit
      case _: PyFloatLit        => TagPyFloatLit
      case _: PyDoubleLit       => TagPyDoubleLit
      case _: PyStringLit       => TagPyStringLit
      case _: PyNullLit         => TagPyNullLit
      case _: PyUnitLit         => TagPyUnitLit

    private def writeTreePayload(t: PyTree): Unit = t match
      // ----- Statements -----
      case n: PyVarDef =>
        writeString(n.name.name)
        writeOptionalString(n.originalName)
        writeType(n.vtpe)
        writeBool(n.mutable)
        writeTree(n.rhs)

      case n: PyAssign =>
        writeTree(n.lhs)
        writeTree(n.rhs)

      case n: PyReturn =>
        writeTree(n.value)

      case n: PyWhile =>
        writeTree(n.cond)
        writeTree(n.body)

      case _: PySkip => ()

      // ----- Control flow -----
      case n: PyIf =>
        writeType(n.tpe)
        writeTree(n.cond)
        writeTree(n.thenp)
        writeTree(n.elsep)

      case n: PyTryCatch =>
        writeType(n.tpe)
        writeTree(n.block)
        writeString(n.errVar.name)
        writeOptionalString(n.errVarOriginalName)
        writeTree(n.handler)

      case n: PyTryFinally =>
        writeTree(n.block)
        writeTree(n.finalizer)

      case n: PyMatch =>
        writeType(n.tpe)
        writeTree(n.selector)
        writeNat(n.cases.size)
        n.cases.foreach { case (lits, body) =>
          writeNat(lits.size)
          lits.foreach(l => writeTree(l: PyTree))
          writeTree(body)
        }
        writeTree(n.default)

      case n: PyBlock =>
        writeNat(n.stats.size)
        n.stats.foreach(writeTree)
        writeTree(n.expr)

      case n: PyLabeled =>
        writeType(n.tpe)
        writeString(n.label.name)
        writeTree(n.body)

      case n: PyLabelReturn =>
        writeString(n.label.name)
        writeTree(n.value)

      // ----- References -----
      case n: PyVarRef =>
        writeType(n.tpe)
        writeString(n.name.name)

      case n: PyThis =>
        writeType(n.tpe)

      case n: PySelect =>
        writeType(n.tpe)
        writeTree(n.qualifier)
        writeFieldName(n.field)

      case n: PySelectStatic =>
        writeType(n.tpe)
        writeFieldName(n.field)

      // ----- Calls -----
      case n: PyApply =>
        writeType(n.tpe)
        writeNat(n.flags.bits)
        writeTree(n.receiver)
        writeClassNameRef(n.className)
        writeMethodNameRef(n.method)
        writeNat(n.args.size)
        n.args.foreach(writeTree)

      case n: PyApplyStatically =>
        writeType(n.tpe)
        writeNat(n.flags.bits)
        writeTree(n.receiver)
        writeClassNameRef(n.className)
        writeMethodNameRef(n.method)
        writeNat(n.args.size)
        n.args.foreach(writeTree)

      case n: PyApplyStatic =>
        writeType(n.tpe)
        writeNat(n.flags.bits)
        writeClassNameRef(n.className)
        writeMethodNameRef(n.method)
        writeNat(n.args.size)
        n.args.foreach(writeTree)

      case n: PyApplyExternal =>
        writeType(n.tpe)
        writeString(n.callee.name)
        writeNat(n.args.size)
        n.args.foreach(writeTree)

      case n: PyExternalRef =>
        writeType(n.tpe)
        writeString(n.module)
        writeNat(n.path.size)
        n.path.foreach(writeString)

      case n: PyAttrAccess =>
        writeType(n.tpe)
        writeTree(n.obj)
        writeString(n.name)

      case n: PyApplyDynamic =>
        writeType(n.tpe)
        writeTree(n.callee)
        writeNat(n.args.size)
        n.args.foreach(writeTree)
        writeNat(n.kwargs.size)
        n.kwargs.foreach { case (k, v) =>
          writeString(k)
          writeTree(v)
        }

      // ----- Construction -----
      case n: PyNew =>
        writeClassNameRef(n.className)
        writeMethodNameRef(n.ctor)
        writeNat(n.args.size)
        n.args.foreach(writeTree)

      case n: PyLoadModule =>
        writeClassNameRef(n.className)

      // ----- Type tests / casts -----
      case n: PyIsInstanceOf =>
        writeTree(n.expr)
        writeTypeRefRef(n.testType)

      case n: PyAsInstanceOf =>
        writeType(n.tpe)
        writeTree(n.expr)

      // ----- Arrays -----
      case n: PyNewArray =>
        writeTypeRefRef(n.elemTypeRef)
        writeTree(n.length)

      case n: PyArrayValue =>
        writeTypeRefRef(n.elemTypeRef)
        writeNat(n.elems.size)
        n.elems.foreach(writeTree)

      case n: PyArraySelect =>
        writeType(n.tpe)
        writeTree(n.array)
        writeTree(n.index)

      // ----- Operators -----
      case n: PyUnaryOp =>
        writeByteRaw(unaryTag(n.op).toInt & 0xff)
        writeTree(n.lhs)

      case n: PyBinaryOp =>
        writeByteRaw(binaryTag(n.op).toInt & 0xff)
        writeTree(n.lhs)
        writeTree(n.rhs)

      // ----- Closures / misc -----
      case n: PyClosure =>
        writeNat(n.params.size)
        n.params.foreach(writeParamDef)
        writeType(n.resultType)
        writeTree(n.body)

      case n: PyClassOf =>
        writeTypeRefRef(n.typeRef)

      // ----- Literals -----
      case n: PyBooleanLit => writeBool(n.value)
      case n: PyCharLit    => writeNat(n.value.toInt)
      case n: PyByteLit    => writeInt(n.value.toInt)
      case n: PyShortLit   => writeInt(n.value.toInt)
      case n: PyIntLit     => writeInt(n.value)
      case n: PyLongLit    => writeLong(n.value)
      case n: PyFloatLit   =>
        // 4 raw BE bytes
        val bits = java.lang.Float.floatToRawIntBits(n.value)
        b.writeByte((bits >>> 24) & 0xff)
        b.writeByte((bits >>> 16) & 0xff)
        b.writeByte((bits >>> 8) & 0xff)
        b.writeByte(bits & 0xff)
      case n: PyDoubleLit  =>
        val bits = java.lang.Double.doubleToRawLongBits(n.value)
        b.writeUncompressedLong(bits)
      case n: PyStringLit  => writeString(n.value)
      case _: PyNullLit    => ()
      case _: PyUnitLit    => ()
