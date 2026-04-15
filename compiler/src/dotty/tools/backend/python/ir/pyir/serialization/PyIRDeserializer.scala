package dotty.tools.backend.python.ir.pyir.serialization

import dotty.tools.backend.python.PyIREmitter
import dotty.tools.backend.python.ir.pyir.*
import dotty.tools.tasty.{TastyHash, TastyReader}

import java.nio.ByteBuffer
import scala.collection.mutable
import scala.compiletime.uninitialized

/** Binary deserializer for a single PyIR compilation unit. */
object PyIRDeserializer:

  /** A deserialized PyIR compilation unit, ready to feed into
   *  `PyLinker.Input`. */
  final case class CompilationUnit(
      classes:   List[PyClassDef],
      mainEntry: Option[PyIREmitter.MainEntry]
  )

  def deserialize(bytes: Array[Byte]): CompilationUnit =
    val d = new Deserializer(bytes)
    d.run()

  def deserialize(buf: ByteBuffer): CompilationUnit =
    if buf.hasArray && buf.arrayOffset() == 0 && buf.position() == 0
       && buf.limit() == buf.array().length
    then
      deserialize(buf.array())
    else
      val arr = new Array[Byte](buf.remaining())
      buf.duplicate().get(arr)
      deserialize(arr)

  // =================================================================
  //  Stateful deserializer instance
  // =================================================================

  private final class Deserializer(bytes: Array[Byte]):
    import PyIRTags.*

    private val reader = new TastyReader(bytes)

    private var stringPool:     Array[String]       = uninitialized
    private var classNamePool:  Array[PyClassName]  = uninitialized
    private var typeRefPool:    Array[PyTypeRef]    = uninitialized
    private var methodNamePool: Array[PyMethodName] = uninitialized
    private var positionPool:   Array[PyPosition]   = uninitialized

    // -------------------------------------------------------------
    //  Top-level entry
    // -------------------------------------------------------------

    def run(): CompilationUnit =
      val totalLen = bytes.length
      if totalLen < 8 then
        throw new CorruptIRException(
          s"PyIR file too small ($totalLen bytes) to contain a trailer hash")

      // Verify trailer hash before we read any structured content.
      val expectedHash = readBE64At(totalLen - 8)
      val actualHash   = TastyHash.pjwHash64(bytes, totalLen - 8)
      if expectedHash != actualHash then
        throw new CorruptIRException(
          s"PyIR trailer hash mismatch: expected 0x${expectedHash.toHexString}, got 0x${actualHash.toHexString}")

      readHeader()
      readStringPool()
      readClassNamePool()
      readTypeRefPool()
      readMethodNamePool()
      readPositionPool()

      val mainEntry =
        if reader.readByte() == 0 then None
        else
          val cn   = readClassNameRef()
          val kind = classKindFromTag(reader.readByte().toByte)
          Some((cn, kind))

      val classCount = reader.readNat()
      val classes    = List.fill(classCount)(readClassDef())

      CompilationUnit(classes, mainEntry)

    private def readBE64At(idx: Int): Long =
      var x = 0L
      var i = 0
      while i < 8 do
        x = (x << 8) | (bytes(idx + i) & 0xffL)
        i += 1
      x

    // -------------------------------------------------------------
    //  Header
    // -------------------------------------------------------------

    private def readHeader(): Unit =
      val magic = readBE32()
      if magic != PyIRFormat.Magic then
        throw new CorruptIRException(
          s"Bad PyIR magic: expected 0x${PyIRFormat.Magic.toHexString}, got 0x${magic.toHexString}")

      val version = readBE16()
      val major   = (version >>> 8) & 0xff
      val minor   = version & 0xff
      if major != PyIRFormat.MajorVersion then
        throw new IncompatibleIRVersionException(
          s"PyIR major version mismatch: file=$major.$minor, reader=${PyIRFormat.MajorVersion}.${PyIRFormat.MinorVersion}")
      if minor > PyIRFormat.MinorVersion then
        throw new IncompatibleIRVersionException(
          s"PyIR minor version $major.$minor is newer than this reader (${PyIRFormat.MajorVersion}.${PyIRFormat.MinorVersion})")

      // Compiler hash - currently not validated; downstream callers may
      // log a warning if they wish.
      reader.readUncompressedLong()
      readBE16() // Flags - reserved

    private def readBE32(): Int =
      val a = reader.readByte() & 0xff
      val b = reader.readByte() & 0xff
      val c = reader.readByte() & 0xff
      val d = reader.readByte() & 0xff
      (a << 24) | (b << 16) | (c << 8) | d

    private def readBE16(): Int =
      val a = reader.readByte() & 0xff
      val b = reader.readByte() & 0xff
      (a << 8) | b

    // -------------------------------------------------------------
    //  Pool readers
    // -------------------------------------------------------------

    private def readStringPool(): Unit =
      val n = reader.readNat()
      stringPool = new Array[String](n)
      var i = 0
      while i < n do
        stringPool(i) = reader.readUtf8()
        i += 1

    private def readClassNamePool(): Unit =
      val n = reader.readNat()
      classNamePool = new Array[PyClassName](n)
      var i = 0
      while i < n do
        val sIdx = reader.readNat()
        classNamePool(i) = PyClassName(stringPool(sIdx))
        i += 1

    private def readTypeRefPool(): Unit =
      val n = reader.readNat()
      typeRefPool = new Array[PyTypeRef](n)
      var i = 0
      while i < n do
        val tag = reader.readByte().toByte
        typeRefPool(i) = tag match
          case TagPyPrimRef =>
            val pt = primFromTag(reader.readByte().toByte)
            PyPrimRef(pt)
          case TagPyClassRef =>
            PyClassRef(classNamePool(reader.readNat()))
          case TagPyArrayRef =>
            val baseIdx = reader.readNat()
            val dims    = reader.readNat()
            PyArrayRef(typeRefPool(baseIdx), dims)
          case _ =>
            throw new CorruptIRException(
              s"Unknown PyTypeRef tag: 0x${(tag & 0xff).toHexString}")
        i += 1

    private def readMethodNamePool(): Unit =
      val n = reader.readNat()
      methodNamePool = new Array[PyMethodName](n)
      var i = 0
      while i < n do
        val simpleStr = stringPool(reader.readNat())
        val paramN    = reader.readNat()
        val params    = List.fill(paramN)(typeRefPool(reader.readNat()))
        val result    = typeRefPool(reader.readNat())
        methodNamePool(i) = PyMethodName(PySimpleMethodName(simpleStr), params, result)
        i += 1

    private def readPositionPool(): Unit =
      val n = reader.readNat()
      positionPool = new Array[PyPosition](n)
      positionPool(0) = PyPosition.NoPosition
      var i = 1
      while i < n do
        val src  = stringPool(reader.readNat())
        val line = reader.readNat()
        val col  = reader.readNat()
        positionPool(i) = PyPosition(src, line, col)
        i += 1

    // -------------------------------------------------------------
    //  Index-resolving helpers used by body readers
    // -------------------------------------------------------------

    private def readString(): String       = stringPool(reader.readNat())
    private def readClassNameRef(): PyClassName  = classNamePool(reader.readNat())
    private def readTypeRefRef(): PyTypeRef = typeRefPool(reader.readNat())
    private def readMethodNameRef(): PyMethodName = methodNamePool(reader.readNat())
    private def readPosition(): PyPosition = positionPool(reader.readNat())

    private def readOptionalString(): PyOriginalName =
      if reader.readByte() == 0 then PyOriginalName.NoOriginalName
      else PyOriginalName.fromString(readString())

    private def readType(): PyType =
      val tag = reader.readByte().toByte
      if tag == TagPyClassType then PyClassType(readClassNameRef())
      else pyTypeFromTag(tag)

    private def readFieldName(): PyFieldName =
      val owner  = readClassNameRef()
      val simple = readString()
      PyFieldName(owner, PySimpleFieldName(simple))

    private def readBool(): Boolean = reader.readByte() != 0

    // -------------------------------------------------------------
    //  Definition readers
    // -------------------------------------------------------------

    private def readClassDef(): PyClassDef =
      val pos          = readPosition()
      val name         = readClassNameRef()
      val originalName = readOptionalString()
      val kind         = classKindFromTag(reader.readByte().toByte)
      val superClass   =
        if reader.readByte() == 0 then None
        else Some(readClassNameRef())
      val ifaceN     = reader.readNat()
      val interfaces = List.fill(ifaceN)(readClassNameRef())
      val fieldN     = reader.readNat()
      val fields     = List.fill(fieldN)(readFieldDef())
      val methodN    = reader.readNat()
      val methods    = List.fill(methodN)(readMethodDef())
      PyClassDef(name, originalName, kind, superClass, interfaces, fields, methods, pos)

    private def readFieldDef(): PyFieldDef =
      val pos          = readPosition()
      val flags        = new PyMemberFlags(reader.readNat())
      val name         = readFieldName()
      val originalName = readOptionalString()
      val ftpe         = readType()
      PyFieldDef(flags, name, originalName, ftpe, pos)

    private def readMethodDef(): PyMethodDef =
      val pos          = readPosition()
      val flags        = new PyMemberFlags(reader.readNat())
      val name         = readMethodNameRef()
      val originalName = readOptionalString()
      val argN         = reader.readNat()
      val args         = List.fill(argN)(readParamDef())
      val resultType   = readType()
      val body         =
        if reader.readByte() == 0 then None
        else Some(readTree())
      PyMethodDef(flags, name, originalName, args, resultType, body, pos)

    private def readParamDef(): PyParamDef =
      val pos          = readPosition()
      val name         = PyLocalName(readString())
      val originalName = readOptionalString()
      val ptpe         = readType()
      val mutable      = readBool()
      PyParamDef(name, originalName, ptpe, mutable, pos)

    // -------------------------------------------------------------
    //  Tree reader
    // -------------------------------------------------------------

    private def readTree(): PyTree =
      val tag = reader.readByte().toByte
      val pos = readPosition()
      readTreeBody(tag, pos)

    private def readTreeBody(tag: Byte, pos: PyPosition): PyTree = tag match
      // ----- Statements -----
      case TagPyVarDef =>
        val name = PyLocalName(readString())
        val on   = readOptionalString()
        val vt   = readType()
        val mut  = readBool()
        val rhs  = readTree()
        PyVarDef(name, on, vt, mut, rhs)(pos)

      case TagPyAssign =>
        val lhsTree = readTree()
        val lhs = lhsTree match
          case a: PyAssignable => a
          case other =>
            throw new CorruptIRException(
              s"corrupt IR: expected PyAssignable on LHS, got ${other.getClass.getSimpleName}")
        val rhs = readTree()
        PyAssign(lhs, rhs)(pos)

      case TagPyReturn =>
        PyReturn(readTree())(pos)

      case TagPyWhile =>
        val cond = readTree()
        val body = readTree()
        PyWhile(cond, body)(pos)

      case TagPyForEach =>
        val v    = PyLocalName(readString())
        val it   = readTree()
        val body = readTree()
        PyForEach(v, it, body)(pos)

      case TagPySkip =>
        PySkip()(pos)

      // ----- Control flow -----
      case TagPyIf =>
        val tpe   = readType()
        val cond  = readTree()
        val thenp = readTree()
        val elsep = readTree()
        PyIf(cond, thenp, elsep)(tpe, pos)

      case TagPyTryCatch =>
        val tpe     = readType()
        val block   = readTree()
        val errVar  = PyLocalName(readString())
        val errOn   = readOptionalString()
        val handler = readTree()
        PyTryCatch(block, errVar, errOn, handler)(tpe, pos)

      case TagPyTryFinally =>
        val block     = readTree()
        val finalizer = readTree()
        PyTryFinally(block, finalizer)(pos)

      case TagPyMatch =>
        val tpe      = readType()
        val selector = readTree()
        val caseN    = reader.readNat()
        val cases = List.fill(caseN) {
          val litN = reader.readNat()
          val lits = List.fill(litN) {
            val t = readTree()
            t match
              case ml: PyMatchableLiteral => ml
              case other =>
                throw new CorruptIRException(
                  s"corrupt IR: expected PyMatchableLiteral in PyMatch case, got ${other.getClass.getSimpleName}")
          }
          val body = readTree()
          (lits, body)
        }
        val default = readTree()
        PyMatch(selector, cases, default)(tpe, pos)

      case TagPyBlock =>
        val statN = reader.readNat()
        val stats = List.fill(statN)(readTree())
        val expr  = readTree()
        PyBlock(stats, expr)(pos)

      case TagPyLabeled =>
        val tpe   = readType()
        val label = PyLabelName(readString())
        val body  = readTree()
        PyLabeled(label, body)(tpe, pos)

      case TagPyLabelReturn =>
        val label = PyLabelName(readString())
        val value = readTree()
        PyLabelReturn(label, value)(pos)

      // ----- References -----
      case TagPyVarRef =>
        val tpe  = readType()
        val name = PyLocalName(readString())
        PyVarRef(name)(tpe, pos)

      case TagPyThis =>
        val tpe = readType()
        PyThis()(tpe, pos)

      case TagPySelect =>
        val tpe   = readType()
        val qual  = readTree()
        val field = readFieldName()
        PySelect(qual, field)(tpe, pos)

      case TagPySelectStatic =>
        val tpe   = readType()
        val field = readFieldName()
        PySelectStatic(field)(tpe, pos)

      // ----- Calls -----
      case TagPyApply =>
        val tpe       = readType()
        val flags     = new PyApplyFlags(reader.readNat())
        val receiver  = readTree()
        val className = readClassNameRef()
        val method    = readMethodNameRef()
        val argN      = reader.readNat()
        val args      = List.fill(argN)(readTree())
        PyApply(flags, receiver, className, method, args)(tpe, pos)

      case TagPyApplyStatically =>
        val tpe       = readType()
        val flags     = new PyApplyFlags(reader.readNat())
        val receiver  = readTree()
        val className = readClassNameRef()
        val method    = readMethodNameRef()
        val argN      = reader.readNat()
        val args      = List.fill(argN)(readTree())
        PyApplyStatically(flags, receiver, className, method, args)(tpe, pos)

      case TagPyApplyStatic =>
        val tpe       = readType()
        val flags     = new PyApplyFlags(reader.readNat())
        val className = readClassNameRef()
        val method    = readMethodNameRef()
        val argN      = reader.readNat()
        val args      = List.fill(argN)(readTree())
        PyApplyStatic(flags, className, method, args)(tpe, pos)

      case TagPyApplyExternal =>
        val tpe    = readType()
        val callee = PyExternalName(readString())
        val argN   = reader.readNat()
        val args   = List.fill(argN)(readTree())
        PyApplyExternal(callee, args)(tpe, pos)

      case TagPyExternalRef =>
        val tpe    = readType()
        val module = readString()
        val pathN  = reader.readNat()
        val path   = List.fill(pathN)(readString())
        PyExternalRef(module, path)(tpe, pos)

      case TagPyAttrAccess =>
        val tpe  = readType()
        val obj  = readTree()
        val name = readString()
        PyAttrAccess(obj, name)(tpe, pos)

      case TagPyApplyDynamic =>
        val tpe    = readType()
        val callee = readTree()
        val argN   = reader.readNat()
        val args   = List.fill(argN)(readTree())
        val kwN    = reader.readNat()
        val kwargs = List.fill(kwN) {
          val k = readString()
          val v = readTree()
          (k, v)
        }
        PyApplyDynamic(callee, args, kwargs)(tpe, pos)

      // ----- Construction -----
      case TagPyNew =>
        val className = readClassNameRef()
        val ctor      = readMethodNameRef()
        val argN      = reader.readNat()
        val args      = List.fill(argN)(readTree())
        PyNew(className, ctor, args)(pos)

      case TagPyLoadModule =>
        PyLoadModule(readClassNameRef())(pos)

      // ----- Type tests / casts -----
      case TagPyIsInstanceOf =>
        val expr = readTree()
        val tt   = readTypeRefRef()
        PyIsInstanceOf(expr, tt)(pos)

      case TagPyAsInstanceOf =>
        val tpe  = readType()
        val expr = readTree()
        PyAsInstanceOf(expr, tpe)(pos)

      // ----- Arrays -----
      case TagPyNewArray =>
        val elem   = readTypeRefRef()
        val length = readTree()
        PyNewArray(elem, length)(pos)

      case TagPyArrayValue =>
        val elem  = readTypeRefRef()
        val elemN = reader.readNat()
        val elems = List.fill(elemN)(readTree())
        PyArrayValue(elem, elems)(pos)

      case TagPyArraySelect =>
        val tpe   = readType()
        val arr   = readTree()
        val index = readTree()
        PyArraySelect(arr, index)(tpe, pos)

      // ----- Operators -----
      case TagPyUnaryOp =>
        val op  = unaryFromTag(reader.readByte().toByte)
        val lhs = readTree()
        PyUnaryOp(op, lhs)(pos)

      case TagPyBinaryOp =>
        val op  = binaryFromTag(reader.readByte().toByte)
        val lhs = readTree()
        val rhs = readTree()
        PyBinaryOp(op, lhs, rhs)(pos)

      // ----- Closures / misc -----
      case TagPyClosure =>
        val capN     = reader.readNat()
        val captures = List.fill(capN)(readParamDef())
        val pN       = reader.readNat()
        val params   = List.fill(pN)(readParamDef())
        val resTpe   = readType()
        val body     = readTree()
        val cvN      = reader.readNat()
        val cvs      = List.fill(cvN)(readTree())
        PyClosure(captures, params, resTpe, body, cvs)(pos)

      case TagPyClassOf =>
        PyClassOf(readTypeRefRef())(pos)

      // ----- Literals -----
      case TagPyBooleanLit =>
        PyBooleanLit(readBool())(pos)
      case TagPyCharLit =>
        PyCharLit(reader.readNat().toChar)(pos)
      case TagPyByteLit =>
        PyByteLit(reader.readInt().toByte)(pos)
      case TagPyShortLit =>
        PyShortLit(reader.readInt().toShort)(pos)
      case TagPyIntLit =>
        PyIntLit(reader.readInt())(pos)
      case TagPyLongLit =>
        PyLongLit(reader.readLongInt())(pos)
      case TagPyFloatLit =>
        val a = reader.readByte() & 0xff
        val b = reader.readByte() & 0xff
        val c = reader.readByte() & 0xff
        val d = reader.readByte() & 0xff
        val bits = (a << 24) | (b << 16) | (c << 8) | d
        PyFloatLit(java.lang.Float.intBitsToFloat(bits))(pos)
      case TagPyDoubleLit =>
        val bits = reader.readUncompressedLong()
        PyDoubleLit(java.lang.Double.longBitsToDouble(bits))(pos)
      case TagPyStringLit =>
        PyStringLit(readString())(pos)
      case TagPyNullLit =>
        PyNullLit()(pos)
      case TagPyUnitLit =>
        PyUnitLit()(pos)

      case _ =>
        throw new CorruptIRException(
          s"Unknown PyTree tag: 0x${(tag & 0xff).toHexString}")
