package dotty.tools.backend.python.ir.pyir.serialization

import dotty.tools.backend.python.PyIREmitter
import dotty.tools.backend.python.ir.pyir.*

import org.junit.Assert.*
import org.junit.Test

class PyIRSerializationTests:

  private val NoPos = PyPosition.NoPosition
  private val SamplePos = PyPosition("Foo.scala", 12, 4)

  // ---------------------------------------------------------------
  //  Round-trip helpers
  // ---------------------------------------------------------------

  private def roundTrip(
      classes:   List[PyClassDef],
      mainEntry: Option[PyIREmitter.MainEntry] = None
  ): PyIRDeserializer.CompilationUnit =
    val bytes = PyIRSerializer.serializeToBytes(classes, mainEntry)
    PyIRDeserializer.deserialize(bytes)

  private def roundTripTree(t: PyTree): PyTree =
    val cls = wrapInClass("Wrapper", List(methodOf(t)))
    val unit = roundTrip(List(cls))
    unit.classes.head.methods.head.body.get

  // Wraps a tree as a method body in a class. Note that no linking
  // happens, so it is OK to reference unresolved names.
  private def methodOf(body: PyTree): PyMethodDef =
    PyMethodDef(
      flags        = PyMemberFlags.empty.withNamespace(PyMemberNamespace.PublicStatic),
      name         = PyMethodName(PySimpleMethodName("test"), Nil, PyPrimRef.VoidRef),
      originalName = PyOriginalName.NoOriginalName,
      args         = Nil,
      resultType   = PyVoidType,
      body         = Some(body),
      pos          = NoPos
    )

  private def wrapInClass(
      name:    String,
      methods: List[PyMethodDef],
      fields:  List[PyFieldDef] = Nil
  ): PyClassDef =
    PyClassDef(
      name         = PyClassName(name),
      originalName = PyOriginalName.NoOriginalName,
      kind         = PyClassKind.Class,
      superClass   = None,
      interfaces   = Nil,
      fields       = fields,
      methods      = methods,
      pos          = NoPos
    )

  // ---------------------------------------------------------------
  //  Header / corruption
  // ---------------------------------------------------------------

  @Test def emptyCompilationUnitRoundTrips(): Unit =
    val cu = roundTrip(Nil)
    assertEquals(Nil, cu.classes)
    assertEquals(None, cu.mainEntry)

  @Test def mainEntryRoundTrips(): Unit =
    val mainCls = wrapInClass("Main", Nil)
    val cu = roundTrip(
      List(mainCls),
      mainEntry = Some((mainCls.name, PyClassKind.ModuleClass))
    )
    assertEquals(Some((PyClassName("Main"), PyClassKind.ModuleClass)), cu.mainEntry)

  @Test def detectsBadMagic(): Unit =
    val bad = new Array[Byte](32)
    bad(0) = 'X'.toByte
    try
      PyIRDeserializer.deserialize(bad)
      fail("expected exception")
    catch
      case _: PyIRException => ()

  @Test def detectsCorruptedTrailerHash(): Unit =
    val bytes = PyIRSerializer.serializeToBytes(Nil, None)
    bytes(bytes.length - 1) = (bytes(bytes.length - 1) ^ 0xff).toByte
    try
      PyIRDeserializer.deserialize(bytes)
      fail("expected exception")
    catch
      case _: CorruptIRException => ()

  // ---------------------------------------------------------------
  //  Hardening: malformed input must surface as PyIRException, not
  //  raw JVM exceptions like ArrayIndexOutOfBoundsException.
  // ---------------------------------------------------------------

  /** Helper: wrap any deserialization attempt and assert that it raises
   *  a `PyIRException` (or subclass) — never a raw JVM exception. */
  private def assertPyIRException(label: String, bytes: Array[Byte]): Unit =
    try
      PyIRDeserializer.deserialize(bytes)
      fail(s"$label: expected PyIRException, but deserialization succeeded")
    catch
      case _: PyIRException => () // expected
      case e: Throwable =>
        fail(s"$label: expected PyIRException, got ${e.getClass.getName}: ${e.getMessage}")

  @Test def detectsTruncatedHeader(): Unit =
    // Less than 8 bytes — too small even to hold a trailer hash.
    assertPyIRException("zero-byte file",   new Array[Byte](0))
    assertPyIRException("seven-byte file",  new Array[Byte](7))

  @Test def detectsTruncatedAtVariousOffsets(): Unit =
    // Build a non-trivial valid bundle, then truncate at every byte
    // offset and confirm we always get a PyIRException.
    val cn = PyClassName("foo.Bar")
    val cls = PyClassDef(
      name = cn,
      originalName = PyOriginalName.fromString("Bar"),
      kind = PyClassKind.Class,
      superClass = Some(PyClassName.ObjectClass),
      interfaces = Nil,
      fields = List(PyFieldDef(
        PyMemberFlags.empty, PyFieldName(cn, PySimpleFieldName("x")),
        PyOriginalName.NoOriginalName, PyIntType, NoPos)),
      methods = List(PyMethodDef(
        PyMemberFlags.empty.withNamespace(PyMemberNamespace.Public),
        PyMethodName(PySimpleMethodName("m"), Nil, PyPrimRef.VoidRef),
        PyOriginalName.NoOriginalName, Nil, PyVoidType,
        Some(PyIntLit(1)(NoPos)), NoPos)),
      pos = NoPos
    )
    val full = PyIRSerializer.serializeToBytes(List(cls), None)

    // Truncate at every length from 0..full.length-1. We don't care
    // whether each one fails for "small file" or "bad hash" or "pool
    // index" reasons — only that none of them leak raw JVM exceptions.
    var i = 0
    while i < full.length do
      val truncated = java.util.Arrays.copyOf(full, i)
      assertPyIRException(s"truncated at $i", truncated)
      i += 1

  @Test def detectsTruncatedBetweenHeaderAndBody(): Unit =
    // 16 bytes is enough to satisfy the 8-byte trailer-hash precondition
    // and even the 8-byte "below trailer" body, but it cannot be a
    // valid header. The trailer-hash check fails first cleanly.
    val bytes = new Array[Byte](16)
    assertPyIRException("16 zero bytes", bytes)

  @Test def detectsBadMagicShortFile(): Unit =
    // Ensure the catch-all handles a bad magic at the smallest legal
    // size (8 bytes — exactly the trailer-hash window). The hash check
    // hits first because all bytes are zero and pjwHash64(0..0) != 0.
    val bytes = new Array[Byte](8)
    assertPyIRException("8 zero bytes", bytes)

  @Test def detectsBadTypeRefTag(): Unit =
    // Build a valid bundle, locate a known type-ref tag byte (0x01..0x03)
    // in the type-ref pool section, and corrupt it to 0xFE. The trailer
    // hash will then mismatch, but the test of interest is that even
    // when we *recompute* the trailer hash, the deserializer rejects
    // the unknown tag with a CorruptIRException rather than something
    // like a MatchError.
    val cls = wrapInClass("X", List(methodOf(PyIntLit(1)(NoPos))))
    val bytes = PyIRSerializer.serializeToBytes(List(cls), None)
    // Find any TagPyPrimRef (0x01) that is preceded by a Nat-encoded
    // pool size. Just scan for the first 0x01 after the magic+version
    // header (offset 18 = 4 magic + 2 version + 8 hash + 2 flags + 2 nat
    // for string-pool size -> approximate; use a wider start).
    var i = 18
    while i < bytes.length - 8 && bytes(i) != 0x01.toByte do i += 1
    if i >= bytes.length - 8 then
      fail("could not locate a TagPyPrimRef byte to corrupt")
    bytes(i) = 0xFE.toByte
    // Recompute trailer hash so the corrupt-tag failure is the *first*
    // failure surfaced (rather than the trailer-hash check).
    val recomputed = dotty.tools.tasty.TastyHash.pjwHash64(bytes, bytes.length - 8)
    var j = 0
    while j < 8 do
      bytes(bytes.length - 8 + j) = ((recomputed >>> (56 - 8 * j)) & 0xff).toByte
      j += 1
    assertPyIRException("bad pool/tree tag byte", bytes)

  @Test def detectsOutOfRangePoolIndex(): Unit =
    // Build a valid bundle, then walk back from the trailer hash and
    // overwrite a Nat byte with a clearly-out-of-range value. The Nat
    // encoding sets bit 0x80 on continuations, so 0x7F is "127, last
    // byte" — an index that is guaranteed to exceed any pool size in a
    // tiny test bundle.
    val cls = wrapInClass("Y", List(methodOf(PyStringLit("hi")(NoPos))))
    val bytes = PyIRSerializer.serializeToBytes(List(cls), None)
    // Replace the byte just before the trailer hash. That position is
    // somewhere in the body / class-list area and is highly likely to
    // be interpreted as a Nat (a pool index, count, etc.).
    val target = bytes.length - 9
    bytes(target) = 0x7F.toByte
    // Recompute hash so we get past the trailer check.
    val recomputed = dotty.tools.tasty.TastyHash.pjwHash64(bytes, bytes.length - 8)
    var j = 0
    while j < 8 do
      bytes(bytes.length - 8 + j) = ((recomputed >>> (56 - 8 * j)) & 0xff).toByte
      j += 1
    assertPyIRException("out-of-range pool index", bytes)

  @Test def detectsBadTreeTag(): Unit =
    // Body ends with the literal tree: tag 0x74 (PyIntLit) + 1-byte
    // position-nat (0 -> NoPosition) + 1-byte value (writeInt of 7
    // encodes as 0x07). Scan backwards from the trailer hash to find
    // the last 0x74 byte and overwrite it with an unmapped tag.
    val cls = wrapInClass("Z", List(methodOf(PyIntLit(7)(NoPos))))
    val bytes = PyIRSerializer.serializeToBytes(List(cls), None)
    var i = bytes.length - 9
    while i > 0 && bytes(i) != 0x74.toByte do i -= 1
    if i <= 0 then
      fail("could not locate a TagPyIntLit byte to corrupt")
    bytes(i) = 0xEE.toByte
    val recomputed = dotty.tools.tasty.TastyHash.pjwHash64(bytes, bytes.length - 8)
    var j = 0
    while j < 8 do
      bytes(bytes.length - 8 + j) = ((recomputed >>> (56 - 8 * j)) & 0xff).toByte
      j += 1
    assertPyIRException("bad tree tag byte", bytes)

  // ---------------------------------------------------------------
  //  Primitive types and refs
  // ---------------------------------------------------------------

  @Test def allPyTypesRoundTrip(): Unit =
    val allTypes: List[PyType] = List(
      PyAnyType, PyVoidType, PyNothingType, PyNullType,
      PyBooleanType, PyCharType, PyByteType, PyShortType, PyIntType,
      PyLongType, PyFloatType, PyDoubleType, PyStringType, PyArrayType,
      PyClassType(PyClassName("foo.Bar"))
    )
    // Carry every type through PyVarRef.tpe and PyAsInstanceOf.tpe.
    for t <- allTypes do
      val tree = PyAsInstanceOf(PyNullLit()(NoPos), t)(NoPos)
      val rt   = roundTripTree(tree).asInstanceOf[PyAsInstanceOf]
      assertEquals(s"asInstanceOf tpe $t", t, rt.tpe)

  @Test def allTypeRefsRoundTrip(): Unit =
    val refs: List[PyTypeRef] = List(
      PyPrimRef.VoidRef, PyPrimRef.BooleanRef, PyPrimRef.CharRef,
      PyPrimRef.ByteRef, PyPrimRef.ShortRef, PyPrimRef.IntRef,
      PyPrimRef.LongRef, PyPrimRef.FloatRef, PyPrimRef.DoubleRef,
      PyPrimRef.NullRef, PyPrimRef.NothingRef,
      PyClassRef(PyClassName("a.B")),
      PyArrayRef(PyClassRef(PyClassName("a.B")), 2),
      PyArrayRef(PyPrimRef.IntRef, 1)
    )
    for r <- refs do
      val tree = PyClassOf(r)(NoPos)
      val rt   = roundTripTree(tree)
      assertEquals(tree, rt)

  // ---------------------------------------------------------------
  //  Literals
  // ---------------------------------------------------------------

  @Test def literalsRoundTrip(): Unit =
    val lits: List[PyTree] = List(
      PyBooleanLit(true)(NoPos),
      PyBooleanLit(false)(NoPos),
      PyCharLit('A')(NoPos),
      PyCharLit('\u00ff')(NoPos),
      PyByteLit(-7)(NoPos),
      PyShortLit(12345)(NoPos),
      PyIntLit(0)(NoPos),
      PyIntLit(-1)(NoPos),
      PyIntLit(Int.MaxValue)(NoPos),
      PyLongLit(0L)(NoPos),
      PyLongLit(Long.MinValue)(NoPos),
      PyLongLit(Long.MaxValue)(NoPos),
      PyFloatLit(3.14f)(NoPos),
      PyFloatLit(0.0f)(NoPos),
      PyFloatLit(Float.PositiveInfinity)(NoPos),
      PyDoubleLit(2.71828)(NoPos),
      PyDoubleLit(Double.NegativeInfinity)(NoPos),
      PyStringLit("")(NoPos),
      PyStringLit("hello, λ world")(NoPos),
      PyNullLit()(NoPos),
      PyUnitLit()(NoPos)
    )
    for l <- lits do
      assertEquals(l, roundTripTree(l))

  @Test def floatNanRoundTripsViaBits(): Unit =
    val nan = Float.NaN
    val rt  = roundTripTree(PyFloatLit(nan)(NoPos)).asInstanceOf[PyFloatLit]
    assertTrue(java.lang.Float.isNaN(rt.value))

  // ---------------------------------------------------------------
  //  Statements / control flow
  // ---------------------------------------------------------------

  @Test def statementsRoundTrip(): Unit =
    val name = PyLocalName("x")
    val stmts: List[PyTree] = List(
      PyVarDef(name, PyOriginalName.fromString("x"), PyIntType, false, PyIntLit(1)(NoPos))(NoPos),
      PyAssign(PyVarRef(name)(PyIntType, NoPos), PyIntLit(2)(NoPos))(NoPos),
      PyReturn(PyIntLit(3)(NoPos))(NoPos),
      PyWhile(PyBooleanLit(true)(NoPos), PySkip()(NoPos))(NoPos),
      PySkip()(NoPos)
    )
    for s <- stmts do
      assertEquals(s, roundTripTree(s))

  @Test def controlFlowRoundTrips(): Unit =
    val ifTree = PyIf(
      PyBooleanLit(true)(NoPos),
      PyIntLit(1)(NoPos),
      PyIntLit(2)(NoPos)
    )(PyIntType, NoPos)
    val tryCatch = PyTryCatch(
      PyIntLit(1)(NoPos),
      PyLocalName("e"),
      PyOriginalName.fromString("e"),
      PyIntLit(2)(NoPos)
    )(PyIntType, NoPos)
    val tryFinally = PyTryFinally(
      PyIntLit(1)(NoPos),
      PySkip()(NoPos)
    )(NoPos)
    val matchTree = PyMatch(
      PyIntLit(0)(NoPos),
      List(
        (List(PyIntLit(1)(NoPos), PyIntLit(2)(NoPos)), PyStringLit("a")(NoPos)),
        (List(PyIntLit(3)(NoPos)), PyStringLit("b")(NoPos))
      ),
      PyStringLit("default")(NoPos)
    )(PyStringType, NoPos)
    val block = PyBlock(
      List(PyIntLit(1)(NoPos), PyIntLit(2)(NoPos)),
      PyIntLit(3)(NoPos)
    )(NoPos)
    val labeled = PyLabeled(
      PyLabelName("L"),
      PyBlock(Nil, PyLabelReturn(PyLabelName("L"), PyIntLit(7)(NoPos))(NoPos))(NoPos)
    )(PyIntType, NoPos)

    for t <- List(ifTree, tryCatch, tryFinally, matchTree, block, labeled) do
      assertEquals(t, roundTripTree(t))

  // ---------------------------------------------------------------
  //  References
  // ---------------------------------------------------------------

  @Test def referencesRoundTrip(): Unit =
    val varRef    = PyVarRef(PyLocalName("y"))(PyIntType, NoPos)
    val thisRef   = PyThis()(PyClassType(PyClassName("a.B")), NoPos)
    val sel       = PySelect(thisRef, PyFieldName(PyClassName("a.B"), PySimpleFieldName("f")))(PyIntType, NoPos)
    val selStatic = PySelectStatic(PyFieldName(PyClassName("a.B"), PySimpleFieldName("g")))(PyStringType, NoPos)

    for t <- List(varRef, thisRef, sel, selStatic) do
      assertEquals(t, roundTripTree(t))

    // The tpe is in the secondary param list and not part of equals,
    // so verify it explicitly.
    val rtVar = roundTripTree(varRef).asInstanceOf[PyVarRef]
    assertEquals(PyIntType, rtVar.tpe)

  // ---------------------------------------------------------------
  //  Calls
  // ---------------------------------------------------------------

  @Test def callsRoundTrip(): Unit =
    val recv  = PyVarRef(PyLocalName("r"))(PyAnyType, NoPos)
    val cls   = PyClassName("a.B")
    val mname = PyMethodName(PySimpleMethodName("foo"), List(PyPrimRef.IntRef), PyPrimRef.IntRef)
    val flags = PyApplyFlags.empty.withPrivate(true)

    val apply = PyApply(flags, recv, cls, mname, List(PyIntLit(1)(NoPos)))(PyIntType, NoPos)
    val applyStatically = PyApplyStatically(flags, recv, cls, mname, Nil)(PyVoidType, NoPos)
    val applyStatic = PyApplyStatic(flags, cls, mname, Nil)(PyVoidType, NoPos)
    val applyExternal = PyApplyExternal(PyExternalName("print"), List(PyStringLit("hi")(NoPos)))(PyVoidType, NoPos)
    val externalRef = PyExternalRef("numpy", List("ndarray", "shape"))(PyAnyType, NoPos)
    val attrAccess = PyAttrAccess(externalRef, "size")(PyIntType, NoPos)
    val applyDynamic = PyApplyDynamic(
      externalRef,
      List(PyIntLit(1)(NoPos), PyIntLit(2)(NoPos)),
      List(("axis", PyIntLit(0)(NoPos)), ("keepdims", PyBooleanLit(true)(NoPos)))
    )(PyAnyType, NoPos)

    for t <- List(apply, applyStatically, applyStatic, applyExternal,
                  externalRef, attrAccess, applyDynamic) do
      assertEquals(t, roundTripTree(t))

    // PyApplyFlags is a value class - it should round-trip its bits via
    // the primary constructor field.
    val rtApply = roundTripTree(apply).asInstanceOf[PyApply]
    assertEquals(flags.bits, rtApply.flags.bits)
    assertEquals(PyIntType, rtApply.tpe)

  // ---------------------------------------------------------------
  //  Construction / type tests / arrays
  // ---------------------------------------------------------------

  @Test def constructionAndArraysRoundTrip(): Unit =
    val cls   = PyClassName("a.B")
    val ctor  = PyMethodName(PySimpleMethodName("<init>"), Nil, PyPrimRef.VoidRef)
    val nu    = PyNew(cls, ctor, Nil)(NoPos)
    val ldMod = PyLoadModule(cls)(NoPos)
    val isI   = PyIsInstanceOf(PyNullLit()(NoPos), PyClassRef(cls))(NoPos)
    val asI   = PyAsInstanceOf(PyNullLit()(NoPos), PyClassType(cls))(NoPos)
    val newA  = PyNewArray(PyPrimRef.IntRef, PyIntLit(10)(NoPos))(NoPos)
    val arrV  = PyArrayValue(PyPrimRef.IntRef, List(PyIntLit(1)(NoPos), PyIntLit(2)(NoPos)))(NoPos)
    val arrSel = PyArraySelect(arrV, PyIntLit(0)(NoPos))(PyIntType, NoPos)

    for t <- List(nu, ldMod, isI, asI, newA, arrV, arrSel) do
      assertEquals(t, roundTripTree(t))

  @Test def tupleValueRoundTrip(): Unit =
    val tupleType = PyClassType(PyClassName("scala.Tuple3"))
    val empty   = PyTupleValue(Nil)(PyClassType(PyClassName("scala.Tuple")), NoPos)
    val single  = PyTupleValue(List(PyIntLit(7)(NoPos)))(
      PyClassType(PyClassName("scala.Tuple1")), NoPos)
    val mixed   = PyTupleValue(
      List(PyIntLit(1)(NoPos), PyStringLit("x")(NoPos), PyBooleanLit(true)(NoPos))
    )(tupleType, NoPos)
    val nested  = PyTupleValue(List(mixed, empty))(tupleType, NoPos)

    for t <- List[PyTree](empty, single, mixed, nested) do
      assertEquals(t, roundTripTree(t))

  // ---------------------------------------------------------------
  //  Operators
  // ---------------------------------------------------------------

  @Test def everyUnaryOpRoundTrips(): Unit =
    for op <- PyUnaryCode.values do
      val tree = PyUnaryOp(op, PyIntLit(1)(NoPos))(NoPos)
      assertEquals(s"unary op $op", tree, roundTripTree(tree))

  @Test def everyBinaryOpRoundTrips(): Unit =
    for op <- PyBinaryCode.values do
      val tree = PyBinaryOp(op, PyIntLit(1)(NoPos), PyIntLit(2)(NoPos))(NoPos)
      assertEquals(s"binary op $op", tree, roundTripTree(tree))

  // ---------------------------------------------------------------
  //  Closures and ClassOf
  // ---------------------------------------------------------------

  @Test def closuresAndClassOfRoundTrip(): Unit =
    val param = PyParamDef(
      PyLocalName("p"), PyOriginalName.NoOriginalName, PyAnyType, false, NoPos
    )
    val closure = PyClosure(
      params     = List(param),
      resultType = PyAnyType,
      body       = PyVarRef(PyLocalName("p"))(PyAnyType, NoPos)
    )(NoPos)
    val classOf = PyClassOf(PyClassRef(PyClassName("foo.Bar")))(NoPos)

    for t <- List[PyTree](closure, classOf) do
      assertEquals(t, roundTripTree(t))

  // ---------------------------------------------------------------
  //  Class definitions: fields, methods, namespaces, originalNames
  // ---------------------------------------------------------------

  @Test def classDefWithFieldsAndMethodsRoundTrips(): Unit =
    val cn = PyClassName("foo.Bar")
    val field = PyFieldDef(
      flags = PyMemberFlags.empty.withMutable(true),
      name  = PyFieldName(cn, PySimpleFieldName("x")),
      originalName = PyOriginalName.fromString("x"),
      ftpe  = PyIntType,
      pos   = SamplePos
    )
    val ctor = PyMethodDef(
      flags = PyMemberFlags.empty.withNamespace(PyMemberNamespace.Constructor),
      name  = PyMethodName(PySimpleMethodName("<init>"), Nil, PyPrimRef.VoidRef),
      originalName = PyOriginalName.NoOriginalName,
      args = Nil,
      resultType = PyVoidType,
      body = Some(PySkip()(NoPos)),
      pos = NoPos
    )
    val abstractMethod = PyMethodDef(
      flags = PyMemberFlags.empty.withNamespace(PyMemberNamespace.Public),
      name  = PyMethodName(PySimpleMethodName("absM"), Nil, PyPrimRef.VoidRef),
      originalName = PyOriginalName.fromString("absM"),
      args = Nil,
      resultType = PyVoidType,
      body = None,
      pos = NoPos
    )
    val cls = PyClassDef(
      name = cn,
      originalName = PyOriginalName.fromString("Bar"),
      kind = PyClassKind.Class,
      superClass = Some(PyClassName.ObjectClass),
      interfaces = List(PyClassName("a.I"), PyClassName("a.J")),
      fields = List(field),
      methods = List(ctor, abstractMethod),
      pos = SamplePos
    )

    val cu = roundTrip(List(cls))
    assertEquals(1, cu.classes.size)
    assertEquals(cls, cu.classes.head)

    // Verify position pool round-trips a non-trivial position.
    assertEquals(SamplePos, cu.classes.head.pos)
    assertEquals(SamplePos, cu.classes.head.fields.head.pos)

  @Test def everyNamespaceRoundTrips(): Unit =
    for ns <- PyMemberNamespace.values do
      val cls = wrapInClass(s"WithNs_$ns", List(
        PyMethodDef(
          flags = PyMemberFlags.empty.withNamespace(ns),
          name  = PyMethodName(PySimpleMethodName("m"), Nil, PyPrimRef.VoidRef),
          originalName = PyOriginalName.NoOriginalName,
          args = Nil,
          resultType = PyVoidType,
          body = Some(PySkip()(NoPos)),
          pos = NoPos
        )
      ))
      val cu = roundTrip(List(cls))
      assertEquals(s"namespace $ns",
        ns, cu.classes.head.methods.head.flags.namespace)

  // ---------------------------------------------------------------
  //  Integration: ser → deser → emit produces identical Python text
  // ---------------------------------------------------------------

  @Test def serDeserEmitProducesIdenticalPython(): Unit =
    val cn = PyClassName("foo.Bar")
    val field = PyFieldDef(
      flags = PyMemberFlags.empty,
      name  = PyFieldName(cn, PySimpleFieldName("x")),
      originalName = PyOriginalName.fromString("x"),
      ftpe  = PyIntType,
      pos   = NoPos
    )
    val ctor = PyMethodDef(
      flags = PyMemberFlags.empty.withNamespace(PyMemberNamespace.Constructor),
      name  = PyMethodName(PySimpleMethodName("<init>"), Nil, PyPrimRef.VoidRef),
      originalName = PyOriginalName.NoOriginalName,
      args  = Nil,
      resultType = PyVoidType,
      body  = Some(PyAssign(
        PySelect(
          PyThis()(PyClassType(cn), NoPos),
          PyFieldName(cn, PySimpleFieldName("x"))
        )(PyIntType, NoPos),
        PyIntLit(42)(NoPos)
      )(NoPos)),
      pos = NoPos
    )
    val m = PyMethodDef(
      flags = PyMemberFlags.empty.withNamespace(PyMemberNamespace.Public),
      name  = PyMethodName(PySimpleMethodName("plus"), List(PyPrimRef.IntRef), PyPrimRef.IntRef),
      originalName = PyOriginalName.NoOriginalName,
      args = List(PyParamDef(PyLocalName("y"), PyOriginalName.NoOriginalName, PyIntType, false, NoPos)),
      resultType = PyIntType,
      body = Some(PyBinaryOp(
        PyBinaryCode.IntAdd,
        PySelect(PyThis()(PyClassType(cn), NoPos), PyFieldName(cn, PySimpleFieldName("x")))(PyIntType, NoPos),
        PyVarRef(PyLocalName("y"))(PyIntType, NoPos)
      )(NoPos)),
      pos = NoPos
    )
    val cls = PyClassDef(cn, PyOriginalName.fromString("Bar"), PyClassKind.Class,
      Some(PyClassName.ObjectClass), Nil, List(field), List(ctor, m), NoPos)

    val classes = List(cls)
    val pyA = PyIREmitter.emitToString(classes, None)

    val bytes = PyIRSerializer.serializeToBytes(classes, None)
    val cu    = PyIRDeserializer.deserialize(bytes)
    val pyB   = PyIREmitter.emitToString(cu.classes, cu.mainEntry)

    assertEquals(pyA, pyB)

  @Test def everyClassKindRoundTrips(): Unit =
    for kind <- PyClassKind.values do
      val cls = PyClassDef(
        name = PyClassName(s"K$kind"),
        originalName = PyOriginalName.NoOriginalName,
        kind = kind,
        superClass = None,
        interfaces = Nil,
        fields = Nil,
        methods = Nil,
        pos = NoPos
      )
      val cu = roundTrip(List(cls))
      assertEquals(s"kind $kind", kind, cu.classes.head.kind)
