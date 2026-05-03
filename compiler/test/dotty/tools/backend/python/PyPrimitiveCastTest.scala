package dotty.tools.backend.python

import dotty.tools.backend.python.ir.pyir.*

import org.junit.Assert.*
import org.junit.Test

/** Tests for the primitive numeric-cast / implicit-coercion helpers
 *  exposed by `scala.Int_` / `scala.Char_` (and the parallel
 *  `scala.{Byte,Short,Long,Float}_` companions).
 *
 *  Background — Wave 5 item 10. Generated Python for an implicit
 *  Int→Double widening lowers to a static call shaped like
 *  `_scpy_module_value(_scpy_mod_scala_Int__).int2double__I__D(i)`.
 *  That helper is provided by the compiled `library-py` PyIR for
 *  `scala.Int_` (the Scala source is `implicit def int2double(x: Int):
 *  Double = x.toDouble` on `object Int`). Earlier the linker silently
 *  dropped the compiled `scala.Int_` because `IntCompanionClass` was
 *  listed in `PyIRRuntime.providedClasses`, and the hand-written
 *  `_scpy_IntModule` runtime stub only carried `toChar__I__C` /
 *  `int2long__I__J`, so `int2double__I__D` raised AttributeError at
 *  runtime. Same shape for `Char_` (`char2int`, `char2long`,
 *  `char2float`, `char2double`).
 *
 *  These tests pin the structural invariant: the two companions are
 *  NOT runtime-provided, the runtime prelude does NOT carry shadow
 *  module-class stubs, and the linker accepts a Support `.pyir` that
 *  defines the static helper plus a User input that calls it. */
class PyPrimitiveCastTest:

  private val NoPos = PyPosition.NoPosition

  // -----------------------------------------------------------------
  //  Provided-class registry must NOT contain the primitive
  //  companions whose `.pyir` carries the implicit-coercion methods.
  // -----------------------------------------------------------------

  @Test def intCompanionIsNotRuntimeProvided(): Unit =
    assertEquals(
      "scala.Int_ must NOT be runtime-provided — its int2double / int2long / int2float "
        + "are supplied by compiled library-py PyIR. Listing it in providedClasses makes "
        + "the linker drop the compiled definitions, leaving the AttributeError seen in "
        + "Wave 5 item 10.",
      None,
      PyIRRuntime.providedClass(PyClassName("scala.Int_"))
    )

  @Test def charCompanionIsNotRuntimeProvided(): Unit =
    assertEquals(
      "scala.Char_ must NOT be runtime-provided — its char2int / char2long / char2float "
        + "/ char2double come from compiled library-py PyIR.",
      None,
      PyIRRuntime.providedClass(PyClassName("scala.Char_"))
    )

  @Test def parallelPrimitiveCompanionsAreAlsoNotProvided(): Unit =
    // Sanity that the structural rule we rely on (compiled `.pyir` for
    // the primitive companion flows through to Python) holds for every
    // primitive companion that carries implicit-coercion methods:
    //   Byte_  -> byte2{short,int,long,float,double}
    //   Short_ -> short2{int,long,float,double}
    //   Long_  -> long2{float,double}
    //   Float_ -> float2double
    // None of these were ever in providedClasses; the ones that DID
    // get listed (Int_, Char_) were the dropouts.
    for sym <- List("scala.Byte_", "scala.Short_", "scala.Long_", "scala.Float_") do
      assertEquals(
        s"$sym must NOT be runtime-provided",
        None,
        PyIRRuntime.providedClass(PyClassName(sym))
      )

  // -----------------------------------------------------------------
  //  Runtime prelude: shadow module stubs are gone.
  // -----------------------------------------------------------------

  @Test def preludeDoesNotShadowIntCompanionWithStubModule(): Unit =
    val prelude = PyIRRuntime.content
    assertFalse(
      "_scpy_IntModule shadow stub must not appear in the prelude — it intercepts "
        + "static calls intended for the compiled scala.Int_ class.",
      prelude.contains("class _scpy_IntModule")
    )
    assertFalse(
      "_scpy_CharModule shadow stub must not appear in the prelude.",
      prelude.contains("class _scpy_CharModule")
    )
    assertFalse(
      "Module-singleton binding `_scpy_mod_scala_Int_ = _scpy_IntModule()` must "
        + "not appear in the prelude — the compiled scala.Int_ supplies it via the "
        + "standard `_scpy_lazy_module(scala_Int_)` form.",
      prelude.contains("_scpy_mod_scala_Int_ = _scpy_IntModule()")
    )
    assertFalse(
      prelude.contains("_scpy_mod_scala_Char_ = _scpy_CharModule()")
    )

  @Test def preludeDoesNotPreRegisterPrimitiveCompanions(): Unit =
    // The compiled scala.Int_ / scala.Char_ classes register themselves
    // via the standard `_scpy_register_class(scala_Int_, ...)` epilogue.
    // Pre-registering with `None` as the runtime class object would
    // collide with the real registration.
    val prelude = PyIRRuntime.content
    assertFalse(
      prelude.contains("""_scpy_register_class(None, "scala.Int_"""")
    )
    assertFalse(
      prelude.contains("""_scpy_register_class(None, "scala.Char_"""")
    )

  // -----------------------------------------------------------------
  //  End-to-end: a User input that calls `scala.Int_.int2double` as a
  //  static helper links cleanly when the Support input supplies the
  //  class. Pre-fix this would silently drop the support definition
  //  and the runtime would AttributeError.
  // -----------------------------------------------------------------

  @Test def userCallToIntCompanionStaticIsPreservedAfterLink(): Unit =
    val intCompanion = PyClassName("scala.Int_")
    val int2double = PyMethodName(
      PySimpleMethodName("int2double"),
      List(PyPrimRef.IntRef),
      PyPrimRef.DoubleRef
    )

    // Support: the compiled scala.Int_ from library-py. The implicit
    // coercion lowers to a static def on the companion module class.
    val intCompanionDef = classDef(
      name = intCompanion,
      kind = PyClassKind.ModuleClass,
      methods = List(
        ctor(),
        method(
          name = int2double,
          namespace = PyMemberNamespace.PublicStatic,
          resultType = PyDoubleType,
          body = PyDoubleLit(0.0)(NoPos),
          args = List(
            PyParamDef(
              name = PyLocalName("x"),
              originalName = PyOriginalName.NoOriginalName,
              ptpe = PyIntType,
              mutable = false,
              pos = NoPos
            )
          )
        )
      )
    )

    // User: a Test main that statically calls scala.Int_.int2double(0).
    val userMain = classDef(
      name = className("example.Test"),
      kind = PyClassKind.ModuleClass,
      methods = List(
        method(
          name = methodName("main"),
          namespace = PyMemberNamespace.PublicStatic,
          body = PyApplyStatic(
            PyApplyFlags.empty,
            intCompanion,
            int2double,
            List(PyIntLit(0)(NoPos))
          )(PyDoubleType, NoPos)
        )
      )
    )

    val bundle = PyLinker.link(
      List(
        PyLinker.Input(List(intCompanionDef), None, PyLinker.InputSource.Support),
        PyLinker.Input(List(userMain), Some((userMain.name, userMain.kind)), PyLinker.InputSource.User)
      )
    )

    assertTrue(
      "scala.Int_ must survive linking when a User input calls int2double on it.",
      bundle.classes.exists(_.name == intCompanion)
    )
    val emitted = PyIREmitter.emitToString(bundle.classes, bundle.mainEntry)
    assertTrue(
      "Emitted bundle must define the int2double helper on the compiled scala.Int_.",
      emitted.contains("def int2double__I__D")
    )

  // -----------------------------------------------------------------
  //  Helpers — duplicated from PyLinkerTest. Keeping them local
  //  avoids cross-suite coupling for a small DSL.
  // -----------------------------------------------------------------

  private def className(name: String): PyClassName =
    PyClassName(name)

  private def methodName(
      simple: String,
      paramRefs: List[PyTypeRef] = Nil,
      resultRef: PyTypeRef = PyPrimRef.VoidRef
  ): PyMethodName =
    PyMethodName(PySimpleMethodName(simple), paramRefs, resultRef)

  private def ctorName(paramRefs: List[PyTypeRef] = Nil): PyMethodName =
    PyMethodName(PySimpleMethodName.Constructor, paramRefs, PyPrimRef.VoidRef)

  private def method(
      name: PyMethodName,
      namespace: PyMemberNamespace = PyMemberNamespace.Public,
      resultType: PyType = PyVoidType,
      body: PyTree = PySkip()(NoPos),
      args: List[PyParamDef] = Nil
  ): PyMethodDef =
    PyMethodDef(
      flags = PyMemberFlags.empty.withNamespace(namespace),
      name = name,
      originalName = PyOriginalName.NoOriginalName,
      args = args,
      resultType = resultType,
      body = Some(body),
      pos = NoPos
    )

  private def ctor(
      body: PyTree = PySkip()(NoPos),
      args: List[PyParamDef] = Nil
  ): PyMethodDef =
    method(
      name = ctorName(args.map(arg => PyTypes.toRef(arg.ptpe))),
      namespace = PyMemberNamespace.Constructor,
      body = body,
      args = args
    )

  private def classDef(
      name: PyClassName,
      kind: PyClassKind = PyClassKind.Class,
      superClass: Option[PyClassName] = None,
      interfaces: List[PyClassName] = Nil,
      methods: List[PyMethodDef] = Nil
  ): PyClassDef =
    PyClassDef(
      name = name,
      originalName = PyOriginalName.NoOriginalName,
      kind = kind,
      superClass = superClass,
      interfaces = interfaces,
      fields = Nil,
      methods = methods,
      pos = NoPos
    )

end PyPrimitiveCastTest
