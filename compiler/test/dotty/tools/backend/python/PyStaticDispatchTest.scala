package dotty.tools.backend.python

import dotty.tools.backend.python.ir.pyir.*

import org.junit.Assert.*
import org.junit.Test

/** Regression tests for `@scala.annotation.static` method dispatch.
 *
 *  `MoveStatics` lifts `@static def`s from the module class onto the
 *  companion class but intentionally does NOT set `JavaStatic` on the
 *  lifted symbol (to preserve `.enclosingClass`). The Python backend
 *  used to gate "emit as `@staticmethod` / dispatch via
 *  `<Owner>.<m>(...)`" purely on the `JavaStatic` flag, leaving
 *  `@static`-annotated members emitted as instance methods on the
 *  companion class while every call site still routed through the
 *  module receiver. The mismatch surfaced as e.g.
 *  `AttributeError: 'Test_' object has no attribute 'test__I__I'`.
 *
 *  These tests pin the IR-level contract that the GenPython predicate
 *  `isStaticMember` enforces: a method declared with the
 *  `PublicStatic` member-namespace lands as `@staticmethod` in the
 *  emitted Python class, and a `PyApplyStatic` call site lowers to
 *  `<Owner>.<encoded-name>(...)` (no implicit `self` receiver). The
 *  end-to-end behaviour is covered by
 *  `tests/pos-py/static-method-dispatch.scala`; this file exercises the
 *  emitter directly so a regression in the predicate can be caught
 *  without a full pipeline run.
 */
class PyStaticDispatchTest:

  private val NoPos = PyPosition.NoPosition

  // ----------------------------------------------------------------
  // Helpers (mirrors PyIREmitterHoistTest)
  // ----------------------------------------------------------------

  private def className(name: String): PyClassName = PyClassName(name)

  private def methodName(
      simple:    String,
      paramRefs: List[PyTypeRef] = Nil,
      resultRef: PyTypeRef       = PyPrimRef.VoidRef
  ): PyMethodName =
    PyMethodName(PySimpleMethodName(simple), paramRefs, resultRef)

  private def ctor(): PyMethodDef =
    PyMethodDef(
      flags        = PyMemberFlags.empty.withNamespace(PyMemberNamespace.Constructor),
      name         = PyMethodName(PySimpleMethodName.Constructor, Nil, PyPrimRef.VoidRef),
      originalName = PyOriginalName.NoOriginalName,
      args         = Nil,
      resultType   = PyVoidType,
      body         = Some(PySkip()(NoPos)),
      pos          = NoPos
    )

  private def method(
      name:       PyMethodName,
      args:       List[PyParamDef]  = Nil,
      body:       PyTree            = PySkip()(NoPos),
      resultType: PyType            = PyVoidType,
      namespace:  PyMemberNamespace = PyMemberNamespace.Public
  ): PyMethodDef =
    PyMethodDef(
      flags        = PyMemberFlags.empty.withNamespace(namespace),
      name         = name,
      originalName = PyOriginalName.NoOriginalName,
      args         = args,
      resultType   = resultType,
      body         = Some(body),
      pos          = NoPos
    )

  private def classDef(
      name:    PyClassName,
      methods: List[PyMethodDef],
      kind:    PyClassKind = PyClassKind.Class
  ): PyClassDef =
    PyClassDef(
      name         = name,
      originalName = PyOriginalName.NoOriginalName,
      kind         = kind,
      superClass   = None,
      interfaces   = Nil,
      fields       = Nil,
      methods      = methods,
      pos          = NoPos
    )

  // ----------------------------------------------------------------
  // PublicStatic namespace -> @staticmethod decl + Owner.method() call
  // ----------------------------------------------------------------

  /** A `PublicStatic`-namespace method declaration must emit a
   *  `@staticmethod` decorator above the `def`, and the parameter list
   *  must NOT include the implicit `self`. Pre-fix, a `@static def`
   *  whose lifted symbol lacked `JavaStatic` was emitted as a regular
   *  instance method (with `self`) on the companion class, while the
   *  call site still issued a `PyApplyStatic` against `Owner.<m>`. The
   *  receiver mismatch produced the `test__I__I` AttributeError. */
  @Test def publicStaticMethodEmittedWithStaticmethodDecorator(): Unit =
    val intParam = PyParamDef(
      name         = PyLocalName("n"),
      originalName = PyOriginalName.NoOriginalName,
      ptpe         = PyIntType,
      mutable      = false,
      pos          = NoPos
    )
    val staticMethod = method(
      name       = methodName("test", List(PyPrimRef.IntRef), PyPrimRef.IntRef),
      args       = List(intParam),
      resultType = PyIntType,
      namespace  = PyMemberNamespace.PublicStatic
    )
    val cls = classDef(
      name    = className("Holder"),
      methods = List(ctor(), staticMethod)
    )
    val src = PyIREmitter.emitToString(List(cls), None)

    // Locate the method declaration line and verify the immediately
    // preceding non-blank line is `@staticmethod`.
    val lines = src.linesIterator.toIndexedSeq
    val defPrefix = "def test__I__I"
    val defIdx = lines.indexWhere(_.trim.startsWith(defPrefix))
    assertTrue(
      s"expected `def test__I__I(...)` in emitted source:\n$src",
      defIdx >= 0
    )
    // Walk backward past blank lines.
    var prev = defIdx - 1
    while prev >= 0 && lines(prev).trim.isEmpty do prev -= 1
    assertTrue(
      s"expected `@staticmethod` on the line preceding `def test__I__I` in:\n$src",
      prev >= 0 && lines(prev).trim == "@staticmethod"
    )
    // The parameter list must NOT start with `self,` (we keep `n` only).
    val defLine = lines(defIdx)
    assertFalse(
      s"static method must not declare `self`; got: $defLine",
      defLine.contains("(self") || defLine.contains("(self,")
    )

  // ----------------------------------------------------------------
  // PyApplyStatic -> Owner.method(...) (no implicit self receiver)
  // ----------------------------------------------------------------

  /** A `PyApplyStatic` call lowers to `<Owner>.<encoded>(...)`, never
   *  to `self.<encoded>(...)`. The full encoded form is the same one
   *  that pins the declaration name (`isStaticMember(sym)` is the
   *  single source of truth on both sides), so a per-fixture mismatch
   *  cannot happen by construction once both ends consult the same
   *  predicate. */
  @Test def staticCallSiteEmitsOwnerDotMethod(): Unit =
    val intParam = PyParamDef(
      name         = PyLocalName("n"),
      originalName = PyOriginalName.NoOriginalName,
      ptpe         = PyIntType,
      mutable      = false,
      pos          = NoPos
    )
    val staticTarget = methodName("test", List(PyPrimRef.IntRef), PyPrimRef.IntRef)
    val staticMethod = method(
      name       = staticTarget,
      args       = List(intParam),
      body       = PyVarRef(PyLocalName("n"))(PyIntType, NoPos),
      resultType = PyIntType,
      namespace  = PyMemberNamespace.PublicStatic
    )
    val callerBody = PyApplyStatic(
      flags     = PyApplyFlags.empty,
      className = className("Holder"),
      method    = staticTarget,
      args      = List(PyIntLit(7)(NoPos))
    )(PyIntType, NoPos)
    val callerMethod = method(
      name       = methodName("invoke", Nil, PyPrimRef.IntRef),
      body       = callerBody,
      resultType = PyIntType
    )
    val holder = classDef(
      name    = className("Holder"),
      methods = List(ctor(), staticMethod)
    )
    val caller = classDef(
      name    = className("Caller"),
      methods = List(ctor(), callerMethod)
    )
    val src = PyIREmitter.emitToString(List(holder, caller), None)

    // The call site must mention `Holder.test__I__I(7)`. It must NOT
    // route through `self` (the receiver-mismatch shape we're guarding
    // against).
    assertTrue(
      s"expected `Holder.test__I__I(7)` in emitted source:\n$src",
      src.contains("Holder.test__I__I(7)")
    )
    assertFalse(
      s"static call must not route through `self`:\n$src",
      src.contains("self.test__I__I(")
    )

end PyStaticDispatchTest
