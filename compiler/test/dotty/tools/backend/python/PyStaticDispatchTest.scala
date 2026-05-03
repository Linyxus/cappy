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
      kind:    PyClassKind = PyClassKind.Class,
      fields:  List[PyFieldDef] = Nil
  ): PyClassDef =
    PyClassDef(
      name         = name,
      originalName = PyOriginalName.NoOriginalName,
      kind         = kind,
      superClass   = None,
      interfaces   = Nil,
      fields       = fields,
      methods      = methods,
      pos          = NoPos
    )

  private def field(
      owner:     PyClassName,
      simple:    String,
      ftpe:      PyType            = PyIntType,
      mutable:   Boolean           = false,
      namespace: PyMemberNamespace = PyMemberNamespace.Public
  ): PyFieldDef =
    PyFieldDef(
      flags        = PyMemberFlags.empty.withMutable(mutable).withNamespace(namespace),
      name         = PyFieldName(owner, PySimpleFieldName(simple)),
      originalName = PyOriginalName.NoOriginalName,
      ftpe         = ftpe,
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

  // ----------------------------------------------------------------
  // Static field dispatch (Wave 6 item 06)
  // ----------------------------------------------------------------

  /** Sibling of `publicStaticMethodEmittedWithStaticmethodDecorator`,
   *  for the field side. A `PublicStatic`-namespace `PyFieldDef` must
   *  emit a class-level attribute (no `self.<name> = default` line in
   *  `__init__`), mirroring the JVM's `<clinit>`-time static slot.
   *  Pre-fix, every `PyFieldDef` was zero-initialized in `__init__`
   *  regardless of namespace, which shadowed the class-level slot on
   *  every instance (`AttributeError: 'Foo' object has no attribute
   *  'field'` showed up at the call site, not here, because the read
   *  was already routed to `self.field`). The two ends of the
   *  contract — declaration namespace + read/write site — both go
   *  through the codegen `isStaticMember` predicate, so a per-fixture
   *  mismatch cannot happen by construction. */
  @Test def publicStaticFieldEmittedAsClassLevelAttribute(): Unit =
    val ownerName = className("Holder")
    val staticField = field(
      owner     = ownerName,
      simple    = "field",
      ftpe      = PyIntType,
      mutable   = false,
      namespace = PyMemberNamespace.PublicStatic
    )
    val cls = classDef(
      name    = ownerName,
      methods = List(ctor()),
      fields  = List(staticField)
    )
    val src = PyIREmitter.emitToString(List(cls), None)

    // The class body must contain a top-level `field = None` line
    // before any method def. The class-level default uses `None`
    // (not the JVM-typed default `0`) so the cross-module-init
    // cascade keeps the "uninitialized static field reads as null"
    // semantics — see `PyIREmitter#classLevelFieldInitExpr`. The
    // synthesized `<clinit>` overwrites the slot with the user's
    // initializer right after class registration.
    val lines = src.linesIterator.toIndexedSeq
    val classIdx = lines.indexWhere(_.startsWith("class Holder"))
    assertTrue(
      s"expected `class Holder` in emitted source:\n$src",
      classIdx >= 0
    )
    val classLevelLine = lines.drop(classIdx + 1)
      .takeWhile(l => l.isEmpty || l.startsWith(" "))
      .find(_.trim == "field = None")
    assertTrue(
      s"expected class-level `field = None` for static val in:\n$src",
      classLevelLine.isDefined
    )
    // Pre-fix: `__init__` had `self.field = 0`. After fix, it must not
    // emit any `self.field = ...` line — the field lives on the class
    // slot, not per-instance.
    assertFalse(
      s"static field must not be re-initialized on every instance via `self.field` in:\n$src",
      src.contains("self.field")
    )

  /** A static field READ goes through `PySelectStatic`, which renders
   *  as `<Owner>.<simple>` (or `<Owner>.<encoded>` for private
   *  fields). Pre-fix, the codegen lowered `Foo.field` (where `field`
   *  carries `@scala.annotation.static` and lifts onto the companion
   *  via `MoveStatics` without `JavaStatic`) to a regular `PySelect`
   *  against the qualifier — the qualifier resolved to the module
   *  proxy `Foo$`, but the field landed on `Foo` (the companion
   *  class), so the read raised `AttributeError`. */
  @Test def staticFieldReadEmitsOwnerDotName(): Unit =
    val ownerName = className("Holder")
    val staticField = field(
      owner     = ownerName,
      simple    = "field",
      ftpe      = PyIntType,
      namespace = PyMemberNamespace.PublicStatic
    )
    val staticReadBody = PySelectStatic(staticField.name)(PyIntType, NoPos)
    val readerMethod = method(
      name       = methodName("read", Nil, PyPrimRef.IntRef),
      body       = staticReadBody,
      resultType = PyIntType
    )
    val holder = classDef(
      name    = ownerName,
      methods = List(ctor()),
      fields  = List(staticField)
    )
    val reader = classDef(
      name    = className("Reader"),
      methods = List(ctor(), readerMethod)
    )
    val src = PyIREmitter.emitToString(List(holder, reader), None)

    // Reader.read body should contain `Holder.field` (not
    // `self.field`). The encoded form for a public field is the
    // simple name.
    assertTrue(
      s"expected `Holder.field` read in emitted source:\n$src",
      src.contains("Holder.field")
    )
    assertFalse(
      s"static field read must not route through `self.field`:\n$src",
      src.contains("self.field")
    )

  /** A static field WRITE goes through the same `PySelectStatic`
   *  shape as a read, since `PySelectStatic` extends `PyAssignable`.
   *  Pre-fix, the var setter `mutable_=` (a `@static def` whose body
   *  is `Assign(Ident(mutable), x_1)`) emitted `self.mutable = x_1`
   *  inside the `@staticmethod`, which raised `NameError: name 'self'
   *  is not defined` at runtime (no `self` parameter on a
   *  staticmethod). The write must land on the class slot:
   *  `Holder.mutable = x_1`. */
  @Test def staticFieldWriteEmitsOwnerDotName(): Unit =
    val ownerName = className("Holder")
    val staticField = field(
      owner     = ownerName,
      simple    = "mutable",
      ftpe      = PyIntType,
      mutable   = true,
      namespace = PyMemberNamespace.PublicStatic
    )
    val xParam = PyParamDef(
      name         = PyLocalName("x_1"),
      originalName = PyOriginalName.NoOriginalName,
      ptpe         = PyIntType,
      mutable      = false,
      pos          = NoPos
    )
    val setterBody = PyAssign(
      PySelectStatic(staticField.name)(PyIntType, NoPos),
      PyVarRef(PyLocalName("x_1"))(PyIntType, NoPos)
    )(NoPos)
    val setterMethod = method(
      name       = methodName("mutable__eq", List(PyPrimRef.IntRef), PyPrimRef.VoidRef),
      args       = List(xParam),
      body       = setterBody,
      resultType = PyVoidType,
      namespace  = PyMemberNamespace.PublicStatic
    )
    val holder = classDef(
      name    = ownerName,
      methods = List(ctor(), setterMethod),
      fields  = List(staticField)
    )
    val src = PyIREmitter.emitToString(List(holder), None)

    // The setter body must mention `Holder.mutable = x_1` (not `self.mutable`).
    assertTrue(
      s"expected `Holder.mutable = x_1` write in emitted source:\n$src",
      src.contains("Holder.mutable = x_1")
    )
    assertFalse(
      s"static field write must not route through `self.mutable`:\n$src",
      src.contains("self.mutable")
    )

  /** Regression guard for Wave 5 item 11: the `@static def` shape
   *  it fixed must keep dispatching through `Owner.method(...)` after
   *  the field-side changes. Same fixture as
   *  `staticCallSiteEmitsOwnerDotMethod` above, asserted alongside
   *  the field tests so a single field-side change cannot regress
   *  method dispatch silently. */
  @Test def staticMethodDispatchRegressionGuard(): Unit =
    val intParam = PyParamDef(
      name         = PyLocalName("n"),
      originalName = PyOriginalName.NoOriginalName,
      ptpe         = PyIntType,
      mutable      = false,
      pos          = NoPos
    )
    val staticTarget = methodName("square", List(PyPrimRef.IntRef), PyPrimRef.IntRef)
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
      args      = List(PyIntLit(3)(NoPos))
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
    assertTrue(
      s"expected `Holder.square__I__I(3)` in emitted source:\n$src",
      src.contains("Holder.square__I__I(3)")
    )
    assertFalse(
      s"static method dispatch must not route through `self.square__I__I(`:\n$src",
      src.contains("self.square__I__I(")
    )

end PyStaticDispatchTest
