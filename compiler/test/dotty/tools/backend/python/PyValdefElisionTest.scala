package dotty.tools.backend.python

import dotty.tools.backend.python.ir.pyir.*

import org.junit.Assert.*
import org.junit.Test

/** Tests for backing-field handling on `Null` / `Unit` / `Nothing`-typed
 *  val/var/lazy-val members — Wave 6 item 03
 *  (`notes/wave6-worklist/03-null-unit-nothing-valdefs.md`).
 *
 *  Background. The Scala 3 `LazyVals` mini-phase emits a three-way
 *  dispatch in the lazy-val accessor:
 *
 *  {{{
 *    val result = this.foo$lzy1
 *    if (result.isInstanceOf[Null])  result.asInstanceOf[Null]    // arm 1
 *    else if (result eq NullValue)   null                          // arm 2
 *    else                            foo$lzyINIT1.asInstanceOf[Null] // arm 3
 *  }}}
 *
 *  On the JVM, arm 1 emits `instanceof scala/runtime/Null$`, which is
 *  always `false` at runtime — `Null$` has no instances. Control flow
 *  therefore falls into arm 2, which compares against the `NullValue`
 *  sentinel and unwraps it to `null` for the caller.
 *
 *  The Python backend previously lowered `isInstanceOf[Null]` (and
 *  `isInstanceOf[Nothing]`) to
 *  `_scpy_is_value_of_type(result, _scpy_class_of_name("java.lang.Object"))`
 *  via the `NullRef`/`NothingRef` fall-through in
 *  `PyIREmitter.typeRefToClassExpr`. That predicate matches every
 *  non-null Python object — including the `LazyVals.NullValue`
 *  sentinel, so arm 1 returned the sentinel object directly, leaking
 *  it past the unwrap step in arm 2. The resulting `println(f.foo)`
 *  then printed `scala.runtime.LazyVals$NullValue$@<id>` instead of
 *  `null`.
 *
 *  Fix: special-case `PyIsInstanceOf` for the two no-instance type
 *  refs to emit `_scpy_is_value_of_type(<expr>, None)`. The runtime
 *  helper short-circuits to `False` when given `clazz=None`, exactly
 *  matching the JVM `instanceof scala/runtime/{Null$,Nothing$}`
 *  semantics: the operand is evaluated for side effects, the result
 *  is `False`.
 */
class PyValdefElisionTest:

  private val NoPos = PyPosition.NoPosition

  // -- Helpers (mirrored from PyIREmitterHoistTest) ------------------

  private def methodName(
      simple:    String,
      paramRefs: List[PyTypeRef] = Nil,
      resultRef: PyTypeRef       = PyPrimRef.BooleanRef
  ): PyMethodName =
    PyMethodName(PySimpleMethodName(simple), paramRefs, resultRef)

  private def method(
      name:       PyMethodName,
      body:       PyTree,
      resultType: PyType            = PyBooleanType,
      namespace:  PyMemberNamespace = PyMemberNamespace.Public
  ): PyMethodDef =
    PyMethodDef(
      flags        = PyMemberFlags.empty.withNamespace(namespace),
      name         = name,
      originalName = PyOriginalName.NoOriginalName,
      args         = Nil,
      resultType   = resultType,
      body         = Some(body),
      pos          = NoPos
    )

  private def classDef(name: PyClassName, methods: List[PyMethodDef]): PyClassDef =
    PyClassDef(
      name         = name,
      originalName = PyOriginalName.NoOriginalName,
      kind         = PyClassKind.Class,
      superClass   = None,
      interfaces   = Nil,
      fields       = Nil,
      methods      = methods,
      pos          = NoPos
    )

  private def emitMethod(method: PyMethodDef): String =
    val cls = classDef(PyClassName("Test"), List(method))
    PyIREmitter.emitToString(List(cls), None)

  /** Build a `def isFoo(): Boolean = result.isInstanceOf[T]`-shaped method
   *  whose body is a single `PyReturn(PyIsInstanceOf(varRef, testType))`.
   *  Returns the emitted Python source so callers can assert on the
   *  `_scpy_is_value_of_type(...)` line.
   */
  private def emitInstanceOfMethod(testType: PyTypeRef): String =
    val varRef = PyVarRef(PyLocalName("result"))(PyAnyType, NoPos)
    val body   = PyReturn(PyIsInstanceOf(varRef, testType)(NoPos))(NoPos)
    emitMethod(method(methodName("isFoo"), body))

  /** Pull the `return _scpy_is_value_of_type(...)` line out of the
   *  emitted `isFoo` body. The runtime prelude also defines the
   *  `_scpy_is_value_of_type` helper, so we must scope the search to
   *  the user method's body (here: the unique `return ...` line).
   */
  private def isInstanceOfCallLine(source: String): String =
    val lines = source.linesIterator.toList
      .filter(_.trim.startsWith("return _scpy_is_value_of_type("))
    assertEquals(
      s"expected exactly one `return _scpy_is_value_of_type(...)` line in:\n$source",
      1, lines.length
    )
    lines.head.trim

  // -- Strict-val type-test elision (Null / Nothing) -----------------

  @Test def isInstanceOfNullEmitsNoneClassExpr(): Unit =
    val src = emitInstanceOfMethod(PyPrimRef.NullRef)
    val call = isInstanceOfCallLine(src)
    // The emitted clazz argument must be the literal `None`, which the
    // runtime `_scpy_is_value_of_type` short-circuits to `False`.
    assertTrue(
      s"expected `_scpy_is_value_of_type(result, None)` in:\n$src",
      call.contains("_scpy_is_value_of_type(result, None)")
    )
    // Must NOT keep the old fallback (`java.lang.Object`), which would
    // have matched every non-null receiver.
    assertFalse(
      s"`isInstanceOf[Null]` must not lower to `java.lang.Object` in:\n$src",
      call.contains("\"java.lang.Object\"")
    )

  @Test def isInstanceOfNothingEmitsNoneClassExpr(): Unit =
    val src = emitInstanceOfMethod(PyPrimRef.NothingRef)
    val call = isInstanceOfCallLine(src)
    assertTrue(
      s"expected `_scpy_is_value_of_type(result, None)` in:\n$src",
      call.contains("_scpy_is_value_of_type(result, None)")
    )
    assertFalse(
      s"`isInstanceOf[Nothing]` must not lower to `java.lang.Object` in:\n$src",
      call.contains("\"java.lang.Object\"")
    )

  // -- Other reference type tests are unchanged ----------------------

  @Test def isInstanceOfStringStillUsesClassOfName(): Unit =
    val src = emitInstanceOfMethod(PyClassRef(PyClassName.StringClass))
    val call = isInstanceOfCallLine(src)
    assertTrue(
      s"expected `_scpy_class_of_name(\"java.lang.String\")` arg in:\n$src",
      call.contains("_scpy_class_of_name(\"java.lang.String\")")
    )

  @Test def isInstanceOfObjectStillUsesClassOfName(): Unit =
    val src = emitInstanceOfMethod(PyClassRef(PyClassName.ObjectClass))
    val call = isInstanceOfCallLine(src)
    assertTrue(
      s"expected `_scpy_class_of_name(\"java.lang.Object\")` arg in:\n$src",
      call.contains("_scpy_class_of_name(\"java.lang.Object\")")
    )
    // And it must NOT degenerate to `None`.
    assertFalse(
      s"`isInstanceOf[Object]` must NOT lower to `None` in:\n$src",
      call.contains("_scpy_is_value_of_type(result, None)")
    )

  // -- Primitive type tests are unchanged ----------------------------

  @Test def isInstanceOfIntStillUsesPrimitiveExpr(): Unit =
    val src = emitInstanceOfMethod(PyPrimRef.IntRef)
    val call = isInstanceOfCallLine(src)
    assertTrue(
      s"expected `_scpy_primitive_int` arg in:\n$src",
      call.contains("_scpy_primitive_int")
    )

  // -- Runtime invariant: `_scpy_is_value_of_type(x, None) -> False` -

  @Test def runtimeIsValueOfTypeShortCircuitsOnNoneClazz(): Unit =
    // The codegen change only works because the runtime helper treats
    // `clazz is None` as `False`. Pin that contract here so the
    // emitter/runtime stay in sync.
    val prelude = PyIRRuntime.content
    val sliceStart = prelude.indexOf("def _scpy_is_value_of_type(")
    assertTrue(
      "expected `_scpy_is_value_of_type` definition in PyIRRuntime",
      sliceStart >= 0
    )
    val slice = prelude.substring(sliceStart, math.min(sliceStart + 200, prelude.length))
    assertTrue(
      s"_scpy_is_value_of_type must short-circuit to False on clazz is None:\n$slice",
      slice.contains("if clazz is None:") &&
      slice.indexOf("return False", slice.indexOf("if clazz is None:")) > 0
    )
