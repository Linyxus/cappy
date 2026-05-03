package dotty.tools.backend.python

import dotty.tools.backend.python.ir.pyir.*

import org.junit.Assert.*
import org.junit.Test

/** Coverage for the structural fix that flips the
 *  `tests/run/unroll-*-integration` cluster: when a single
 *  separate-compilation test rewrites the same JVM class across
 *  multiple groups (`Unrolled_1.scala`, `Unrolled_2.scala`,
 *  `Unrolled_3.scala`), each compilation lands as a sibling `.pyir`
 *  in the output dir. The linker must pick the freshest `.pyir`
 *  rather than erroring on Support × Support duplicates, and it
 *  must accept multiple User inputs declaring distinct classes
 *  (the deferred-link path that GenPython uses to delay linking
 *  until every CU in a single compile invocation has written its
 *  `.pyir`).
 *
 *  The test cases here are framework-level, not Scala source-level:
 *  they construct PyIR by hand to isolate the linker behavior from
 *  the typer/erasure pipeline. End-to-end coverage of the actual
 *  `@unroll` forwarders flows through the `tests/run/unroll-*`
 *  fixtures via `PyStockRunTests`.
 */
class PyUnrollForwarderTest:

  private val NoPos = PyPosition.NoPosition

  /** Method-on-class shape: same JVM class declared by two Support
   *  inputs (older shape with one `foo` overload, newer shape with
   *  two). The newer input has a higher `priority`; the linker must
   *  emit only the newer class so callers compiled against either
   *  shape still find the `foo` overload set the JVM would expose.
   *  Without the priority tiebreak this raised
   *  `Duplicate class 'example.Unrolled'`.
   */
  @Test def latestSupportPyirWinsOnDuplicateClass(): Unit =
    val owner = className("example.Unrolled")

    val v1 = classDef(
      name = owner,
      methods = List(
        ctor(),
        method(name = methodName("foo")),
      )
    )

    val v2 = classDef(
      name = owner,
      methods = List(
        ctor(),
        method(name = methodName("foo")),
        method(name = methodName("foo", paramRefs = List(PyPrimRef.IntRef))),
      )
    )

    val driver = classDef(
      name = className("example.Driver"),
      kind = PyClassKind.ModuleClass,
      methods = List(
        method(
          name = methodName("main"),
          namespace = PyMemberNamespace.PublicStatic,
          body = PyApply(
            PyApplyFlags.empty,
            PyNew(owner, ctorName(), Nil)(NoPos),
            owner,
            methodName("foo", paramRefs = List(PyPrimRef.IntRef)),
            List(PyIntLit(1)(NoPos))
          )(PyVoidType, NoPos)
        )
      )
    )

    val bundle = PyLinker.link(
      List(
        PyLinker.Input(List(driver), Some((driver.name, driver.kind)), PyLinker.InputSource.User)
      ),
      List(
        PyLinker.Input(List(v1), None, PyLinker.InputSource.Support, priority = 100L),
        PyLinker.Input(List(v2), None, PyLinker.InputSource.Support, priority = 200L),
      )
    )

    val unrolled = bundle.classes.find(_.name == owner).getOrElse(
      throw new AssertionError("Unrolled was pruned")
    )
    val fooArities = unrolled.methods
      .iterator
      .filter(_.name.simple.name == "foo")
      .map(_.name.paramTypeRefs.length)
      .toSet
    assertTrue(
      s"Expected the V2 (higher-priority) class to win; saw arities $fooArities",
      fooArities.contains(1)
    )

  /** Reverse ordering of the same scenario: V2 loaded before V1.
   *  The result must still be V2 because `priority` — not insertion
   *  order — is the discriminator. Guards against a regression
   *  where the linker silently picked first-seen on
   *  Support × Support duplicates.
   */
  @Test def latestSupportPyirWinsRegardlessOfLoadOrder(): Unit =
    val owner = className("example.Unrolled")

    val v1 = classDef(name = owner, methods = List(ctor(), method(name = methodName("foo"))))
    val v2 = classDef(
      name = owner,
      methods = List(
        ctor(),
        method(name = methodName("foo")),
        method(name = methodName("foo", paramRefs = List(PyPrimRef.IntRef))),
      )
    )

    val driver = classDef(
      name = className("example.Driver"),
      kind = PyClassKind.ModuleClass,
      methods = List(
        method(
          name = methodName("main"),
          namespace = PyMemberNamespace.PublicStatic,
          body = PyApply(
            PyApplyFlags.empty,
            PyNew(owner, ctorName(), Nil)(NoPos),
            owner,
            methodName("foo", paramRefs = List(PyPrimRef.IntRef)),
            List(PyIntLit(1)(NoPos))
          )(PyVoidType, NoPos)
        )
      )
    )

    val bundle = PyLinker.link(
      List(
        PyLinker.Input(List(driver), Some((driver.name, driver.kind)), PyLinker.InputSource.User)
      ),
      List(
        PyLinker.Input(List(v2), None, PyLinker.InputSource.Support, priority = 200L),
        PyLinker.Input(List(v1), None, PyLinker.InputSource.Support, priority = 100L),
      )
    )

    val unrolled = bundle.classes.find(_.name == owner).getOrElse(
      throw new AssertionError("Unrolled was pruned")
    )
    val fooArities = unrolled.methods
      .iterator
      .filter(_.name.simple.name == "foo")
      .map(_.name.paramTypeRefs.length)
      .toSet
    assertTrue(
      s"Expected V2 to win regardless of load order; saw arities $fooArities",
      fooArities.contains(1)
    )

  /** Primary-constructor shape: the case-class/primary-constructor
   *  forwarders generated for `@unroll` add overloaded constructors,
   *  not overloaded methods. The same class declared in two Support
   *  inputs with different ctor arities exercises the same code path
   *  as the method-side test, but ensures constructor identity (the
   *  `<init>` slot) is included in the per-class merge.
   */
  @Test def primaryCtorForwarderShapeFromFreshestPyir(): Unit =
    val owner = className("example.UnrolledCtor")

    val v1 = classDef(
      name = owner,
      methods = List(ctor()),
    )
    val v2 = classDef(
      name = owner,
      methods = List(
        ctor(),
        ctor(args = List(paramDef("x", PyIntType))),
      ),
    )

    val driver = classDef(
      name = className("example.Driver"),
      kind = PyClassKind.ModuleClass,
      methods = List(
        method(
          name = methodName("main"),
          namespace = PyMemberNamespace.PublicStatic,
          body = PyNew(owner, ctorName(List(PyPrimRef.IntRef)), List(PyIntLit(1)(NoPos)))(NoPos)
        )
      )
    )

    val bundle = PyLinker.link(
      List(
        PyLinker.Input(List(driver), Some((driver.name, driver.kind)), PyLinker.InputSource.User)
      ),
      List(
        PyLinker.Input(List(v1), None, PyLinker.InputSource.Support, priority = 1L),
        PyLinker.Input(List(v2), None, PyLinker.InputSource.Support, priority = 2L),
      )
    )

    val unrolledCtorArities = bundle.classes
      .find(_.name == owner)
      .toList
      .flatMap(_.methods)
      .iterator
      .filter(_.name.simple == PySimpleMethodName.Constructor)
      .map(_.name.paramTypeRefs.length)
      .toSet
    assertTrue(
      s"V2 (with the extra ctor) must win; saw ctor arities $unrolledCtorArities",
      unrolledCtorArities.contains(1)
    )

  /** User × Support overlap: User input keeps authoritative even
   *  when a Support input declares the same class with a higher
   *  priority. This is the freshness rule for an in-progress
   *  recompile (the just-emitted `.pyir` for the current CU is a
   *  Support input written to disk; its in-memory User counterpart
   *  must take precedence). Guards against a regression where
   *  priority would override the User vs. Support precedence.
   */
  @Test def userInputBeatsHigherPrioritySupport(): Unit =
    val owner = className("example.User")

    val supportShape = classDef(
      name = owner,
      methods = List(ctor(), method(name = methodName("ghost")))
    )

    val userShape = classDef(
      name = owner,
      methods = List(ctor(), method(name = methodName("real")))
    )

    val bundle = PyLinker.link(
      List(PyLinker.Input(List(userShape), None, PyLinker.InputSource.User)),
      List(PyLinker.Input(List(supportShape), None, PyLinker.InputSource.Support, priority = Long.MaxValue))
    )

    val emitted = bundle.classes.find(_.name == owner).getOrElse(
      throw new AssertionError("User class was pruned")
    )
    val methodNames = emitted.methods.iterator.map(_.name.simple.name).toSet
    assertTrue(s"User shape must win; saw methods $methodNames", methodNames.contains("real"))
    assertFalse(s"Support shape must lose; saw methods $methodNames", methodNames.contains("ghost"))

  // --- helpers (mirror PyLinkerTest fixtures, kept local so this file
  //     can fail-isolate from changes to the bigger linker test suite) -----

  private def className(name: String): PyClassName = PyClassName(name)

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

  private def paramDef(name: String, tpe: PyType): PyParamDef =
    PyParamDef(
      name = PyLocalName(name),
      originalName = PyOriginalName.NoOriginalName,
      ptpe = tpe,
      mutable = false,
      pos = NoPos
    )

  private def classDef(
      name: PyClassName,
      kind: PyClassKind = PyClassKind.Class,
      superClass: Option[PyClassName] = None,
      interfaces: List[PyClassName] = Nil,
      fields: List[PyFieldDef] = Nil,
      methods: List[PyMethodDef] = Nil
  ): PyClassDef =
    PyClassDef(
      name = name,
      originalName = PyOriginalName.NoOriginalName,
      kind = kind,
      superClass = superClass,
      interfaces = interfaces,
      fields = fields,
      methods = methods,
      pos = NoPos
    )
