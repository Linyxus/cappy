package dotty.tools.backend.python

import dotty.tools.backend.python.ir.pyir.*

import org.junit.Assert.*
import org.junit.Test

class PyReachabilityTest:

  private val NoPos = PyPosition.NoPosition

  // -- Core rules --------------------------------------------------------

  @Test def keepsEveryUserClassVerbatim(): Unit =
    val a = classDef(className("example.A"))
    val b = classDef(className("example.B"))
    val result = PyReachability.analyze(
      userClasses    = List(a, b),
      supportClasses = Nil,
      mainEntry      = None
    )
    assertTrue(result.isReachable(a.name))
    assertTrue(result.isReachable(b.name))

  @Test def dropsUnreferencedSupportClass(): Unit =
    val user = classDef(className("example.User"))
    val dead = classDef(className("example.Dead"))
    val result = PyReachability.analyze(
      userClasses    = List(user),
      supportClasses = List(dead),
      mainEntry      = None
    )
    assertTrue(result.isReachable(user.name))
    assertFalse(result.isReachable(dead.name))

  @Test def keepsReferencedSupportClassThroughStaticCall(): Unit =
    val targetName = className("example.Target")
    val target = classDef(
      name    = targetName,
      methods = List(method(methodName("helper"), namespace = PyMemberNamespace.PublicStatic))
    )
    val user = classDef(
      name    = className("example.User"),
      methods = List(method(
        methodName("use"),
        body = PyApplyStatic(
          PyApplyFlags.empty, targetName, methodName("helper"), Nil
        )(PyVoidType, NoPos)
      ))
    )
    val result = PyReachability.analyze(
      userClasses    = List(user),
      supportClasses = List(target),
      mainEntry      = None
    )
    assertTrue(result.isReachable(target.name))

  @Test def keepsReferencedSupportClassThroughNew(): Unit =
    val targetName = className("example.Target")
    val target = classDef(name = targetName, methods = List(ctor()))
    val user = classDef(
      name    = className("example.User"),
      methods = List(method(
        methodName("use"),
        body = PyNew(targetName, ctorName(), Nil)(NoPos)
      ))
    )
    val result = PyReachability.analyze(
      userClasses    = List(user),
      supportClasses = List(target),
      mainEntry      = None
    )
    assertTrue(result.isReachable(target.name))

  @Test def keepsReferencedSupportClassThroughLoadModule(): Unit =
    val modName = className("example.Mod")
    val mod = classDef(name = modName, kind = PyClassKind.ModuleClass, methods = List(ctor()))
    val user = classDef(
      name    = className("example.User"),
      methods = List(method(
        methodName("use"),
        body = PyLoadModule(modName)(NoPos)
      ))
    )
    val result = PyReachability.analyze(
      userClasses    = List(user),
      supportClasses = List(mod),
      mainEntry      = None
    )
    assertTrue(result.isReachable(mod.name))

  @Test def keepsReferencedSupportClassThroughSelect(): Unit =
    val ownerName = className("example.Holder")
    val holder = classDef(
      name   = ownerName,
      fields = List(fieldDef(ownerName, "x", PyIntType))
    )
    val user = classDef(
      name    = className("example.User"),
      methods = List(method(
        methodName("use"),
        body = PySelectStatic(
          PyFieldName(ownerName, PySimpleFieldName("x"))
        )(PyIntType, NoPos)
      ))
    )
    val result = PyReachability.analyze(
      userClasses    = List(user),
      supportClasses = List(holder),
      mainEntry      = None
    )
    assertTrue(result.isReachable(holder.name))

  @Test def keepsReferencedSupportClassThroughClassOf(): Unit =
    val targetName = className("example.Target")
    val target = classDef(targetName)
    val user = classDef(
      name    = className("example.User"),
      methods = List(method(
        methodName("use"),
        body = PyClassOf(PyClassRef(targetName))(NoPos)
      ))
    )
    val result = PyReachability.analyze(
      userClasses    = List(user),
      supportClasses = List(target),
      mainEntry      = None
    )
    assertTrue(result.isReachable(target.name))

  @Test def keepsReferencedSupportClassThroughIsInstanceOf(): Unit =
    val targetName = className("example.Target")
    val target = classDef(targetName)
    val user = classDef(
      name    = className("example.User"),
      methods = List(method(
        methodName("use"),
        body = PyIsInstanceOf(
          PyThis()(PyClassType(targetName), NoPos),
          PyClassRef(targetName)
        )(NoPos)
      ))
    )
    val result = PyReachability.analyze(
      userClasses    = List(user),
      supportClasses = List(target),
      mainEntry      = None
    )
    assertTrue(result.isReachable(target.name))

  // -- Structural rules --------------------------------------------------

  @Test def keepsSuperclassOfKeptClass(): Unit =
    val baseName = className("example.Base")
    val base = classDef(baseName)
    val user = classDef(
      name       = className("example.User"),
      superClass = Some(baseName)
    )
    val result = PyReachability.analyze(
      userClasses    = List(user),
      supportClasses = List(base),
      mainEntry      = None
    )
    assertTrue(result.isReachable(base.name))

  @Test def keepsInterfacesOfKeptClass(): Unit =
    val ifaceName = className("example.I")
    val iface = classDef(ifaceName, kind = PyClassKind.Interface)
    val user = classDef(
      name       = className("example.User"),
      interfaces = List(ifaceName)
    )
    val result = PyReachability.analyze(
      userClasses    = List(user),
      supportClasses = List(iface),
      mainEntry      = None
    )
    assertTrue(result.isReachable(iface.name))

  @Test def keepsTransitiveSupportClass(): Unit =
    // user -> B (static call) -> C (new). C reachable only via B.
    val bName = className("example.B")
    val cName = className("example.C")
    val c     = classDef(cName, methods = List(ctor()))
    val b = classDef(
      name = bName,
      methods = List(
        method(
          methodName("helper"),
          namespace = PyMemberNamespace.PublicStatic,
          body = PyNew(cName, ctorName(), Nil)(NoPos)
        )
      )
    )
    val user = classDef(
      name    = className("example.User"),
      methods = List(method(
        methodName("use"),
        body = PyApplyStatic(
          PyApplyFlags.empty, bName, methodName("helper"), Nil
        )(PyVoidType, NoPos)
      ))
    )
    val result = PyReachability.analyze(
      userClasses    = List(user),
      supportClasses = List(b, c),
      mainEntry      = None
    )
    assertTrue(result.isReachable(b.name))
    assertTrue(result.isReachable(c.name))

  // -- Main entry -------------------------------------------------------

  @Test def keepsMainEntryClassEvenIfNoOneElseReferencesIt(): Unit =
    val mainName = className("example.MainMod")
    val mainMod  = classDef(mainName, kind = PyClassKind.ModuleClass)
    val result = PyReachability.analyze(
      userClasses    = Nil,
      supportClasses = List(mainMod),
      mainEntry      = Some((mainName, PyClassKind.ModuleClass))
    )
    assertTrue(result.isReachable(mainMod.name))

  // -- Sibling support classes are dropped when not referenced -----

  @Test def dropsSupportClassWhoseSiblingIsUsed(): Unit =
    // Classic DCE regression: both `Used` and `Unused` are support
    // classes. The user only references `Used`. `Unused` must not be
    // dragged in just because it lives in the "support" set.
    val usedName   = className("scala.Used")
    val unusedName = className("scala.Unused")
    val used = classDef(
      name    = usedName,
      methods = List(method(methodName("helper"), namespace = PyMemberNamespace.PublicStatic))
    )
    val unused = classDef(unusedName)
    val user = classDef(
      name    = className("example.User"),
      methods = List(method(
        methodName("use"),
        body = PyApplyStatic(
          PyApplyFlags.empty, usedName, methodName("helper"), Nil
        )(PyVoidType, NoPos)
      ))
    )
    val result = PyReachability.analyze(
      userClasses    = List(user),
      supportClasses = List(used, unused),
      mainEntry      = None
    )
    assertTrue(result.isReachable(used.name))
    assertFalse("sibling support class must not be kept", result.isReachable(unused.name))

  // -- Runtime-provided classes ----------------------------------------

  @Test def skipsRuntimeProvidedClassReferences(): Unit =
    // Referencing java.lang.Integer should NOT add it to the result —
    // runtime-provided classes are opaque to the linker/emitter.
    val integerClass = className("java.lang.Integer")
    val user = classDef(
      name    = className("example.User"),
      methods = List(method(
        methodName("use"),
        body = PyApplyStatic(
          PyApplyFlags.empty, integerClass, methodName("toBinaryString"), Nil
        )(PyVoidType, NoPos)
      ))
    )
    val result = PyReachability.analyze(
      userClasses    = List(user),
      supportClasses = Nil,
      mainEntry      = None
    )
    assertFalse(result.isReachable(integerClass))
    assertTrue(result.isReachable(user.name))

  // -- Helpers (cloned from PyLinkerTest) ------------------------------

  private def className(name: String): PyClassName =
    PyClassName(name)

  private def fieldDef(owner: PyClassName, name: String, tpe: PyType): PyFieldDef =
    PyFieldDef(
      flags        = PyMemberFlags.empty,
      name         = PyFieldName(owner, PySimpleFieldName(name)),
      originalName = PyOriginalName.NoOriginalName,
      ftpe         = tpe,
      pos          = NoPos
    )

  private def methodName(
      simple:    String,
      paramRefs: List[PyTypeRef] = Nil,
      resultRef: PyTypeRef       = PyPrimRef.VoidRef
  ): PyMethodName =
    PyMethodName(PySimpleMethodName(simple), paramRefs, resultRef)

  private def ctorName(paramRefs: List[PyTypeRef] = Nil): PyMethodName =
    PyMethodName(PySimpleMethodName.Constructor, paramRefs, PyPrimRef.VoidRef)

  private def method(
      name:       PyMethodName,
      namespace:  PyMemberNamespace = PyMemberNamespace.Public,
      resultType: PyType            = PyVoidType,
      body:       PyTree            = PySkip()(NoPos),
      args:       List[PyParamDef]  = Nil
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

  private def ctor(
      body: PyTree           = PySkip()(NoPos),
      args: List[PyParamDef] = Nil
  ): PyMethodDef =
    method(
      name      = ctorName(args.map(arg => PyTypes.toRef(arg.ptpe))),
      namespace = PyMemberNamespace.Constructor,
      body      = body,
      args      = args
    )

  private def classDef(
      name:       PyClassName,
      kind:       PyClassKind         = PyClassKind.Class,
      superClass: Option[PyClassName] = None,
      interfaces: List[PyClassName]   = Nil,
      fields:     List[PyFieldDef]    = Nil,
      methods:    List[PyMethodDef]   = Nil
  ): PyClassDef =
    PyClassDef(
      name         = name,
      originalName = PyOriginalName.NoOriginalName,
      kind         = kind,
      superClass   = superClass,
      interfaces   = interfaces,
      fields       = fields,
      methods      = methods,
      pos          = NoPos
    )
