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

  @Test def recordsFieldReadsAndWritesSeparately(): Unit =
    val ownerName = className("example.Holder")
    val readField = fieldDef(ownerName, "read", PyIntType, PyMemberNamespace.PublicStatic)
    val writtenField = fieldDef(ownerName, "written", PyIntType, PyMemberNamespace.PublicStatic)
    val holder = classDef(
      name   = ownerName,
      fields = List(readField, writtenField)
    )
    val user = classDef(
      name    = className("example.User"),
      methods = List(method(
        methodName("use"),
        body = PyBlock(
          List(
            PySelectStatic(readField.name)(PyIntType, NoPos),
            PyAssign(
              PySelectStatic(writtenField.name)(PyIntType, NoPos),
              PyIntLit(1)(NoPos)
            )(NoPos)
          ),
          PyUnitLit()(NoPos)
        )(NoPos)
      ))
    )
    val result = PyReachability.analyze(
      userClasses    = List(user),
      supportClasses = List(holder),
      mainEntry      = None
    )
    assertTrue(result.isFieldRead(ownerName, readField.name))
    assertFalse(result.isFieldWritten(ownerName, readField.name))
    assertFalse(result.isFieldRead(ownerName, writtenField.name))
    assertTrue(result.isFieldWritten(ownerName, writtenField.name))

  @Test def fieldAssignmentWalksQualifierAndRhsButDoesNotCountLhsAsRead(): Unit =
    val ownerName = className("example.Holder")
    val targetField = fieldDef(ownerName, "target", PyIntType)
    val rhsField = fieldDef(ownerName, "rhs", PyIntType, PyMemberNamespace.PublicStatic)
    val holder = classDef(
      name    = ownerName,
      fields  = List(targetField, rhsField),
      methods = List(ctor())
    )
    val user = classDef(
      name    = className("example.User"),
      methods = List(method(
        methodName("use"),
        body = PyAssign(
          PySelect(
            PyNew(ownerName, ctorName(), Nil)(NoPos),
            targetField.name
          )(PyIntType, NoPos),
          PySelectStatic(rhsField.name)(PyIntType, NoPos)
        )(NoPos)
      ))
    )
    val result = PyReachability.analyze(
      userClasses    = List(user),
      supportClasses = List(holder),
      mainEntry      = None
    )
    assertTrue(result.isInstantiated(ownerName))
    assertFalse(result.isFieldRead(ownerName, targetField.name))
    assertTrue(result.isFieldWritten(ownerName, targetField.name))
    assertTrue(result.isFieldRead(ownerName, rhsField.name))

  @Test def loadModuleAnalyzesInitializerFieldWrites(): Unit =
    val modName = className("example.Mod")
    val stateField = fieldDef(modName, "state", PyIntType)
    val mod = classDef(
      name   = modName,
      kind   = PyClassKind.ModuleClass,
      fields = List(stateField),
      methods = List(ctor(
        body = PyAssign(
          PySelect(
            PyThis()(PyClassType(modName), NoPos),
            stateField.name
          )(PyIntType, NoPos),
          PyIntLit(1)(NoPos)
        )(NoPos)
      ))
    )
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
    assertTrue(result.isInstantiated(modName))
    assertFalse(result.isFieldRead(modName, stateField.name))
    assertTrue(result.isFieldWritten(modName, stateField.name))

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

  @Test def replaysVirtualDispatchOnlyForInstantiatedDescendants(): Unit =
    val animalName = className("example.Animal")
    val dogName = className("example.Dog")
    val catName = className("example.Cat")
    val speak = methodName("speak")
    val animal = classDef(
      name    = animalName,
      kind    = PyClassKind.Class,
      methods = List(abstractMethod(speak))
    )
    val dog = classDef(
      name       = dogName,
      superClass = Some(animalName),
      methods    = List(ctor(), method(speak))
    )
    val cat = classDef(
      name       = catName,
      superClass = Some(animalName),
      methods    = List(ctor(), method(speak))
    )
    val user = classDef(
      name    = className("example.User"),
      methods = List(method(
        methodName("use"),
        body = PyBlock(
          List(PyNew(dogName, ctorName(), Nil)(NoPos)),
          PyApply(
            PyApplyFlags.empty,
            PyThis()(PyClassType(animalName), NoPos),
            animalName,
            speak,
            Nil
          )(PyVoidType, NoPos)
        )(NoPos)
      ))
    )
    val result = PyReachability.analyze(
      userClasses    = List(user),
      supportClasses = List(animal, dog, cat),
      mainEntry      = None
    )
    assertTrue(result.isMethodReachable(dogName, speak))
    assertFalse(result.isMethodReachable(catName, speak))

  // -- Runtime-provided classes ----------------------------------------

  @Test def skipsRuntimeProvidedClassReferences(): Unit =
    // Referencing java.lang.String should NOT add it to the result —
    // runtime-provided classes are opaque to the linker/emitter.
    val stringClass = PyClassName.StringClass
    val user = classDef(
      name    = className("example.User"),
      methods = List(method(
        methodName("use"),
        body = PyApplyStatic(
          PyApplyFlags.empty, stringClass, methodName("valueOf"), Nil
        )(PyVoidType, NoPos)
      ))
    )
    val result = PyReachability.analyze(
      userClasses    = List(user),
      supportClasses = Nil,
      mainEntry      = None
    )
    assertFalse(result.isReachable(stringClass))
    assertTrue(result.isReachable(user.name))

  // -- Dunder allow-list ------------------------------------------------

  @Test def dropsUnusedCtorOverloadOnInstantiatedSupportClass(): Unit =
    // After audit finding #12, `instantiate` no longer auto-roots every
    // constructor on an instantiated class — only the no-arg ctor (used
    // by Python's `cls()` / `__init__()` protocol) is kept by default.
    // Other ctor overloads must be reached by an explicit `PyNew` (or
    // `super.<init>` / `this(...)`). A support class with both no-arg
    // and (Int) ctors, instantiated only via the (Int) ctor, should
    // keep BOTH (the (Int) one is reached, the no-arg one is auto-
    // rooted) — but a third unused (String) ctor must be DCE'd.
    val targetName = className("example.Target")
    val intCtor    = ctorName(List(PyPrimRef.IntRef))
    val stringCtor = ctorName(List(PyClassRef(PyClassName.StringClass)))
    val noArgCtor  = ctorName()
    val intParam =
      PyParamDef(PyLocalName("x"), PyOriginalName.NoOriginalName, PyIntType, mutable = false, NoPos)
    val strParam =
      PyParamDef(PyLocalName("s"), PyOriginalName.NoOriginalName, PyStringType, mutable = false, NoPos)
    val target = classDef(
      name    = targetName,
      methods = List(
        ctor(args = List(intParam)),
        ctor(args = List(strParam)),
        ctor()
      )
    )
    val user = classDef(
      name    = className("example.User"),
      methods = List(method(
        methodName("use"),
        body = PyNew(targetName, intCtor, Nil)(NoPos)
      ))
    )
    val result = PyReachability.analyze(
      userClasses    = List(user),
      supportClasses = List(target),
      mainEntry      = None
    )
    assertTrue(result.isInstantiated(targetName))
    assertTrue(
      "ctor reached via PyNew must survive",
      result.isMethodReachable(targetName, intCtor)
    )
    assertTrue(
      "no-arg ctor is auto-rooted on instantiated class",
      result.isMethodReachable(targetName, noArgCtor)
    )
    assertFalse(
      "ctor not reached by any call site must be DCE'd",
      result.isMethodReachable(targetName, stringCtor)
    )

  @Test def keepsAllowedDunderOnInstantiatedClass(): Unit =
    // `__call__` is in the keep-set: any instantiated class that
    // declares one must keep it reachable even though no PyIR call
    // edge exists (Python invokes it implicitly via `obj(args)`).
    val callableName = className("example.Callable")
    val callDunder = methodName("__call__")
    val callable = classDef(
      name    = callableName,
      methods = List(ctor(), method(callDunder))
    )
    val user = classDef(
      name    = className("example.User"),
      methods = List(method(
        methodName("use"),
        body = PyNew(callableName, ctorName(), Nil)(NoPos)
      ))
    )
    val result = PyReachability.analyze(
      userClasses    = List(user),
      supportClasses = List(callable),
      mainEntry      = None
    )
    assertTrue(result.isInstantiated(callableName))
    assertTrue(
      "allowed dunder __call__ must survive on instantiated class",
      result.isMethodReachable(callableName, callDunder)
    )

  @Test def dropsDisallowedDunderOnInstantiatedClass(): Unit =
    // `__class__` is a Python-internal dunder that the backend never
    // emits for user code. It's NOT in `KeptDunders`, so even if a
    // class declares one and has no static call edges, the analyzer
    // must let DCE prune it.
    val pyName = className("example.Bag")
    val internalDunder = methodName("__class__")
    val cls = classDef(
      name    = pyName,
      methods = List(ctor(), method(internalDunder))
    )
    val user = classDef(
      name    = className("example.User"),
      methods = List(method(
        methodName("use"),
        body = PyNew(pyName, ctorName(), Nil)(NoPos)
      ))
    )
    val result = PyReachability.analyze(
      userClasses    = List(user),
      supportClasses = List(cls),
      mainEntry      = None
    )
    assertTrue(result.isInstantiated(pyName))
    assertFalse(
      "non-allow-listed dunder __class__ must NOT survive on instantiated support class",
      result.isMethodReachable(pyName, internalDunder)
    )

  @Test def keptDundersIncludesEveryDunderTheEmitterEmitsLocally(): Unit =
    // The emitter only treats `__eq__` and `__hash__` specially
    // (`methodEmitsAsDunder` in PyIREmitter). `__init__` is the
    // constructor namespace. All three must be in the allow-list so
    // they survive on instantiated classes regardless of PyIR call
    // edges.
    for d <- List("__init__", "__eq__", "__hash__", "__str__", "__call__") do
      assertTrue(
        s"`$d` must be in PyReachability.KeptDunders",
        PyReachability.KeptDunders.contains(d)
      )

  @Test def keptDundersExcludesPythonInternalDunders(): Unit =
    // These dunders are never emitted from Scala source — they're
    // Python's class-protocol hooks. Keeping them would defeat DCE
    // for any Scala class that happens to override them.
    for d <- List("__class__", "__mro__", "__dict__", "__name__",
                  "__getattribute__", "__getattr__", "__setattr__",
                  "__new__", "__slots__") do
      assertFalse(
        s"`$d` must NOT be in PyReachability.KeptDunders",
        PyReachability.KeptDunders.contains(d)
      )

  // -- Transitive interface defaults ------------------------------------

  @Test def resolvesInterfaceDefaultThroughInterfaceOfInterface(): Unit =
    // base interface `IBase` declares `helper` as a default. Sub
    // interface `IExt` extends `IBase` (no method override). Class `C`
    // implements `IExt` only. A virtual call against `IExt.helper`
    // must resolve through the transitive interface chain to
    // `IBase.helper`, which is the only declaration site.
    val baseIfaceName = className("example.IBase")
    val extIfaceName  = className("example.IExt")
    val concreteName  = className("example.C")
    val helper = methodName("helper")
    val baseIface = classDef(
      name    = baseIfaceName,
      kind    = PyClassKind.Interface,
      methods = List(method(helper))
    )
    val extIface = classDef(
      name       = extIfaceName,
      kind       = PyClassKind.Interface,
      interfaces = List(baseIfaceName)
    )
    val concrete = classDef(
      name       = concreteName,
      interfaces = List(extIfaceName),
      methods    = List(ctor())
    )
    // user instantiates the concrete class and makes a virtual call
    // against the extension interface.
    val user = classDef(
      name    = className("example.User"),
      methods = List(method(
        methodName("use"),
        body = PyBlock(
          List(PyNew(concreteName, ctorName(), Nil)(NoPos)),
          PyApply(
            PyApplyFlags.empty,
            PyThis()(PyClassType(extIfaceName), NoPos),
            extIfaceName,
            helper,
            Nil
          )(PyVoidType, NoPos)
        )(NoPos)
      ))
    )
    val result = PyReachability.analyze(
      userClasses    = List(user),
      supportClasses = List(baseIface, extIface, concrete),
      mainEntry      = None
    )
    assertTrue(
      "transitive interface-default lookup must resolve helper to IBase",
      result.isMethodReachable(baseIfaceName, helper)
    )

  // -- Drift checks against PyIRRuntime ---------------------------------

  @Test def preludeCallsAreActuallyReferencedFromPreludeText(): Unit =
    // Every entry in `PyIRRuntime.preludeCalls` should correspond to a
    // call site visible in the prelude text. We approximate by
    // checking the encoded method identifier appears as a substring of
    // `PyIRRuntime.content`. If a future refactor removes the call
    // from the prelude, the entry becomes dead weight (still seeds
    // unused work into reachability) and this test catches the drift.
    val content = PyIRRuntime.content
    for (cls, method) <- PyIRRuntime.preludeCalls do
      val encoded = method.encoded
      assertTrue(
        s"preludeCalls entry ($cls.${method.simple.name}) -> encoded `$encoded` is not referenced anywhere in the prelude string",
        content.contains(encoded)
      )

  @Test def providedClassesAndPreludeRegistrationsAreInSync(): Unit =
    // Every `_scpy_register_class(... "<class.name>" ...)` literal in
    // the prelude must correspond to either:
    //   (a) a `PyIRRuntime.providedClasses` entry, OR
    //   (b) a primitive type registration (kept inside the prelude
    //       so the linker doesn't see them — those have kind="primitive").
    // And every non-primitive `providedClasses` entry must have a
    // matching registration call in the prelude. Catches drift in
    // either direction.
    val content = PyIRRuntime.content
    val registerPattern = """_scpy_register_class\([^,]+,\s*"([^"]+)"""".r
    val registeredNames: Set[String] =
      registerPattern.findAllMatchIn(content).flatMap(m => Option(m.group(1))).toSet
    val providedNames: Set[String] =
      PyIRRuntime.providedClasses.keys.map(_.nameString).toSet

    // Primitive class names show up in registrations only.
    val primitives = Set(
      "void", "boolean", "char", "byte", "short",
      "int", "long", "float", "double",
      // Box / utility names registered in the prelude but kept off the
      // providedClasses list because the bundle defines them.
      "java.lang.Cloneable", "java.lang.Number", "java.lang.Boolean",
      "java.lang.Character", "java.lang.Byte", "java.lang.Short",
      "java.lang.Integer", "java.lang.Long",
      "java.lang.Float", "java.lang.Double",
    )

    val unregistered = providedNames -- registeredNames
    assertTrue(
      s"providedClasses entries with no `_scpy_register_class` in prelude: $unregistered",
      unregistered.isEmpty
    )

    val unprovided = registeredNames -- providedNames -- primitives
    assertTrue(
      s"`_scpy_register_class` calls with no providedClasses entry (and not a known primitive): $unprovided",
      unprovided.isEmpty
    )

  // -- Helpers (cloned from PyLinkerTest) ------------------------------

  private def className(name: String): PyClassName =
    PyClassName(name)

  private def fieldDef(
      owner:     PyClassName,
      name:      String,
      tpe:       PyType,
      namespace: PyMemberNamespace = PyMemberNamespace.Public
  ): PyFieldDef =
    PyFieldDef(
      flags        = PyMemberFlags.empty.withNamespace(namespace),
      name         = PyFieldName(owner, PySimpleFieldName(name)),
      originalName = PyOriginalName.NoOriginalName,
      ftpe         = tpe,
      pos          = NoPos
    )

  private def abstractMethod(
      name:       PyMethodName,
      namespace:  PyMemberNamespace = PyMemberNamespace.Public,
      resultType: PyType            = PyVoidType,
      args:       List[PyParamDef]  = Nil
  ): PyMethodDef =
    PyMethodDef(
      flags        = PyMemberFlags.empty.withNamespace(namespace),
      name         = name,
      originalName = PyOriginalName.NoOriginalName,
      args         = args,
      resultType   = resultType,
      body         = None,
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
