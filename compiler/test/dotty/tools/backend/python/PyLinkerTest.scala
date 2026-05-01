package dotty.tools.backend.python

import dotty.tools.assertThrows
import dotty.tools.backend.python.ir.pyir.*

import org.junit.Assert.*
import org.junit.Test

class PyLinkerTest:

  private val NoPos = PyPosition.NoPosition

  @Test def bundlesMultipleInputsIntoOnePythonBundle(): Unit =
    val classA = classDef(
      name = className("example.A"),
      methods = List(
        ctor(),
        method(
          name = methodName("ping", resultRef = PyPrimRef.IntRef),
          resultType = PyIntType,
          body = PyIntLit(1)(NoPos)
        )
      )
    )

    val moduleMain = classDef(
      name = className("example.Main"),
      kind = PyClassKind.ModuleClass,
      methods = List(
        method(
          name = methodName("main"),
          namespace = PyMemberNamespace.PublicStatic,
          body = PyApply(
            PyApplyFlags.empty,
            PyNew(classA.name, ctorName(), Nil)(NoPos),
            classA.name,
            methodName("ping", resultRef = PyPrimRef.IntRef),
            Nil
          )(PyIntType, NoPos)
        )
      )
    )

    val bundle = PyLinker.link(
      List(
        PyLinker.Input(List(classA), None),
        PyLinker.Input(List(moduleMain), Some((moduleMain.name, moduleMain.kind)))
      )
    )

    val emitted = PyIREmitter.emitToString(bundle.classes, bundle.mainEntry)

    assertEquals(List(classA, moduleMain), bundle.classes)
    // User classes are emitted with mangled FQN identifiers (to avoid
    // simple-name collisions across packages). Runtime-provided classes
    // keep their simple names. See `PyIREmitter.classIdentifier`.
    assertTrue(emitted.contains("class example_A"))
    assertTrue(emitted.contains("class example_Main"))
    assertTrue(emitted.contains("if __name__ == \"__main__\":"))

  @Test def acceptsJavaProvidedObjectCtorAndMethods(): Unit =
    val childName = className("example.Child")

    val child = classDef(
      name = childName,
      methods = List(
        ctor(
          body = PyBlock(
            List(
              PyApplyStatically(
                PyApplyFlags.empty,
                PyThis()(PyClassType(childName), NoPos),
                PyClassName.ObjectClass,
                ctorName(),
                Nil
              )(PyVoidType, NoPos)
            ),
            // Call toString on Object — java-provided, lenient lookup
            PyApply(
              PyApplyFlags.empty,
              PyThis()(PyClassType(childName), NoPos),
              PyClassName.ObjectClass,
              methodName("toString", resultRef = PyClassRef(PyClassName.StringClass)),
              Nil
            )(PyClassType(PyClassName.StringClass), NoPos)
          )(NoPos)
        )
      )
    )

    val bundle = PyLinker.link(List(PyLinker.Input(List(child), None)))
    assertEquals(List(child), bundle.classes)

  @Test def rejectsDuplicateClassesAndMainEntries(): Unit =
    val dupName = className("example.Dup")
    val dup = classDef(name = dupName)
    val mainA = classDef(name = className("example.MainA"), kind = PyClassKind.ModuleClass)
    val mainB = classDef(name = className("example.MainB"), kind = PyClassKind.ModuleClass)

    assertLinkError("Duplicate class 'example.Dup'") {
      PyLinker.link(List(PyLinker.Input(List(dup, dup), None)))
    }

    assertLinkError("Multiple Python main entries") {
      PyLinker.link(
        List(
          PyLinker.Input(List(mainA), Some((mainA.name, mainA.kind))),
          PyLinker.Input(List(mainB), Some((mainB.name, mainB.kind)))
        )
      )
    }

  @Test def rejectsDuplicateFieldsAndMethodsWithinAClass(): Unit =
    val owner = className("example.HasDupMembers")
    val field = fieldDef(owner, "x", PyIntType)
    val meth = method(name = methodName("ping"))

    val cls = classDef(
      name = owner,
      fields = List(field, field),
      methods = List(meth, meth)
    )

    assertLinkError("Duplicate field 'x' in class 'example.HasDupMembers'") {
      PyLinker.link(List(PyLinker.Input(List(cls), None)))
    }

    assertLinkError("Duplicate method 'ping():V' in class 'example.HasDupMembers'") {
      PyLinker.link(List(PyLinker.Input(List(cls), None)))
    }

  @Test def rejectsMissingNominalReferences(): Unit =
    val hostName = className("example.Host")
    val targetName = className("example.Target")
    val missingName = className("example.Missing")
    val missingModule = className("example.MissingModule")

    val target = classDef(
      name = targetName,
      methods = List(ctor())
    )

    val host = classDef(
      name = hostName,
      methods = List(
        method(
          name = methodName("missingModule"),
          body = PyLoadModule(missingModule)(NoPos)
        ),
        method(
          name = methodName("missingInstance"),
          body = PyApply(
            PyApplyFlags.empty,
            PyNew(targetName, ctorName(), Nil)(NoPos),
            targetName,
            methodName("nope"),
            Nil
          )(PyVoidType, NoPos)
        ),
        method(
          name = methodName("missingStatic"),
          body = PyApplyStatic(
            PyApplyFlags.empty,
            targetName,
            methodName("nope"),
            Nil
          )(PyVoidType, NoPos)
        ),
        method(
          name = methodName("missingField"),
          body = PySelect(
            PyNew(targetName, ctorName(), Nil)(NoPos),
            fieldName(targetName, "x")
          )(PyIntType, NoPos)
        ),
        method(
          name = methodName("missingCtor"),
          body = PyNew(
            targetName,
            ctorName(List(PyPrimRef.IntRef)),
            List(PyIntLit(1)(NoPos))
          )(NoPos)
        )
      )
    )

    val badSuper = classDef(
      name = className("example.BadSuper"),
      superClass = Some(missingName)
    )

    assertLinkError("Unresolved superclass 'example.Missing'") {
      PyLinker.link(List(PyLinker.Input(List(badSuper), None)))
    }

    assertLinkError("Unresolved module class 'example.MissingModule'") {
      PyLinker.link(List(PyLinker.Input(List(host, target), None)))
    }

    assertLinkError("Unresolved instance method 'example.Target.nope():V'") {
      PyLinker.link(
        List(
          PyLinker.Input(
            List(
              host.copy(methods = List(host.methods(1))),
              target
            ),
            None
          )
        )
      )
    }

    assertLinkError("Unresolved static method 'example.Target.nope():V'") {
      PyLinker.link(
        List(
          PyLinker.Input(
            List(
              host.copy(methods = List(host.methods(2))),
              target
            ),
            None
          )
        )
      )
    }

    assertLinkError("Unresolved field 'example.Target.x'") {
      PyLinker.link(
        List(
          PyLinker.Input(
            List(
              host.copy(methods = List(host.methods(3))),
              target
            ),
            None
          )
        )
      )
    }

    assertLinkError("Unresolved constructor 'example.Target.<init>(I):V'") {
      PyLinker.link(
        List(
          PyLinker.Input(
            List(
              host.copy(methods = List(host.methods(4))),
              target
            ),
            None
          )
        )
      )
    }

  @Test def rejectsUnprovidedRuntimeMembers(): Unit =
    // Static dispatch against Java-provided classes is strict: the
    // method must match the whitelist. Calling a name not in the
    // matcher must fail.
    //
    // `java.lang.String` is runtime-provided with no static-method
    // matcher, so any static call on it must fail at link time.
    val runtimeClass = className("java.lang.String")
    val host = classDef(
      name = className("example.RuntimeMismatch"),
      methods = List(
        method(
          name = methodName("badRuntimeCall"),
          body = PyApplyStatic(
            PyApplyFlags.empty,
            runtimeClass,
            methodName("totallyMissingHelper"),
            Nil
          )(PyVoidType, NoPos)
        )
      )
    )

    assertLinkError("Unresolved static method 'java.lang.String.totallyMissingHelper():V'") {
      PyLinker.link(List(PyLinker.Input(List(host), None)))
    }

  @Test def prunesUntouchedSupportFieldsButKeepsUserFields(): Unit =
    val supportName = className("example.Support")
    val live = fieldDef(supportName, "live", PyIntType, PyMemberNamespace.PublicStatic)
    val dead = fieldDef(supportName, "dead", PyIntType, PyMemberNamespace.PublicStatic)
    val support = classDef(
      name   = supportName,
      fields = List(live, dead)
    )
    val userName = className("example.User")
    val userLive = fieldDef(userName, "userLive", PyIntType)
    val userDead = fieldDef(userName, "userDead", PyIntType)
    val user = classDef(
      name   = userName,
      fields = List(userLive, userDead),
      methods = List(method(
        methodName("use"),
        body = PySelectStatic(live.name)(PyIntType, NoPos)
      ))
    )

    val bundle = PyLinker.link(List(
      PyLinker.Input(List(user), None, PyLinker.InputSource.User),
      PyLinker.Input(List(support), None, PyLinker.InputSource.Support)
    ))

    assertEquals(List(live.name), fieldsOf(bundle, supportName))
    assertEquals(List(userLive.name, userDead.name), fieldsOf(bundle, userName))

  @Test def keepsTouchedInstanceFieldOnInstantiatedSupportClass(): Unit =
    val supportName = className("example.Support")
    val live = fieldDef(supportName, "live", PyIntType)
    val dead = fieldDef(supportName, "dead", PyIntType)
    val support = classDef(
      name    = supportName,
      fields  = List(live, dead),
      methods = List(ctor())
    )
    val user = classDef(
      name    = className("example.User"),
      methods = List(method(
        methodName("use"),
        body = PySelect(
          PyNew(supportName, ctorName(), Nil)(NoPos),
          live.name
        )(PyIntType, NoPos)
      ))
    )

    val bundle = PyLinker.link(List(
      PyLinker.Input(List(user), None, PyLinker.InputSource.User),
      PyLinker.Input(List(support), None, PyLinker.InputSource.Support)
    ))

    assertEquals(List(live.name), fieldsOf(bundle, supportName))

  @Test def keepsTouchedSuperclassFieldWhenOnlySubclassIsInstantiated(): Unit =
    val baseName = className("example.Base")
    val childName = className("example.Child")
    val inherited = fieldDef(baseName, "inherited", PyIntType)
    val base = classDef(
      name   = baseName,
      fields = List(inherited)
    )
    val child = classDef(
      name       = childName,
      superClass = Some(baseName),
      methods = List(ctor(
        body = PyAssign(
          PySelect(
            PyThis()(PyClassType(childName), NoPos),
            inherited.name
          )(PyIntType, NoPos),
          PyIntLit(1)(NoPos)
        )(NoPos)
      ))
    )
    val user = classDef(
      name    = className("example.User"),
      methods = List(method(
        methodName("use"),
        body = PyNew(childName, ctorName(), Nil)(NoPos)
      ))
    )

    val bundle = PyLinker.link(List(
      PyLinker.Input(List(user), None, PyLinker.InputSource.User),
      PyLinker.Input(List(base, child), None, PyLinker.InputSource.Support)
    ))

    assertEquals(List(inherited.name), fieldsOf(bundle, baseName))

  @Test def prunesInstanceFieldsOnReachableButUninstantiatedSupportClass(): Unit =
    val supportName = className("example.Support")
    val dead = fieldDef(supportName, "dead", PyIntType)
    val support = classDef(
      name   = supportName,
      fields = List(dead)
    )
    val user = classDef(
      name    = className("example.User"),
      methods = List(method(
        methodName("use"),
        body = PyClassOf(PyClassRef(supportName))(NoPos)
      ))
    )

    val bundle = PyLinker.link(List(
      PyLinker.Input(List(user), None, PyLinker.InputSource.User),
      PyLinker.Input(List(support), None, PyLinker.InputSource.Support)
    ))

    assertEquals(Nil, fieldsOf(bundle, supportName))

  @Test def prunedModuleFieldDoesNotEmitDefaultNoneInitializer(): Unit =
    val modName = className("example.Mod")
    val live = fieldDef(modName, "live", PyIntType)
    val dead = fieldDef(modName, "dead", PyIntType)
    val mod = classDef(
      name   = modName,
      kind   = PyClassKind.ModuleClass,
      fields = List(live, dead),
      methods = List(ctor(
        body = PyAssign(
          PySelect(
            PyThis()(PyClassType(modName), NoPos),
            live.name
          )(PyIntType, NoPos),
          PyIntLit(1)(NoPos)
        )(NoPos)
      ))
    )
    val user = classDef(
      name    = className("example.User"),
      methods = List(method(
        methodName("use"),
        body = PySelect(
          PyLoadModule(modName)(NoPos),
          live.name
        )(PyIntType, NoPos)
      ))
    )

    val bundle = PyLinker.link(List(
      PyLinker.Input(List(user), None, PyLinker.InputSource.User),
      PyLinker.Input(List(mod), None, PyLinker.InputSource.Support)
    ))
    val emitted = PyIREmitter.emitToString(bundle.classes, bundle.mainEntry)

    assertTrue(emitted.contains("live = None"))
    assertFalse(emitted.contains("dead = None"))

  @Test def prunesUnreadModuleCtorStoreAndItsModuleLoadRhs(): Unit =
    val (userInput, supportInput, modName, deadName) = deadModuleStoreFixture()

    val bundle = PyLinker.link(List(
      userInput,
      supportInput
    ))

    assertFalse(classNamesOf(bundle).contains(deadName))
    assertEquals(Nil, fieldsOf(bundle, modName))

  @Test def reachabilityFixpointRepeatsUntilReachabilitySetConverges(): Unit =
    val (userInput, supportInput, modName, deadName) = deadModuleStoreFixture()

    val bundle = PyLinker.link(
      List(userInput),
      List(supportInput),
      maxReachabilityIterations = 2
    )

    assertFalse(classNamesOf(bundle).contains(deadName))
    assertEquals(Nil, fieldsOf(bundle, modName))

  @Test def reachabilityFixpointReportsErrorWhenIterationLimitIsExhausted(): Unit =
    val (userInput, supportInput, _, _) = deadModuleStoreFixture()

    assertLinkError("Reachability DCE did not converge after 1 iterations") {
      PyLinker.link(
        List(userInput),
        List(supportInput),
        maxReachabilityIterations = 1
      )
    }

  @Test def keepsModuleCtorStoreWhenFieldIsRead(): Unit =
    val liveName = className("example.LiveModule")
    val liveModule = classDef(
      name    = liveName,
      kind    = PyClassKind.ModuleClass,
      methods = List(ctor())
    )
    val modName = className("example.Mod")
    val liveField = fieldDef(modName, "live", PyClassType(liveName))
    val mod = classDef(
      name   = modName,
      kind   = PyClassKind.ModuleClass,
      fields = List(liveField),
      methods = List(ctor(
        body = PyAssign(
          PySelect(
            PyThis()(PyClassType(modName), NoPos),
            liveField.name
          )(PyClassType(liveName), NoPos),
          PyLoadModule(liveName)(NoPos)
        )(NoPos)
      ))
    )
    val user = classDef(
      name    = className("example.User"),
      methods = List(method(
        methodName("use"),
        body = PySelect(
          PyLoadModule(modName)(NoPos),
          liveField.name
        )(PyClassType(liveName), NoPos)
      ))
    )

    val bundle = PyLinker.link(List(
      PyLinker.Input(List(user), None, PyLinker.InputSource.User),
      PyLinker.Input(List(mod, liveModule), None, PyLinker.InputSource.Support)
    ))

    assertTrue(classNamesOf(bundle).contains(liveName))
    assertEquals(List(liveField.name), fieldsOf(bundle, modName))

  @Test def keepsModuleCtorStoreWhenLinkedClassFieldAliasIsRead(): Unit =
    val liveName = className("example.LiveModule")
    val liveModule = classDef(
      name    = liveName,
      kind    = PyClassKind.ModuleClass,
      methods = List(ctor())
    )
    val linkedName = className("example.Linked")
    val modName = className("example.Linked_")
    val linkedField = fieldDef(linkedName, "constant", PyClassType(liveName), PyMemberNamespace.PublicStatic)
    val moduleField = fieldDef(modName, "constant", PyClassType(liveName))
    val linked = classDef(
      name   = linkedName,
      fields = List(linkedField)
    )
    val mod = classDef(
      name   = modName,
      kind   = PyClassKind.ModuleClass,
      fields = List(moduleField),
      methods = List(ctor(
        body = PyAssign(
          PySelect(
            PyThis()(PyClassType(modName), NoPos),
            moduleField.name
          )(PyClassType(liveName), NoPos),
          PyLoadModule(liveName)(NoPos)
        )(NoPos)
      ))
    )
    val user = classDef(
      name    = className("example.User"),
      methods = List(method(
        methodName("use"),
        body = PyBlock(
          List(PyLoadModule(modName)(NoPos)),
          PySelectStatic(linkedField.name)(PyClassType(liveName), NoPos)
        )(NoPos)
      ))
    )

    val bundle = PyLinker.link(List(
      PyLinker.Input(List(user), None, PyLinker.InputSource.User),
      PyLinker.Input(List(linked, mod, liveModule), None, PyLinker.InputSource.Support)
    ))

    assertTrue(classNamesOf(bundle).contains(liveName))
    assertEquals(List(moduleField.name), fieldsOf(bundle, modName))

  @Test def prunesUnreadStaticCtorStoreAndItsModuleLoadRhs(): Unit =
    val deadName = className("example.DeadModule")
    val deadModule = classDef(
      name    = deadName,
      kind    = PyClassKind.ModuleClass,
      methods = List(ctor())
    )
    val holderName = className("example.Holder")
    val deadField = fieldDef(holderName, "dead", PyClassType(deadName), PyMemberNamespace.PublicStatic)
    val holder = classDef(
      name   = holderName,
      fields = List(deadField),
      methods = List(staticCtor(
        body = PyAssign(
          PySelectStatic(deadField.name)(PyClassType(deadName), NoPos),
          PyLoadModule(deadName)(NoPos)
        )(NoPos)
      ))
    )
    val user = classDef(
      name    = className("example.User"),
      methods = List(method(
        methodName("use"),
        body = PyClassOf(PyClassRef(holderName))(NoPos)
      ))
    )

    val bundle = PyLinker.link(List(
      PyLinker.Input(List(user), None, PyLinker.InputSource.User),
      PyLinker.Input(List(holder, deadModule), None, PyLinker.InputSource.Support)
    ))

    assertFalse(classNamesOf(bundle).contains(deadName))
    assertEquals(Nil, fieldsOf(bundle, holderName))

  @Test def preservesEffectfulRhsWhenPruningUnreadModuleCtorStore(): Unit =
    val effectName = className("example.Effect")
    val touchName = methodName("touch")
    val effect = classDef(
      name    = effectName,
      methods = List(method(touchName, namespace = PyMemberNamespace.PublicStatic))
    )
    val modName = className("example.Mod")
    val deadField = fieldDef(modName, "dead", PyIntType)
    val mod = classDef(
      name   = modName,
      kind   = PyClassKind.ModuleClass,
      fields = List(deadField),
      methods = List(ctor(
        body = PyAssign(
          PySelect(
            PyThis()(PyClassType(modName), NoPos),
            deadField.name
          )(PyIntType, NoPos),
          PyApplyStatic(
            PyApplyFlags.empty,
            effectName,
            touchName,
            Nil
          )(PyVoidType, NoPos)
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

    val bundle = PyLinker.link(List(
      PyLinker.Input(List(user), None, PyLinker.InputSource.User),
      PyLinker.Input(List(mod, effect), None, PyLinker.InputSource.Support)
    ))

    assertTrue(classNamesOf(bundle).contains(effectName))
    assertEquals(Nil, fieldsOf(bundle, modName))

  @Test def preservesCrossOwnerModuleCtorStore(): Unit =
    val loadedName = className("example.Loaded")
    val loaded = classDef(
      name    = loadedName,
      kind    = PyClassKind.ModuleClass,
      methods = List(ctor())
    )
    val linkedName = className("example.Linked")
    val linkedField = fieldDef(linkedName, "constant", PyClassType(loadedName))
    val linked = classDef(
      name   = linkedName,
      fields = List(linkedField)
    )
    val modName = className("example.Linked_")
    val mod = classDef(
      name    = modName,
      kind    = PyClassKind.ModuleClass,
      methods = List(ctor(
        body = PyAssign(
          PySelect(
            PyThis()(PyClassType(modName), NoPos),
            linkedField.name
          )(PyClassType(loadedName), NoPos),
          PyLoadModule(loadedName)(NoPos)
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

    val bundle = PyLinker.link(List(
      PyLinker.Input(List(user), None, PyLinker.InputSource.User),
      PyLinker.Input(List(linked, mod, loaded), None, PyLinker.InputSource.Support)
    ))

    assertTrue(classNamesOf(bundle).contains(loadedName))
    assertEquals(List(linkedField.name), fieldsOf(bundle, linkedName))

  @Test def preservesUserModuleCtorStoreAndItsModuleLoadRhs(): Unit =
    val loadedName = className("example.Loaded")
    val loaded = classDef(
      name    = loadedName,
      kind    = PyClassKind.ModuleClass,
      methods = List(ctor())
    )
    val userModName = className("example.UserMod")
    val field = fieldDef(userModName, "kept", PyClassType(loadedName))
    val userMod = classDef(
      name   = userModName,
      kind   = PyClassKind.ModuleClass,
      fields = List(field),
      methods = List(ctor(
        body = PyAssign(
          PySelect(
            PyThis()(PyClassType(userModName), NoPos),
            field.name
          )(PyClassType(loadedName), NoPos),
          PyLoadModule(loadedName)(NoPos)
        )(NoPos)
      ))
    )

    val bundle = PyLinker.link(List(
      PyLinker.Input(List(userMod), None, PyLinker.InputSource.User),
      PyLinker.Input(List(loaded), None, PyLinker.InputSource.Support)
    ))

    assertTrue(classNamesOf(bundle).contains(loadedName))
    assertEquals(List(field.name), fieldsOf(bundle, userModName))

  @Test def dropsSupportMainWhenUserHasNoMain(): Unit =
    // User CU has only a non-main method; a Support input ships its own
    // `main` (analogous to scala.util.Properties.main). The linker must
    // NOT promote the Support main to the bundle entry — doing so would
    // root the support graph in PyReachability and bloat the output by
    // an order of magnitude.
    val userCls = classDef(
      name    = className("user.Lib"),
      methods = List(
        ctor(),
        method(
          name       = methodName(
            "adder",
            paramRefs = List(PyPrimRef.IntRef, PyPrimRef.IntRef),
            resultRef = PyPrimRef.IntRef
          ),
          resultType = PyIntType,
          body       = PyIntLit(0)(NoPos)
        )
      )
    )

    val supportMain = classDef(
      name    = className("support.Properties"),
      kind    = PyClassKind.ModuleClass,
      methods = List(
        ctor(),
        method(
          name      = methodName("main"),
          namespace = PyMemberNamespace.PublicStatic,
          body      = PySkip()(NoPos)
        )
      )
    )

    val bundle = PyLinker.link(List(
      PyLinker.Input(List(userCls), None),
      PyLinker.Input(
        List(supportMain),
        Some((supportMain.name, supportMain.kind)),
        PyLinker.InputSource.Support
      )
    ))

    assertEquals(None, bundle.mainEntry)
    assertTrue(
      "support.Properties should be pruned when its main is not promoted",
      !classNamesOf(bundle).contains(supportMain.name)
    )
    assertTrue(
      "user.Lib should be preserved as a User root",
      classNamesOf(bundle).contains(userCls.name)
    )

    val emitted = PyIREmitter.emitToString(bundle.classes, bundle.mainEntry)
    assertTrue(!emitted.contains("if __name__ == \"__main__\":"))

  @Test def userMainWinsOverSupportMain(): Unit =
    val userMain = classDef(
      name    = className("user.App"),
      kind    = PyClassKind.ModuleClass,
      methods = List(
        ctor(),
        method(
          name      = methodName("main"),
          namespace = PyMemberNamespace.PublicStatic,
          body      = PySkip()(NoPos)
        )
      )
    )
    val supportMain = classDef(
      name    = className("support.Lib"),
      kind    = PyClassKind.ModuleClass,
      methods = List(
        ctor(),
        method(
          name      = methodName("main"),
          namespace = PyMemberNamespace.PublicStatic,
          body      = PySkip()(NoPos)
        )
      )
    )

    val bundle = PyLinker.link(List(
      PyLinker.Input(List(userMain), Some((userMain.name, userMain.kind))),
      PyLinker.Input(
        List(supportMain),
        Some((supportMain.name, supportMain.kind)),
        PyLinker.InputSource.Support
      )
    ))

    assertEquals(Some((userMain.name, userMain.kind)), bundle.mainEntry)

  private def assertLinkError(expectedMessage: String)(body: => Any): Unit =
    assertThrows[PyLinkingException](_.errors.exists(_.message.contains(expectedMessage))) {
      body
    }

  private def classNamesOf(bundle: PyLinker.LinkedBundle): Set[PyClassName] =
    bundle.classes.iterator.map(_.name).toSet

  private def fieldsOf(bundle: PyLinker.LinkedBundle, name: PyClassName): List[PyFieldName] =
    bundle.classes.find(_.name == name).toList.flatMap(_.fields.map(_.name))

  private def deadModuleStoreFixture(): (
      PyLinker.Input,
      PyLinker.Input,
      PyClassName,
      PyClassName
  ) =
    val deadName = className("example.DeadModule")
    val deadModule = classDef(
      name    = deadName,
      kind    = PyClassKind.ModuleClass,
      methods = List(ctor())
    )
    val modName = className("example.Mod")
    val deadField = fieldDef(modName, "dead", PyClassType(deadName))
    val mod = classDef(
      name   = modName,
      kind   = PyClassKind.ModuleClass,
      fields = List(deadField),
      methods = List(ctor(
        body = PyAssign(
          PySelect(
            PyThis()(PyClassType(modName), NoPos),
            deadField.name
          )(PyClassType(deadName), NoPos),
          PyLoadModule(deadName)(NoPos)
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
    (
      PyLinker.Input(List(user), None, PyLinker.InputSource.User),
      PyLinker.Input(List(mod, deadModule), None, PyLinker.InputSource.Support),
      modName,
      deadName
    )

  private def className(name: String): PyClassName =
    PyClassName(name)

  private def fieldName(owner: PyClassName, name: String): PyFieldName =
    PyFieldName(owner, PySimpleFieldName(name))

  private def fieldDef(
      owner: PyClassName,
      name: String,
      tpe: PyType,
      namespace: PyMemberNamespace = PyMemberNamespace.Public
  ): PyFieldDef =
    PyFieldDef(
      flags = PyMemberFlags.empty.withNamespace(namespace),
      name = fieldName(owner, name),
      originalName = PyOriginalName.NoOriginalName,
      ftpe = tpe,
      pos = NoPos
    )

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

  private def staticCtor(body: PyTree = PySkip()(NoPos)): PyMethodDef =
    method(
      name = PyMethodName(PySimpleMethodName.StaticInit, Nil, PyPrimRef.VoidRef),
      namespace = PyMemberNamespace.StaticConstructor,
      body = body
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
