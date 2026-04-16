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
    assertTrue(emitted.contains("class A"))
    assertTrue(emitted.contains("class Main"))
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
    val integerClass = className("java.lang.Integer")
    val host = classDef(
      name = className("example.RuntimeMismatch"),
      methods = List(
        method(
          name = methodName("badRuntimeCall"),
          body = PyApplyStatic(
            PyApplyFlags.empty,
            integerClass,
            methodName("totallyMissingHelper"),
            Nil
          )(PyVoidType, NoPos)
        )
      )
    )

    assertLinkError("Unresolved static method 'java.lang.Integer.totallyMissingHelper():V'") {
      PyLinker.link(List(PyLinker.Input(List(host), None)))
    }

  private def assertLinkError(expectedMessage: String)(body: => Any): Unit =
    assertThrows[PyLinkingException](_.errors.exists(_.message.contains(expectedMessage))) {
      body
    }

  private def className(name: String): PyClassName =
    PyClassName(name)

  private def fieldName(owner: PyClassName, name: String): PyFieldName =
    PyFieldName(owner, PySimpleFieldName(name))

  private def fieldDef(owner: PyClassName, name: String, tpe: PyType): PyFieldDef =
    PyFieldDef(
      flags = PyMemberFlags.empty,
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
