package dotty.tools.backend.python

import dotty.tools.backend.python.ir.pyir.*

import java.io.PrintWriter

import scala.collection.mutable

final case class PyLinkingError(message: String, pos: PyPosition)

final class PyLinkingException(val errors: List[PyLinkingError])
    extends Exception(
      errors.map { err =>
        if err.pos.isDefined then
          s"${err.pos.source}:${err.pos.line + 1}:${err.pos.column + 1}: ${err.message}"
        else err.message
      }.mkString("\n")
    )

/** Strict linker for the Python backend.
 *
 *  Validates that every nominal reference in the input `PyIR` bundle is
 *  satisfied either by another bundled class or by the fixed runtime
 *  contract in [[PyIRRuntime]]. On success, returns the classes in
 *  stable input order so the emitter can bundle them into one Python
 *  file.
 */
object PyLinker:

  /** Provenance of a link input.
   *
   *  `User` — a `.pyir` produced by the compilation unit(s) currently
   *  being compiled (written to the output directory). These classes are
   *  unconditionally preserved in the emitted bundle.
   *
   *  `Support` — a `.pyir` sourced from the classpath (stdlib/library
   *  jars or directories). These classes are subject to link-time
   *  reachability filtering (V1: class-level DCE).
   */
  enum InputSource:
    case User, Support

  final case class Input(
      classes:   List[PyClassDef],
      mainEntry: Option[PyIREmitter.MainEntry],
      source:    InputSource = InputSource.User
  )

  final case class LinkedBundle(
      classes: List[PyClassDef],
      mainEntry: Option[PyIREmitter.MainEntry]
  )

  def link(userInputs: List[Input], supportInputs: List[Input]): LinkedBundle =
    new Linker(userInputs, supportInputs).link()

  /** Convenience overload for callers that don't distinguish sources
   *  (e.g. unit tests). Inputs are partitioned by their `source` field. */
  def link(inputs: List[Input]): LinkedBundle =
    val (userInputs, supportInputs) = inputs.partition(_.source == InputSource.User)
    link(userInputs, supportInputs)

  def linkAndEmit(userInputs: List[Input], supportInputs: List[Input], out: PrintWriter): Unit =
    val bundle = link(userInputs, supportInputs)
    PyIREmitter.emit(bundle.classes, bundle.mainEntry, out)

  def linkAndEmit(inputs: List[Input], out: PrintWriter): Unit =
    val bundle = link(inputs)
    PyIREmitter.emit(bundle.classes, bundle.mainEntry, out)

  private final class Linker(userInputs: List[Input], supportInputs: List[Input]):
    private val inputs = userInputs ++ supportInputs
    private val errors = mutable.ListBuffer.empty[PyLinkingError]

    private final case class ClassInfo(
        kind:        PyClassKind,
        superClass:  Option[PyClassName],
        interfaces:  List[PyClassName],
        fieldsByName: Map[PyFieldName, PyFieldDef],
        methodsByName: Map[PyMethodName, PyMethodDef],
        runtime:     Option[PyIRRuntime.ProvidedClass]
    ):
      def hasField(field: PyFieldName): Boolean =
        fieldsByName.contains(field) || runtime.exists(_.hasField(field))

      def hasInstanceMethod(method: PyMethodName): Boolean =
        methodsByName.get(method).exists(isInstanceMethod) || runtime.exists(_.hasInstanceMethod(method))

      def hasExactInstanceMethod(method: PyMethodName): Boolean =
        methodsByName.get(method).exists(isInstanceLike) || runtime.exists { runtimeClass =>
          runtimeClass.hasInstanceMethod(method) || runtimeClass.hasConstructor(method)
        }

      def hasStaticMethod(method: PyMethodName): Boolean =
        methodsByName.get(method).exists(isStaticMethod) || runtime.exists(_.hasStaticMethod(method))

      def hasConstructor(method: PyMethodName): Boolean =
        methodsByName.get(method).exists(_.flags.namespace == PyMemberNamespace.Constructor) ||
          runtime.exists(_.hasConstructor(method))

    def link(): LinkedBundle =
      val allClasses = collectClasses()
      val mainEntry  = collectMainEntry()

      // Class-level reachability DCE. User classes (CU-local `.pyir`) are
      // preserved verbatim; support classes (classpath `.pyir`) are
      // kept only if transitively referenced from a user class, the main
      // entry, or another kept class. See `PyReachability`.
      val userClassNames =
        userInputs.iterator.flatMap(_.classes).map(_.name).toSet
      val (userClasses, supportClasses) =
        allClasses.partition(c => userClassNames.contains(c.name))
      val reach = PyReachability.analyze(userClasses, supportClasses, mainEntry)
      val keptClasses = applyReachability(allClasses, reach)

      val classInfos = buildClassInfos(keptClasses)
      keptClasses.foreach(validateClass(_, classInfos))
      mainEntry.foreach(validateMainEntry(_, classInfos))

      if errors.nonEmpty then
        throw new PyLinkingException(errors.toList)

      // Python evaluates class statements top-to-bottom and a `class Foo(Bar)`
      // form requires `Bar` to already be defined. Re-order the bundled
      // classes so a class always appears after its superclass (when both
      // are bundled), preserving input order otherwise.
      LinkedBundle(orderForEmission(keptClasses), mainEntry)

    // V1 filter hook: class-level only. V2 will also map over each
    // kept class to prune unreachable methods/fields.
    private def applyReachability(
        classes: List[PyClassDef],
        reach:   PyReachability.Result
    ): List[PyClassDef] =
      classes.filter(c => reach.isReachable(c.name))

    private def orderForEmission(classes: List[PyClassDef]): List[PyClassDef] =
      val byName = classes.iterator.map(c => c.name -> c).toMap
      val visited = mutable.HashSet.empty[PyClassName]
      val ordered = mutable.ListBuffer.empty[PyClassDef]
      def visit(cls: PyClassDef): Unit =
        if visited.add(cls.name) then
          cls.superClass.flatMap(byName.get).foreach(visit)
          ordered += cls
      classes.foreach(visit)
      ordered.toList

    private def collectClasses(): List[PyClassDef] =
      val classDefs = mutable.LinkedHashMap.empty[PyClassName, PyClassDef]

      for
        input <- inputs
        cls <- input.classes
      do
        if PyIRRuntime.providedClass(cls.name).isDefined then
          // User-input collision is a hard error — a user shouldn't be
          // redefining `scala.Function0`, `scala.deriving.Mirror`, etc.
          // Support-input collision is silently ignored: the runtime bakes
          // those classes into the Python bundle (see `PyIRRuntime.prelude`),
          // and stdlib's compiled `.pyir` for them would duplicate that
          // hand-written baseline. Prefer the runtime version.
          if input.source == InputSource.User then
            error(s"Class '${cls.name.nameString}' collides with a runtime-provided class", cls.pos)
        else
          classDefs.get(cls.name) match
            case Some(_) =>
              error(s"Duplicate class '${cls.name.nameString}'", cls.pos)
            case None =>
              classDefs += cls.name -> cls

      classDefs.values.toList

    private def buildClassInfos(classes: List[PyClassDef]): Map[PyClassName, ClassInfo] =
      val bundledInfos =
        classes.iterator.map { cls =>
          checkDuplicateFields(cls)
          checkDuplicateMethods(cls)

          cls.name -> ClassInfo(
            kind = cls.kind,
            superClass = cls.superClass,
            interfaces = cls.interfaces,
            fieldsByName = collectFirsts(cls.fields)(_.name),
            methodsByName = collectFirsts(cls.methods)(_.name),
            runtime = None
          )
        }.toMap

      bundledInfos ++ PyIRRuntime.providedClasses.iterator.collect {
        case (name, runtimeInfo) if !bundledInfos.contains(name) =>
          name -> ClassInfo(
            kind = runtimeInfo.kind,
            superClass = runtimeInfo.superClass,
            interfaces = runtimeInfo.interfaces,
            fieldsByName = Map.empty,
            methodsByName = Map.empty,
            runtime = Some(runtimeInfo)
          )
      }

    private def collectMainEntry(): Option[PyIREmitter.MainEntry] =
      // Prefer a User input's main over any Support main. Stdlib code
      // (e.g. `scala.util.Properties` carrying a version-printer `main`)
      // ships its own entry, but we want the user's entry point — same
      // policy as scalac/JVM where the bootstrap picks the explicit
      // -Dmain. Multiple User mains are still a hard error.
      val userMains = userInputs.iterator.flatMap(_.mainEntry).toList
      val supportMains = supportInputs.iterator.flatMap(_.mainEntry).toList
      userMains match
        case Nil => supportMains.headOption
        case head :: Nil => Some(head)
        case head :: rest =>
          for other <- rest do
            error(
              s"Multiple Python main entries: '${head._1.nameString}' and '${other._1.nameString}'",
              PyPosition.NoPosition
            )
          Some(head)

    private def checkDuplicateFields(cls: PyClassDef): Unit =
      val seen = mutable.HashSet.empty[PyFieldName]
      for field <- cls.fields do
        if field.name.owner != cls.name then
          error(
            s"Field '${field.name.simple.name}' is owned by '${field.name.owner.nameString}' but declared in '${cls.name.nameString}'",
            field.pos
          )
        if !seen.add(field.name) then
          error(
            s"Duplicate field '${field.name.simple.name}' in class '${cls.name.nameString}'",
            field.pos
          )

    private def checkDuplicateMethods(cls: PyClassDef): Unit =
      val seen = mutable.HashSet.empty[PyMethodName]
      for method <- cls.methods do
        if !seen.add(method.name) then
          error(
            s"Duplicate method '${showMethod(method.name)}' in class '${cls.name.nameString}'",
            method.pos
          )

    private def validateMainEntry(
        mainEntry: PyIREmitter.MainEntry,
        classInfos: Map[PyClassName, ClassInfo]
    ): Unit =
      val (className, expectedKind) = mainEntry
      classInfos.get(className) match
        case None =>
          error(s"Main entry class '${className.nameString}' is not provided", PyPosition.NoPosition)
        case Some(info) if info.kind != expectedKind =>
          error(
            s"Main entry kind mismatch for '${className.nameString}': expected $expectedKind but found ${info.kind}",
            PyPosition.NoPosition
          )
        case _ =>
          ()

    private def validateClass(cls: PyClassDef, classInfos: Map[PyClassName, ClassInfo]): Unit =
      cls.superClass.foreach { superClass =>
        requireClass(superClass, cls.pos, classInfos, s"superclass '${superClass.nameString}'")
      }
      // Interfaces are intentionally NOT validated here. Python is
      // duck-typed and the emitter does not turn interfaces into Python
      // base classes, so a missing nominal trait does not affect runtime
      // behavior. Bridging this softness keeps user code that mixes in
      // synthetic stdlib traits (Mirror, Equals, Product, Serializable)
      // from failing the linker.

      cls.fields.foreach(validateField(_, classInfos))
      cls.methods.foreach(validateMethod(_, classInfos))

    private def validateField(field: PyFieldDef, classInfos: Map[PyClassName, ClassInfo]): Unit =
      validateType(field.ftpe, field.pos, classInfos)

    private def validateMethod(method: PyMethodDef, classInfos: Map[PyClassName, ClassInfo]): Unit =
      validateMethodName(method.name, method.pos, classInfos)
      method.args.foreach(validateParam(_, classInfos))
      validateType(method.resultType, method.pos, classInfos)
      method.body.foreach(validateTree(_, classInfos))

    private def validateParam(param: PyParamDef, classInfos: Map[PyClassName, ClassInfo]): Unit =
      validateType(param.ptpe, param.pos, classInfos)

    private def validateTree(tree: PyTree, classInfos: Map[PyClassName, ClassInfo]): Unit =
      tree match
        case tree: PyVarDef =>
          validateType(tree.vtpe, tree.pos, classInfos)
          validateTree(tree.rhs, classInfos)

        case tree: PyAssign =>
          validateTree(tree.lhs, classInfos)
          validateTree(tree.rhs, classInfos)

        case tree: PyReturn =>
          validateTree(tree.value, classInfos)

        case tree: PyWhile =>
          validateTree(tree.cond, classInfos)
          validateTree(tree.body, classInfos)

        case tree: PyForEach =>
          validateTree(tree.iterable, classInfos)
          validateTree(tree.body, classInfos)

        case _: PySkip =>
          ()

        case tree: PyIf =>
          validateTree(tree.cond, classInfos)
          validateTree(tree.thenp, classInfos)
          validateTree(tree.elsep, classInfos)

        case tree: PyTryCatch =>
          validateTree(tree.block, classInfos)
          validateTree(tree.handler, classInfos)

        case tree: PyTryFinally =>
          validateTree(tree.block, classInfos)
          validateTree(tree.finalizer, classInfos)

        case tree: PyMatch =>
          validateTree(tree.selector, classInfos)
          tree.cases.foreach { case (_, body) => validateTree(body, classInfos) }
          validateTree(tree.default, classInfos)

        case tree: PyBlock =>
          tree.stats.foreach(validateTree(_, classInfos))
          validateTree(tree.expr, classInfos)

        case tree: PyLabeled =>
          validateTree(tree.body, classInfos)

        case tree: PyLabelReturn =>
          validateTree(tree.value, classInfos)

        case tree: PyVarRef =>
          validateType(tree.tpe, tree.pos, classInfos)

        case tree: PyThis =>
          validateType(tree.tpe, tree.pos, classInfos)

        case tree: PySelect =>
          validateTree(tree.qualifier, classInfos)
          requireField(tree.field, tree.pos, classInfos)

        case tree: PySelectStatic =>
          requireField(tree.field, tree.pos, classInfos)

        case tree: PyApply =>
          validateTree(tree.receiver, classInfos)
          tree.args.foreach(validateTree(_, classInfos))
          validateMethodName(tree.method, tree.pos, classInfos)
          if tree.method.simple.isConstructor then
            requireConstructor(tree.className, tree.method, tree.pos, classInfos)
          else
            requireInstanceMethod(tree.className, tree.method, tree.pos, classInfos)

        case tree: PyApplyStatically =>
          validateTree(tree.receiver, classInfos)
          tree.args.foreach(validateTree(_, classInfos))
          validateMethodName(tree.method, tree.pos, classInfos)
          if tree.method.simple.isConstructor then
            requireConstructor(tree.className, tree.method, tree.pos, classInfos)
          else
            requireExactInstanceMethod(tree.className, tree.method, tree.pos, classInfos)

        case tree: PyApplyStatic =>
          tree.args.foreach(validateTree(_, classInfos))
          validateMethodName(tree.method, tree.pos, classInfos)
          requireStaticMethod(tree.className, tree.method, tree.pos, classInfos)

        case tree: PyApplyExternal =>
          tree.args.foreach(validateTree(_, classInfos))
          validateType(tree.tpe, tree.pos, classInfos)

        case tree: PyExternalRef =>
          validateType(tree.tpe, tree.pos, classInfos)

        case tree: PyApplyDynamic =>
          validateTree(tree.callee, classInfos)
          tree.args.foreach(validateTree(_, classInfos))
          tree.kwargs.foreach((_, value) => validateTree(value, classInfos))
          validateType(tree.tpe, tree.pos, classInfos)

        case tree: PyAttrAccess =>
          validateTree(tree.obj, classInfos)
          validateType(tree.tpe, tree.pos, classInfos)

        case tree: PyNew =>
          tree.args.foreach(validateTree(_, classInfos))
          validateMethodName(tree.ctor, tree.pos, classInfos)
          requireConstructor(tree.className, tree.ctor, tree.pos, classInfos)

        case tree: PyLoadModule =>
          classInfos.get(tree.className) match
            case None =>
              error(s"Unresolved module class '${tree.className.nameString}'", tree.pos)
            case Some(_) =>
              // Accept any kind. In Python a `class` object IS the thing
              // you "load" — both `Boolean_` (Scala module) and `Boolean`
              // (the class carrying static forwarders for `TYPE` etc.)
              // are valid targets of `PyLoadModule`. Field access
              // (`PyLoadModule(Boolean) . TYPE`) then resolves against
              // the static field forwarder.
              ()

        case tree: PyIsInstanceOf =>
          validateTree(tree.expr, classInfos)
          validateTypeRef(tree.testType, tree.pos, classInfos)

        case tree: PyAsInstanceOf =>
          validateTree(tree.expr, classInfos)
          validateType(tree.tpe, tree.pos, classInfos)

        case tree: PyNewArray =>
          validateTypeRef(tree.elemTypeRef, tree.pos, classInfos)
          validateTree(tree.length, classInfos)

        case tree: PyArrayValue =>
          validateTypeRef(tree.elemTypeRef, tree.pos, classInfos)
          tree.elems.foreach(validateTree(_, classInfos))

        case tree: PyArraySelect =>
          validateTree(tree.array, classInfos)
          validateTree(tree.index, classInfos)
          validateType(tree.tpe, tree.pos, classInfos)

        case tree: PyUnaryOp =>
          validateTree(tree.lhs, classInfos)

        case tree: PyBinaryOp =>
          validateTree(tree.lhs, classInfos)
          validateTree(tree.rhs, classInfos)

        case tree: PyClosure =>
          tree.captureParams.foreach(validateParam(_, classInfos))
          tree.params.foreach(validateParam(_, classInfos))
          validateType(tree.resultType, tree.pos, classInfos)
          tree.captureValues.foreach(validateTree(_, classInfos))
          validateTree(tree.body, classInfos)

        case tree: PyClassOf =>
          validateTypeRef(tree.typeRef, tree.pos, classInfos)

        case _: PyLiteral =>
          ()

    private def requireField(
        field: PyFieldName,
        pos: PyPosition,
        classInfos: Map[PyClassName, ClassInfo]
    ): Unit =
      classInfos.get(field.owner) match
        case None =>
          error(s"Unresolved field owner '${field.owner.nameString}'", pos)
        case Some(info) if !info.hasField(field) =>
          error(s"Unresolved field '${showField(field)}'", pos)
        case Some(_) =>
          ()

    private def requireInstanceMethod(
        startClass: PyClassName,
        method: PyMethodName,
        pos: PyPosition,
        classInfos: Map[PyClassName, ClassInfo]
    ): Unit =
      classInfos.get(startClass) match
        case None =>
          error(s"Unresolved class '${startClass.nameString}'", pos)
        case Some(_) =>
          val resolved = lookupInAncestors(startClass, classInfos)(_.hasInstanceMethod(method))
          if !resolved then
            // Be lenient when the lookup chain touches a Java-provided
            // class: these are backed by Python builtins and expose methods
            // via `__getattr__`-style dispatch that can't be enumerated.
            // Compiled stdlib classes (Predef, Product, etc.) are NOT lenient
            // — missing methods on them are real errors.
            val touchesJavaProvided =
              lookupInAncestors(startClass, classInfos)(_.runtime.exists(_.javaProvided))
            if !touchesJavaProvided then
              error(s"Unresolved instance method '${startClass.nameString}.${showMethod(method)}'", pos)

    private def requireExactInstanceMethod(
        owner: PyClassName,
        method: PyMethodName,
        pos: PyPosition,
        classInfos: Map[PyClassName, ClassInfo]
    ): Unit =
      classInfos.get(owner) match
        case None =>
          error(s"Unresolved class '${owner.nameString}'", pos)
        case Some(info) if !info.hasExactInstanceMethod(method) =>
          error(s"Unresolved exact instance method '${owner.nameString}.${showMethod(method)}'", pos)
        case Some(_) =>
          ()

    private def requireStaticMethod(
        owner: PyClassName,
        method: PyMethodName,
        pos: PyPosition,
        classInfos: Map[PyClassName, ClassInfo]
    ): Unit =
      classInfos.get(owner) match
        case None =>
          error(s"Unresolved class '${owner.nameString}'", pos)
        case Some(info) if !info.hasStaticMethod(method) =>
          error(s"Unresolved static method '${owner.nameString}.${showMethod(method)}'", pos)
        case Some(_) =>
          ()

    private def requireConstructor(
        owner: PyClassName,
        ctor: PyMethodName,
        pos: PyPosition,
        classInfos: Map[PyClassName, ClassInfo]
    ): Unit =
      classInfos.get(owner) match
        case None =>
          error(s"Unresolved class '${owner.nameString}'", pos)
        case Some(info) if !info.hasConstructor(ctor) =>
          error(s"Unresolved constructor '${owner.nameString}.${showMethod(ctor)}'", pos)
        case Some(_) =>
          ()

    private def requireClass(
        className: PyClassName,
        pos: PyPosition,
        classInfos: Map[PyClassName, ClassInfo],
        what: String
    ): Unit =
      if !classInfos.contains(className) then
        error(s"Unresolved $what", pos)

    private def validateMethodName(
        method: PyMethodName,
        pos: PyPosition,
        classInfos: Map[PyClassName, ClassInfo]
    ): Unit =
      method.paramTypeRefs.foreach(validateTypeRef(_, pos, classInfos))
      validateTypeRef(method.resultTypeRef, pos, classInfos)

    private def validateTypeRef(
        typeRef: PyTypeRef,
        pos: PyPosition,
        classInfos: Map[PyClassName, ClassInfo]
    ): Unit =
      typeRef match
        case PyPrimRef(_) =>
          ()
        case PyClassRef(_) =>
          // Type references are non-binding at runtime - Python is
          // duck-typed and the emitter strips all nominal type
          // annotations on parameters and returns. Skip the linker
          // check so that synthetic methods returning stdlib types
          // (e.g. `productIterator(): scala.collection.Iterator`) do
          // not fail the link.
          ()
        case PyArrayRef(base, _) =>
          validateTypeRef(base, pos, classInfos)

    private def validateType(
        tpe: PyType,
        pos: PyPosition,
        classInfos: Map[PyClassName, ClassInfo]
    ): Unit =
      tpe match
        case PyClassType(_) =>
          // Soft check, see `validateTypeRef`. Type tags on tree nodes
          // are descriptive only.
          ()
        case _ =>
          ()

    private def lookupInAncestors(
        startClass: PyClassName,
        classInfos: Map[PyClassName, ClassInfo]
    )(predicate: ClassInfo => Boolean): Boolean =
      def loop(pending: List[PyClassName], seen: Set[PyClassName]): Boolean =
        pending match
          case Nil =>
            false
          case className :: rest if seen.contains(className) =>
            loop(rest, seen)
          case className :: rest =>
            classInfos.get(className) match
              case Some(info) if predicate(info) =>
                true
              case Some(info) =>
                loop(info.superClass.toList ::: info.interfaces ::: rest, seen + className)
              case None =>
                loop(rest, seen + className)

      loop(List(startClass), Set.empty)

    private def collectFirsts[A, K](items: List[A])(key: A => K): Map[K, A] =
      val firsts = mutable.LinkedHashMap.empty[K, A]
      items.foreach { item =>
        firsts.getOrElseUpdate(key(item), item)
      }
      firsts.toMap

    private def isInstanceMethod(method: PyMethodDef): Boolean =
      method.flags.namespace match
        case PyMemberNamespace.Public | PyMemberNamespace.Private => true
        case _ => false

    private def isInstanceLike(method: PyMethodDef): Boolean =
      method.flags.namespace match
        case PyMemberNamespace.Public | PyMemberNamespace.Private | PyMemberNamespace.Constructor => true
        case _ => false

    private def isStaticMethod(method: PyMethodDef): Boolean =
      method.flags.namespace match
        case PyMemberNamespace.PublicStatic | PyMemberNamespace.PrivateStatic => true
        case _ => false

    private def showMethod(method: PyMethodName): String =
      val params = method.paramTypeRefs.map(_.encoded).mkString("(", ", ", ")")
      s"${method.simple.name}$params:${method.resultTypeRef.encoded}"

    private def showField(field: PyFieldName): String =
      s"${field.owner.nameString}.${field.simple.name}"

    private def error(message: String, pos: PyPosition): Unit =
      errors += PyLinkingError(message, pos)
