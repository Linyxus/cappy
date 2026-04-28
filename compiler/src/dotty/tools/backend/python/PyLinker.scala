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
  private val MaxReachabilityIterations = 10

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
    link(userInputs, supportInputs, MaxReachabilityIterations)

  private[python] def link(
      userInputs:                 List[Input],
      supportInputs:              List[Input],
      maxReachabilityIterations:  Int
  ): LinkedBundle =
    new Linker(userInputs, supportInputs, maxReachabilityIterations).link()

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

  private final class Linker(
      userInputs:                List[Input],
      supportInputs:             List[Input],
      maxReachabilityIterations: Int
  ):
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
      val keptClasses = optimizeReachability(allClasses, mainEntry, userClassNames)

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

    private def optimizeReachability(
        classes:        List[PyClassDef],
        mainEntry:      Option[PyIREmitter.MainEntry],
        userClassNames: Set[PyClassName]
    ): List[PyClassDef] =
      var current = classes
      var reach = analyzeReachability(current, mainEntry, userClassNames)
      var iteration = 0

      while iteration < maxReachabilityIterations do
        val methodPrunedClasses = pruneReachableClassesAndMethods(current, reach)
        val rewrittenClasses =
          rewriteDeadSupportInitStores(methodPrunedClasses, reach, userClassNames)
        val nextReach =
          analyzeReachability(rewrittenClasses, mainEntry, userClassNames)

        if nextReach == reach then
          return applyReachability(rewrittenClasses, nextReach, userClassNames)

        current = rewrittenClasses
        reach = nextReach
        iteration += 1

      error(
        s"Reachability DCE did not converge after $maxReachabilityIterations iterations",
        PyPosition.NoPosition
      )
      applyReachability(current, reach, userClassNames)

    private def analyzeReachability(
        classes:        List[PyClassDef],
        mainEntry:      Option[PyIREmitter.MainEntry],
        userClassNames: Set[PyClassName]
    ): PyReachability.Result =
      val (userClasses, supportClasses) =
        classes.partition(c => userClassNames.contains(c.name))
      PyReachability.analyze(userClasses, supportClasses, mainEntry)

    /** Drop unreachable classes and, for each class that survives,
     *  drop support methods and fields whose bodies / declarations the
    *  analyzer never reached. User classes are preserved verbatim. */
    private def applyReachability(
        classes:        List[PyClassDef],
        reach:          PyReachability.Result,
        userClassNames: Set[PyClassName]
    ): List[PyClassDef] =
      val instantiatedFieldOwners = collectInstantiatedFieldOwners(classes, reach)
      val methodPrunedClasses = pruneReachableClassesAndMethods(classes, reach)
      // Safety net: if a method survives for a conservative linker reason
      // (ctor dispatcher, <clinit>, abstract surface), fields referenced by
      // that emitted body must survive too.
      val emittedFieldRefs = collectEmittedFieldRefs(methodPrunedClasses)
      methodPrunedClasses.map { c =>
        pruneClassFields(c, reach, userClassNames, instantiatedFieldOwners, emittedFieldRefs)
      }

    private def pruneReachableClassesAndMethods(
        classes: List[PyClassDef],
        reach:   PyReachability.Result
    ): List[PyClassDef] =
      classes.iterator.collect {
        case c if reach.isReachable(c.name) =>
          pruneClassMethods(c, reach)
      }.toList

    private def collectInstantiatedFieldOwners(
        classes: List[PyClassDef],
        reach:   PyReachability.Result
    ): Set[PyClassName] =
      val byName = classes.iterator.map(c => c.name -> c).toMap
      val owners = mutable.HashSet.empty[PyClassName]
      def mark(cls: PyClassName): Unit =
        if owners.add(cls) then
          byName.get(cls).foreach { c =>
            c.superClass.foreach(mark)
            c.interfaces.foreach(mark)
          }
      classes.iterator.map(_.name).filter(reach.isInstantiated).foreach(mark)
      owners.toSet

    private def pruneClassMethods(c: PyClassDef, reach: PyReachability.Result): PyClassDef =
      def keepMethod(m: PyMethodDef): Boolean =
        val ns = m.flags.namespace
        if reach.isMethodReachable(c.name, m.name) then true
        // `<clinit>` runs on class definition; keep it when the class
        // is kept. Matches the analyzer's proactive <clinit> analysis.
        else if ns == PyMemberNamespace.StaticConstructor then true
        // Abstract declarations have no body to pull anything in, and
        // preserving them keeps the interface surface intact for
        // downstream consumers.
        else if m.body.isEmpty then true
        else false
      c.copy(methods = c.methods.filter(keepMethod))

    private def pruneClassFields(
        c:                       PyClassDef,
        reach:                   PyReachability.Result,
        userClassNames:          Set[PyClassName],
        instantiatedFieldOwners: Set[PyClassName],
        emittedFieldRefs:        Set[PyFieldName]
    ): PyClassDef =
      val isUserClass = userClassNames.contains(c.name)
      def keepField(f: PyFieldDef): Boolean =
        val ns = f.flags.namespace
        if isUserClass then true
        else if emittedFieldRefs.contains(f.name) then true
        else if ns.isStatic then reach.isFieldReachable(c.name, f.name)
        else instantiatedFieldOwners.contains(c.name) && reach.isFieldReachable(c.name, f.name)
      c.copy(fields = c.fields.filter(keepField))

    private final case class RewriteContext(
        currentClass: PyClassDef,
        method:       PyMethodDef
    ):
      def rewritesModuleCtorStores: Boolean =
        currentClass.kind == PyClassKind.ModuleClass &&
          method.flags.namespace == PyMemberNamespace.Constructor

      def rewritesStaticCtorStores: Boolean =
        method.flags.namespace == PyMemberNamespace.StaticConstructor

      def canRewriteStores: Boolean =
        rewritesModuleCtorStores || rewritesStaticCtorStores

    private def rewriteDeadSupportInitStores(
        classes:        List[PyClassDef],
        reach:          PyReachability.Result,
        userClassNames: Set[PyClassName]
    ): List[PyClassDef] =
      val supportClassNames =
        classes.iterator.map(_.name).filterNot(userClassNames.contains).toSet
      val declaredSupportFields =
        classes.iterator
          .filter(c => supportClassNames.contains(c.name))
          .flatMap(_.fields.iterator.map(_.name))
          .toSet

      def isDeclaredSupportField(field: PyFieldName): Boolean =
        declaredSupportFields.contains(field)

      // Scala 2/3 module-class vs companion-class share the same backing
      // PyIR field when the GenPython static-forwarder pass folds them.
      // A read of `Foo.x` must keep `Foo_.x` alive (and vice versa). The
      // suffix-flip itself is centralized in
      // `PyEncoding.companionFieldOf` — DCE never re-derives the
      // underscore convention inline.
      def linkedFieldAlias(field: PyFieldName): Option[PyFieldName] =
        val alias = PyEncoding.companionFieldOf(field)
        if declaredSupportFields.contains(alias) then Some(alias) else None

      def isFieldRead(field: PyFieldName): Boolean =
        reach.isFieldRead(field.owner, field) ||
          linkedFieldAlias(field).exists(alias => reach.isFieldRead(alias.owner, alias))

      def isUnreadSupportField(field: PyFieldName): Boolean =
        isDeclaredSupportField(field) && !isFieldRead(field)

      def shouldRewriteDeadStore(lhs: PyAssignable, ctx: RewriteContext): Boolean =
        ctx.canRewriteStores && (lhs match
          case PySelect(_: PyThis, field) =>
            ctx.rewritesModuleCtorStores &&
              field.owner == ctx.currentClass.name &&
              isUnreadSupportField(field)
          case PySelectStatic(field) =>
            ctx.rewritesStaticCtorStores &&
              field.owner == ctx.currentClass.name &&
              isUnreadSupportField(field)
          case _ =>
            false
        )

      def isDroppablePure(tree: PyTree): Boolean = tree match
        case _: PyLiteral | _: PyThis | _: PyVarRef | _: PyClassOf =>
          true
        case PyUnaryOp(_, lhs) =>
          isDroppablePure(lhs)
        case PyBinaryOp(_, lhs, rhs) =>
          isDroppablePure(lhs) && isDroppablePure(rhs)
        case PyAsInstanceOf(expr, _) =>
          isDroppablePure(expr)
        case PyIsInstanceOf(expr, _) =>
          isDroppablePure(expr)
        case PyArrayValue(_, elems) =>
          elems.forall(isDroppablePure)
        case _ =>
          false

      def replacementForDeadStore(rhs: PyTree, pos: PyPosition, ctx: RewriteContext): PyTree =
        val rewrittenRhs = rewriteTree(rhs, ctx)
        rewrittenRhs match
          // Loading a module solely to cache it in an unread support
          // initializer field is the bloat pattern this pass targets.
          case _: PyLoadModule => PySkip()(pos)
          case _ if isDroppablePure(rewrittenRhs) => PySkip()(pos)
          case _ => rewrittenRhs

      def rewriteAssignable(tree: PyAssignable, ctx: RewriteContext): PyAssignable =
        tree match
          case PyVarRef(name) =>
            PyVarRef(name)(tree.tpe, tree.pos)
          case PySelect(qualifier, field) =>
            PySelect(rewriteTree(qualifier, ctx), field)(tree.tpe, tree.pos)
          case PySelectStatic(field) =>
            PySelectStatic(field)(tree.tpe, tree.pos)
          case PyAttrAccess(obj, name) =>
            PyAttrAccess(rewriteTree(obj, ctx), name)(tree.tpe, tree.pos)
          case PyArraySelect(array, index) =>
            PyArraySelect(rewriteTree(array, ctx), rewriteTree(index, ctx))(tree.tpe, tree.pos)

      def rewriteTree(tree: PyTree, ctx: RewriteContext): PyTree = tree match
        case PyVarDef(name, originalName, vtpe, mutable, rhs) =>
          PyVarDef(name, originalName, vtpe, mutable, rewriteTree(rhs, ctx))(tree.pos)

        case PyAssign(lhs, rhs) if shouldRewriteDeadStore(lhs, ctx) =>
          replacementForDeadStore(rhs, tree.pos, ctx)

        case PyAssign(lhs, rhs) =>
          PyAssign(rewriteAssignable(lhs, ctx), rewriteTree(rhs, ctx))(tree.pos)

        case PyReturn(value) =>
          PyReturn(rewriteTree(value, ctx))(tree.pos)

        case PyWhile(cond, body) =>
          PyWhile(rewriteTree(cond, ctx), rewriteTree(body, ctx))(tree.pos)

        case _: PySkip =>
          tree

        case PyIf(cond, thenp, elsep) =>
          PyIf(rewriteTree(cond, ctx), rewriteTree(thenp, ctx), rewriteTree(elsep, ctx))(tree.tpe, tree.pos)

        case PyTryCatch(block, errVar, errVarOriginalName, handler) =>
          PyTryCatch(rewriteTree(block, ctx), errVar, errVarOriginalName, rewriteTree(handler, ctx))(tree.tpe, tree.pos)

        case PyTryFinally(block, finalizer) =>
          PyTryFinally(rewriteTree(block, ctx), rewriteTree(finalizer, ctx))(tree.pos)

        case PyMatch(selector, cases, default) =>
          PyMatch(
            rewriteTree(selector, ctx),
            cases.map { case (lits, body) => (lits, rewriteTree(body, ctx)) },
            rewriteTree(default, ctx)
          )(tree.tpe, tree.pos)

        case PyBlock(stats, expr) =>
          PyBlock(stats.map(rewriteTree(_, ctx)), rewriteTree(expr, ctx))(tree.pos)

        case PyLabeled(label, body) =>
          PyLabeled(label, rewriteTree(body, ctx))(tree.tpe, tree.pos)

        case PyLabelReturn(label, value) =>
          PyLabelReturn(label, rewriteTree(value, ctx))(tree.pos)

        case _: PyVarRef | _: PyThis =>
          tree

        case PySelect(qualifier, field) =>
          PySelect(rewriteTree(qualifier, ctx), field)(tree.tpe, tree.pos)

        case PySelectStatic(_) =>
          tree

        case PyApply(flags, receiver, className, method, args) =>
          PyApply(flags, rewriteTree(receiver, ctx), className, method, args.map(rewriteTree(_, ctx)))(tree.tpe, tree.pos)

        case PyApplyStatically(flags, receiver, className, method, args) =>
          PyApplyStatically(flags, rewriteTree(receiver, ctx), className, method, args.map(rewriteTree(_, ctx)))(tree.tpe, tree.pos)

        case PyApplyStatic(flags, className, method, args) =>
          PyApplyStatic(flags, className, method, args.map(rewriteTree(_, ctx)))(tree.tpe, tree.pos)

        case PyApplyExternal(callee, args) =>
          PyApplyExternal(callee, args.map(rewriteTree(_, ctx)))(tree.tpe, tree.pos)

        case _: PyExternalRef =>
          tree

        case PyAttrAccess(obj, name) =>
          PyAttrAccess(rewriteTree(obj, ctx), name)(tree.tpe, tree.pos)

        case PyApplyDynamic(callee, args, kwargs) =>
          PyApplyDynamic(
            rewriteTree(callee, ctx),
            args.map(rewriteTree(_, ctx)),
            kwargs.map((name, value) => (name, rewriteTree(value, ctx)))
          )(tree.tpe, tree.pos)

        case PyNew(className, ctor, args) =>
          PyNew(className, ctor, args.map(rewriteTree(_, ctx)))(tree.pos)

        case _: PyLoadModule =>
          tree

        case PyIsInstanceOf(expr, testType) =>
          PyIsInstanceOf(rewriteTree(expr, ctx), testType)(tree.pos)

        case PyAsInstanceOf(expr, tpe) =>
          PyAsInstanceOf(rewriteTree(expr, ctx), tpe)(tree.pos)

        case PyNewArray(elemTypeRef, length) =>
          PyNewArray(elemTypeRef, rewriteTree(length, ctx))(tree.pos)

        case PyArrayValue(elemTypeRef, elems) =>
          PyArrayValue(elemTypeRef, elems.map(rewriteTree(_, ctx)))(tree.pos)

        case PyArraySelect(array, index) =>
          PyArraySelect(rewriteTree(array, ctx), rewriteTree(index, ctx))(tree.tpe, tree.pos)

        case PyUnaryOp(op, lhs) =>
          PyUnaryOp(op, rewriteTree(lhs, ctx))(tree.pos)

        case PyBinaryOp(op, lhs, rhs) =>
          PyBinaryOp(op, rewriteTree(lhs, ctx), rewriteTree(rhs, ctx))(tree.pos)

        case PyClosure(params, resultType, body) =>
          PyClosure(
            params,
            resultType,
            rewriteTree(body, ctx)
          )(tree.pos)

        case _: PyClassOf | _: PyLiteral =>
          tree

      classes.map { cls =>
        if userClassNames.contains(cls.name) then cls
        else
          val methods = cls.methods.map { method =>
            val ctx = RewriteContext(cls, method)
            if ctx.canRewriteStores then
              method.copy(body = method.body.map(rewriteTree(_, ctx)))
            else method
          }
          cls.copy(methods = methods)
      }

    private def collectEmittedFieldRefs(classes: List[PyClassDef]): Set[PyFieldName] =
      val refs = mutable.HashSet.empty[PyFieldName]
      def walk(tree: PyTree): Unit = tree match
        case t: PyVarDef         => walk(t.rhs)
        case t: PyAssign         => walk(t.lhs); walk(t.rhs)
        case t: PyReturn         => walk(t.value)
        case t: PyWhile          => walk(t.cond); walk(t.body)
        case _: PySkip           => ()
        case t: PyIf             => walk(t.cond); walk(t.thenp); walk(t.elsep)
        case t: PyTryCatch       => walk(t.block); walk(t.handler)
        case t: PyTryFinally     => walk(t.block); walk(t.finalizer)
        case t: PyMatch          =>
          walk(t.selector)
          t.cases.foreach { case (_, body) => walk(body) }
          walk(t.default)
        case t: PyBlock          => t.stats.foreach(walk); walk(t.expr)
        case t: PyLabeled        => walk(t.body)
        case t: PyLabelReturn    => walk(t.value)
        case _: PyVarRef         => ()
        case _: PyThis           => ()
        case t: PySelect         => walk(t.qualifier); refs += t.field
        case t: PySelectStatic   => refs += t.field
        case t: PyApply          => walk(t.receiver); t.args.foreach(walk)
        case t: PyApplyStatically =>
          walk(t.receiver)
          t.args.foreach(walk)
        case t: PyApplyStatic    => t.args.foreach(walk)
        case t: PyApplyExternal  => t.args.foreach(walk)
        case _: PyExternalRef    => ()
        case t: PyAttrAccess     => walk(t.obj)
        case t: PyApplyDynamic   =>
          walk(t.callee)
          t.args.foreach(walk)
          t.kwargs.foreach((_, value) => walk(value))
        case t: PyNew            => t.args.foreach(walk)
        case _: PyLoadModule     => ()
        case t: PyIsInstanceOf   => walk(t.expr)
        case t: PyAsInstanceOf   => walk(t.expr)
        case t: PyNewArray       => walk(t.length)
        case t: PyArrayValue     => t.elems.foreach(walk)
        case t: PyArraySelect    => walk(t.array); walk(t.index)
        case t: PyUnaryOp        => walk(t.lhs)
        case t: PyBinaryOp       => walk(t.lhs); walk(t.rhs)
        case t: PyClosure        => walk(t.body)
        case _: PyClassOf        => ()
        case _: PyLiteral        => ()

      for
        cls <- classes
        method <- cls.methods
        body <- method.body
      do walk(body)
      refs.toSet

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
      val classDefs = mutable.LinkedHashMap.empty[PyClassName, (PyClassDef, InputSource)]

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
            case Some((_, InputSource.User)) if input.source == InputSource.Support =>
              // Stale `.pyir` on the classpath or in the output dir
              // carrying the same class name as a fresh User class.
              // The in-memory User copy is authoritative; drop silently.
              ()
            case Some(_) =>
              // User × User or Support × Support duplicates remain hard
              // errors. The first signals user error; the second is
              // normally prevented by the canonical-path dedupe upstream.
              error(s"Duplicate class '${cls.name.nameString}'", cls.pos)
            case None =>
              classDefs += cls.name -> (cls, input.source)

      classDefs.values.iterator.map(_._1).toList

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
      cls.interfaces.foreach { iface =>
        requireClass(iface, cls.pos, classInfos, s"interface '${iface.nameString}'")
      }

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
          tree.params.foreach(validateParam(_, classInfos))
          validateType(tree.resultType, tree.pos, classInfos)
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

    /** True when the lookup chain starting at `startClass` includes a
     *  Java-provided runtime class. These classes are backed by Python
     *  builtins (`object`, `str`, `type`, `Throwable`, …) and expose
     *  methods via `__getattr__`-style dispatch we cannot enumerate at
     *  link time, so we treat them as duck-typed external surface. The
     *  laxness deliberately does NOT propagate to compiled stdlib
     *  classes (Predef, Product, …): those have a closed PyIR surface
     *  and missing methods on them remain hard errors. */
    private def hasJavaProvidedInChain(
        startClass: PyClassName,
        classInfos: Map[PyClassName, ClassInfo]
    ): Boolean =
      lookupInAncestors(startClass, classInfos)(_.runtime.exists(_.javaProvided))

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
          if !resolved && !hasJavaProvidedInChain(startClass, classInfos) then
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
          // Type references in method signatures (param/result types)
          // are deliberately NOT linked here. They're carried by PyIR
          // for method-name uniqueness across overloads and for the
          // sjsir-style mangled identifier (`foo__I__V`), but the
          // Python emitter strips all nominal annotations: signatures
          // become `def foo(self, x):` with no type info. A signature
          // that mentions an unbundled stdlib class (e.g. an abstract
          // `productIterator(): scala.collection.Iterator`) therefore
          // generates working Python even when the named class is DCE'd
          // away. Linking these would force us to keep nominally-
          // referenced-but-runtime-unused classes alive, defeating the
          // class-level DCE pass in `PyReachability`.
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
          // `PyType` tags on tree nodes describe a value's static type
          // for documentation/diagnostics; they are never emitted as
          // runtime checks. Same rationale as `validateTypeRef` above:
          // linking them would couple type tags to reachability and
          // pin classes that the emitter never references.
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
