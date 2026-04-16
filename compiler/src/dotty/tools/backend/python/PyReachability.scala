package dotty.tools.backend.python

import dotty.tools.backend.python.ir.pyir.*

import scala.collection.mutable

/** Class-level link-time reachability analysis.
 *
 *  Seeds from the user compilation unit(s) + the main entry and walks
 *  the PyIR of every reached class, enqueueing any transitively named
 *  class. Runtime-provided classes (see [[PyIRRuntime]]) are opaque —
 *  the emitter does not produce them, so they are never added to the
 *  reachable set.
 *
 *  Modeled on Scala.js's `Analyzer` (simplified for a single-pass,
 *  single-threaded, class-level pass). Shaped so V2 method-level DCE
 *  can slot in without changing the public signature: [[Result]] is a
 *  case class that will grow per-method/per-field fields, and
 *  `PyLinker.applyReachability` maps over it as a single hook.
 *
 *  V1 scope:
 *    - Preserve every class in `userClasses` verbatim.
 *    - Preserve the main entry class.
 *    - Preserve the superclass chain + interfaces of every preserved class.
 *    - Preserve any class named by a reference site in a preserved class's
 *      method bodies.
 *    - Skip type-annotation-only references (`resultType`, `ptpe`, etc.)
 *      — stays consistent with `PyLinker.validateTypeRef`'s "descriptive
 *      only" stance for type references.
 */
object PyReachability:

  /** The output of [[analyze]]. A value-level wrapper (not a raw
   *  `Set[PyClassName]`) so V2 can grow this with per-method /
   *  per-field reachability without breaking call sites. */
  final case class Result(reachableClasses: Set[PyClassName]):
    def isReachable(cls: PyClassName): Boolean = reachableClasses.contains(cls)

  def analyze(
      userClasses:    List[PyClassDef],
      supportClasses: List[PyClassDef],
      mainEntry:      Option[PyIREmitter.MainEntry]
  ): Result =
    new Analyzer(userClasses, supportClasses, mainEntry).run()

  private final class Analyzer(
      userClasses:    List[PyClassDef],
      supportClasses: List[PyClassDef],
      mainEntry:      Option[PyIREmitter.MainEntry]
  ):
    private val classByName: Map[PyClassName, PyClassDef] =
      (userClasses.iterator ++ supportClasses.iterator).map(c => c.name -> c).toMap

    private val visited  = mutable.HashSet.empty[PyClassName]
    private val worklist = mutable.ArrayDeque.empty[PyClassName]

    def run(): Result =
      for cls <- userClasses do enqueue(cls.name)
      mainEntry.foreach { case (cn, _) => enqueue(cn) }

      while worklist.nonEmpty do
        val name = worklist.removeHead()
        if visited.add(name) then
          classByName.get(name).foreach(process)

      Result(visited.toSet)

    /** Enqueue `name` for later processing. Runtime-provided classes are
     *  dropped on the floor: they are already opaque leaves to the
     *  linker (see `PyIRRuntime`) and never participate in emission. */
    private def enqueue(name: PyClassName): Unit =
      if !visited.contains(name)
         && PyIRRuntime.providedClass(name).isEmpty
      then worklist += name

    private def process(cls: PyClassDef): Unit =
      cls.superClass.foreach(enqueue)
      cls.interfaces.foreach(enqueue)
      cls.methods.foreach { m => m.body.foreach(walkTree) }

    // --- Tree visitor -------------------------------------------------
    //
    // Mirrors the structure of `PyLinker.validateTree`. Each case either
    // extracts named class references via `enqueue` / `fromTypeRef` /
    // `fromType`, or recurses into children. Keep structurally in sync
    // with `PyLinker.validateTree` when new PyTree cases are added.

    private def walkTree(tree: PyTree): Unit = tree match
      case t: PyVarDef =>
        walkTree(t.rhs)

      case t: PyAssign =>
        walkTree(t.lhs)
        walkTree(t.rhs)

      case t: PyReturn =>
        walkTree(t.value)

      case t: PyWhile =>
        walkTree(t.cond)
        walkTree(t.body)

      case t: PyForEach =>
        walkTree(t.iterable)
        walkTree(t.body)

      case _: PySkip =>
        ()

      case t: PyIf =>
        walkTree(t.cond); walkTree(t.thenp); walkTree(t.elsep)

      case t: PyTryCatch =>
        walkTree(t.block); walkTree(t.handler)

      case t: PyTryFinally =>
        walkTree(t.block); walkTree(t.finalizer)

      case t: PyMatch =>
        walkTree(t.selector)
        t.cases.foreach { case (_, body) => walkTree(body) }
        walkTree(t.default)

      case t: PyBlock =>
        t.stats.foreach(walkTree)
        walkTree(t.expr)

      case t: PyLabeled =>
        walkTree(t.body)

      case t: PyLabelReturn =>
        walkTree(t.value)

      case _: PyVarRef =>
        ()

      case _: PyThis =>
        ()

      case t: PySelect =>
        walkTree(t.qualifier)
        enqueue(t.field.owner)

      case t: PySelectStatic =>
        enqueue(t.field.owner)

      case t: PyApply =>
        walkTree(t.receiver)
        t.args.foreach(walkTree)
        enqueue(t.className)

      case t: PyApplyStatically =>
        walkTree(t.receiver)
        t.args.foreach(walkTree)
        enqueue(t.className)

      case t: PyApplyStatic =>
        t.args.foreach(walkTree)
        enqueue(t.className)

      case t: PyApplyExternal =>
        // Opaque — the callee targets a Python builtin / runtime helper.
        t.args.foreach(walkTree)

      case _: PyExternalRef =>
        // Opaque — resolved at runtime via `import`.
        ()

      case t: PyAttrAccess =>
        walkTree(t.obj)

      case t: PyApplyDynamic =>
        walkTree(t.callee)
        t.args.foreach(walkTree)
        t.kwargs.foreach((_, v) => walkTree(v))

      case t: PyNew =>
        t.args.foreach(walkTree)
        enqueue(t.className)

      case t: PyLoadModule =>
        enqueue(t.className)

      case t: PyIsInstanceOf =>
        walkTree(t.expr)
        fromTypeRef(t.testType)

      case t: PyAsInstanceOf =>
        walkTree(t.expr)
        fromType(t.tpe)

      case t: PyNewArray =>
        fromTypeRef(t.elemTypeRef)
        walkTree(t.length)

      case t: PyArrayValue =>
        fromTypeRef(t.elemTypeRef)
        t.elems.foreach(walkTree)

      case t: PyArraySelect =>
        walkTree(t.array); walkTree(t.index)

      case t: PyUnaryOp =>
        walkTree(t.lhs)

      case t: PyBinaryOp =>
        walkTree(t.lhs); walkTree(t.rhs)

      case t: PyClosure =>
        t.captureValues.foreach(walkTree)
        walkTree(t.body)

      case t: PyClassOf =>
        fromTypeRef(t.typeRef)

      case _: PyLiteral =>
        ()

    private def fromTypeRef(ref: PyTypeRef): Unit = ref match
      case PyClassRef(name)   => enqueue(name)
      case PyArrayRef(base, _) => fromTypeRef(base)
      case PyPrimRef(_)       => ()

    private def fromType(tpe: PyType): Unit = tpe match
      case PyClassType(name) => enqueue(name)
      case _                 => ()
