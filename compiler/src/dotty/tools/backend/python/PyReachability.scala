package dotty.tools.backend.python

import dotty.tools.backend.python.ir.pyir.*

import scala.collection.mutable
import scala.util.boundary, boundary.break

/** Link-time reachability analysis for PyIR.
 *
 *  Tracks four facts per class:
 *
 *   1. **`isReachable`** — the class header appears in the output; its
 *      name is referenced somewhere in the kept graph. Class is either
 *      instantiated, a static receiver, an ancestor of a kept class, or
 *      named in a nominal type test.
 *   2. **`isInstantiated`** — an instance of this exact class is ever
 *      constructed via `PyNew` (or the class is a `ModuleClass` whose
 *      singleton is loaded). Gates survival of instance fields and
 *      non-constructor instance methods.
 *   3. **`reachableMethods`** — which specific method signatures on the
 *      class are known to be called. The linker uses this to prune
 *      unused methods from kept classes.
 *   4. **`readFields` / `writtenFields`** — which specific fields are
 *      touched by reachable bodies. The linker prunes fields untouched by
 *      reachable code.
 *
 *  Virtual dispatch uses the Scala.js dispatch-log technique (see
 *  `inbox/scala-js/.../analyzer/Analyzer.scala`): a virtual call on a
 *  static-receiver class `C` is logged against `C` and every ancestor;
 *  each time a class `D` becomes instantiated, every logged call on an
 *  ancestor of `D` is resolved on `D`'s vtable and the target marked
 *  reachable. This replaces "mark every override in every subclass"
 *  with "mark overrides only for actually-instantiated subclasses", the
 *  main win over class-level-only DCE.
 *
 *  Runtime-provided classes ([[PyIRRuntime.providedClass]]) are opaque:
 *  they never enter the reachable set, never get methods analyzed, and
 *  the emitter replaces them with hand-written shims.
 *
 *  Seeding policy for V1: **preserve user classes verbatim.** Every
 *  `PyClassDef` in `userClasses` has all its methods and fields
 *  seeded; module classes are additionally forced to instantiated.
 *  Support classes (classpath `.pyir`) live or die on the edges
 *  discovered by walking user code.
 */
object PyReachability:

  /** Per-class reachability facts accumulated during analysis.
   *
   *  V1 sets only [[isReachable]] once per class; subsequent V2 steps
   *  populate the method / field / instantiation sets and the virtual
   *  call log.
   */
  final case class ClassReachability(
      isReachable:      Boolean             = false,
      isInstantiated:   Boolean             = false,
      reachableMethods: Set[PyMethodName]   = Set.empty,
      readFields:       Set[PyFieldName]    = Set.empty,
      writtenFields:    Set[PyFieldName]    = Set.empty,
      virtualCallLog:   Set[PyMethodName]   = Set.empty,
  )

  /** The output of [[analyze]]. `facts` is keyed by class name; classes
   *  absent from the map are unreachable. Query helpers collapse the
   *  absent case to `false`. */
  final case class Result(facts: Map[PyClassName, ClassReachability]):
    def isReachable(cls: PyClassName): Boolean =
      facts.get(cls).exists(_.isReachable)
    def isInstantiated(cls: PyClassName): Boolean =
      facts.get(cls).exists(_.isInstantiated)
    def isMethodReachable(owner: PyClassName, method: PyMethodName): Boolean =
      facts.get(owner).exists(_.reachableMethods.contains(method))
    def isFieldRead(owner: PyClassName, field: PyFieldName): Boolean =
      facts.get(owner).exists(_.readFields.contains(field))
    def isFieldWritten(owner: PyClassName, field: PyFieldName): Boolean =
      facts.get(owner).exists(_.writtenFields.contains(field))
    def isFieldReachable(owner: PyClassName, field: PyFieldName): Boolean =
      isFieldRead(owner, field) || isFieldWritten(owner, field)

  def analyze(
      userClasses:    List[PyClassDef],
      supportClasses: List[PyClassDef],
      mainEntry:      Option[PyIREmitter.MainEntry]
  ): Result =
    new Analyzer(userClasses, supportClasses, mainEntry).run()

  /** Allow-list of Python dunder method *simple names* that must remain
   *  reachable on every instantiated class. The runtime / Python data
   *  model invokes these implicitly — never through a PyIR call edge —
   *  so the analyzer would otherwise prune them.
   *
   *  Scope notes:
   *
   *    - `__init__` is also covered by the `Constructor` namespace
   *      branch, but it's listed here for completeness.
   *    - `__eq__` / `__ne__` / `__hash__` are kept because Python's
   *      data model invokes them for `==`, `!=`, dict / set membership,
   *      and `hash()`. The emitter additionally rebinds
   *      `__hash__ = Ancestor.__hash__` for any class that defines
   *      `__eq__` but not `__hash__`.
   *    - `__str__` / `__repr__` are kept because `_scpy_to_str` /
   *      Python's default printer call them for `String.valueOf` and
   *      diagnostic output (`print(x)`, `repr(x)`).
   *    - `__iter__`, `__next__`, `__len__`, `__getitem__`,
   *      `__setitem__`, `__contains__`, `__bool__` realize iteration,
   *      sequence, and truthiness protocols used by user-side code that
   *      writes Pythonic loops over Scala collections wired through
   *      facades.
   *    - `__call__` is kept because closures and `FunctionN` instances
   *      route `f(args)` through `__call__`.
   *    - `__enter__` / `__exit__` realize the `with` statement protocol
   *      for context managers (`AutoCloseable`-style facades).
   *    - The arithmetic dunders (`__add__` etc.) cover BigInteger /
   *      BigDecimal-style operator-overloading facades in pylib.
   *
   *  Excluded on purpose: `__new__`, `__class__`, `__mro__`, `__name__`,
   *  `__getattr__`, `__setattr__`, `__getattribute__`, `__slots__`,
   *  `__main__`. Those are either Python-internal (handled by the
   *  prelude) or never emitted from Scala source. */
  val KeptDunders: Set[String] = Set(
    "__init__",
    // comparison / hash
    "__eq__", "__ne__", "__hash__",
    "__lt__", "__le__", "__gt__", "__ge__",
    // string conversion
    "__str__", "__repr__",
    // iteration / sequence
    "__iter__", "__next__", "__len__",
    "__getitem__", "__setitem__", "__delitem__",
    "__contains__", "__reversed__",
    // truthiness / call
    "__bool__", "__call__",
    // context manager
    "__enter__", "__exit__",
    // arithmetic (pylib BigInteger / BigDecimal)
    "__add__", "__sub__", "__mul__",
    "__truediv__", "__floordiv__", "__mod__", "__divmod__",
    "__pow__", "__lshift__", "__rshift__",
    "__and__", "__or__", "__xor__",
    "__neg__", "__pos__", "__abs__", "__invert__",
    // numeric conversion
    "__int__", "__float__", "__index__",
    // copy / pickle
    "__copy__", "__deepcopy__",
  )

  // -------------------------------------------------------------------
  // Internal worklist tokens
  // -------------------------------------------------------------------
  private enum Work:
    case ReachClass(cls: PyClassName)
    case Instantiate(cls: PyClassName)
    case AnalyzeMethod(owner: PyClassName, method: PyMethodName)
    case ReadField(owner: PyClassName, field: PyFieldName)
    case WriteField(owner: PyClassName, field: PyFieldName)

  // -------------------------------------------------------------------
  // Analyzer
  // -------------------------------------------------------------------
  private final class Analyzer(
      userClasses:    List[PyClassDef],
      supportClasses: List[PyClassDef],
      mainEntry:      Option[PyIREmitter.MainEntry]
  ):
    private val classByName: Map[PyClassName, PyClassDef] =
      (userClasses.iterator ++ supportClasses.iterator).map(c => c.name -> c).toMap

    /** Mutable per-class facts. Classes absent from this map are
     *  unreachable; query helpers on [[Result]] collapse that to
     *  `false`. We pre-insert an entry at first touch in
     *  [[stateOf]]. */
    private val state = mutable.HashMap.empty[PyClassName, MutableState]

    private final class MutableState:
      var isReachable:     Boolean = false
      var isInstantiated:  Boolean = false
      val reachableMethods: mutable.HashSet[PyMethodName] = new mutable.HashSet
      val readFields:       mutable.HashSet[PyFieldName]  = new mutable.HashSet
      val writtenFields:    mutable.HashSet[PyFieldName]  = new mutable.HashSet
      val virtualCallLog:   mutable.HashSet[PyMethodName] = new mutable.HashSet
      def freeze: ClassReachability = ClassReachability(
        isReachable      = isReachable,
        isInstantiated   = isInstantiated,
        reachableMethods = reachableMethods.toSet,
        readFields       = readFields.toSet,
        writtenFields    = writtenFields.toSet,
        virtualCallLog   = virtualCallLog.toSet,
      )

    private def stateOf(cls: PyClassName): MutableState =
      state.getOrElseUpdate(cls, new MutableState)

    private val worklist = mutable.ArrayDeque.empty[Work]
    private def enqueue(w: Work): Unit = worklist += w

    // --- Inheritance graph (precomputed) ----------------------------
    // directDescendants[C] = every class whose superClass or interfaces
    // directly contains C. Built lazily with transitiveAncestors below.
    private val directDescendants: Map[PyClassName, Set[PyClassName]] =
      val acc = mutable.HashMap.empty[PyClassName, mutable.HashSet[PyClassName]]
      for c <- classByName.valuesIterator do
        c.superClassName.foreach { s => acc.getOrElseUpdate(s, mutable.HashSet.empty) += c.name }
        c.interfaces.foreach { i => acc.getOrElseUpdate(i, mutable.HashSet.empty) += c.name }
      acc.view.mapValues(_.toSet).toMap

    /** Memo for [[ancestorsOf]]. Scoped to this `Analyzer` instance,
     *  so it is dropped when the analyzer is GC'd after `analyze()`
     *  returns. */
    private val ancestorsCache = mutable.HashMap.empty[PyClassName, Set[PyClassName]]

    /** Transitively reachable ancestors of `cls`, not including `cls`
     *  itself. Runtime-provided ancestors are included (they are class
     *  names we *might* want to log virtual calls against, even if we
     *  never emit them). */
    private def ancestorsOf(cls: PyClassName): Set[PyClassName] =
      ancestorsCache.get(cls) match
        case Some(cached) => cached
        case None =>
          val seen = mutable.HashSet.empty[PyClassName]
          val stack = mutable.ArrayDeque.empty[PyClassName]
          classByName.get(cls).foreach { cd =>
            cd.superClassName.foreach(stack += _)
            cd.interfaces.foreach(stack += _)
          }
          while stack.nonEmpty do
            val a = stack.removeHead()
            if seen.add(a) then
              classByName.get(a).foreach { cd =>
                cd.superClassName.foreach(stack += _)
                cd.interfaces.foreach(stack += _)
              }
          val result = seen.toSet
          ancestorsCache(cls) = result
          result

    // --- Entry point ------------------------------------------------

    def run(): Result =
      seed()
      drain()
      Result(state.view.mapValues(_.freeze).toMap)

    private def seed(): Unit =
      // Preserve user classes verbatim: every method and field rooted.
      for cls <- userClasses do
        enqueue(Work.ReachClass(cls.name))
        if cls.kind == PyClassKind.ModuleClass then
          enqueue(Work.Instantiate(cls.name))
        for m <- cls.methods do enqueue(Work.AnalyzeMethod(cls.name, m.name))
        for f <- cls.fields do
          enqueue(Work.ReadField(cls.name, f.name))
          enqueue(Work.WriteField(cls.name, f.name))

      mainEntry.foreach { case (cn, kind) =>
        enqueue(Work.ReachClass(cn))
        if kind == PyClassKind.ModuleClass then
          enqueue(Work.Instantiate(cn))
        // If the main-entry class is a Support class (rare: a classpath
        // class supplying a main), the `main` method needs an explicit
        // AnalyzeMethod edge. For a User main the method was already
        // enqueued above; the dedupe in AnalyzeMethod handling makes
        // the double-enqueue harmless.
        classByName.get(cn).foreach { cd =>
          cd.methods
            .find(_.name.simple.name == "main")
            .foreach(m => enqueue(Work.AnalyzeMethod(cn, m.name)))
        }
      }

      // Runtime-prelude call edges. The hand-written Python preamble
      // (see `PyIRRuntime.prelude`) calls a handful of Scala-defined
      // methods directly; the analyzer cannot see those calls because
      // they are not part of any walked tree. Seed them from the
      // static list in `PyIRRuntime`.
      for (owner, method) <- PyIRRuntime.preludeCalls do
        enqueue(Work.ReachClass(owner))
        enqueue(Work.Instantiate(owner))
        enqueue(Work.AnalyzeMethod(owner, method))

      // Runtime-prelude virtual-call seeds. Codegen replaces certain
      // interface calls (e.g. `CharSequence.subSequence`) with
      // `PyApplyExternal` calls into runtime helpers, so the analyzer
      // never observes the underlying virtual call. Replay the call
      // here so subtype overrides survive DCE; see [[virtualCallSeeds]].
      for (staticRecv, method) <- PyIRRuntime.virtualCallSeeds do
        enqueue(Work.ReachClass(staticRecv))
        logVirtualCall(staticRecv, method)

    private def drain(): Unit =
      while worklist.nonEmpty do
        worklist.removeHead() match
          case Work.ReachClass(c)         => reachClass(c)
          case Work.Instantiate(c)        => instantiate(c)
          case Work.AnalyzeMethod(o, m)   => analyzeMethod(o, m)
          case Work.ReadField(o, f)       => readField(o, f)
          case Work.WriteField(o, f)      => writeField(o, f)

    // --- Work item handlers -----------------------------------------

    /** Runtime-provided classes never participate in emission, so we
     *  never touch their state. */
    private def isRuntimeProvided(cls: PyClassName): Boolean =
      PyIRRuntime.providedClass(cls).isDefined

    private def reachClass(cls: PyClassName): Unit = boundary:
      if isRuntimeProvided(cls) then break()
      val s = stateOf(cls)
      if s.isReachable then break()
      s.isReachable = true
      classByName.get(cls).foreach { cd =>
        cd.superClassName.foreach(sc => enqueue(Work.ReachClass(sc)))
        cd.interfaces.foreach(i  => enqueue(Work.ReachClass(i)))
        // `<clinit>` runs on class definition in Python; if we keep the
        // class we'll run its static init, so analyze the body to pull
        // in whatever it references.
        for m <- cd.methods if m.flags.namespace == PyMemberNamespace.StaticConstructor do
          enqueue(Work.AnalyzeMethod(cls, m.name))
      }

    private def instantiate(cls: PyClassName): Unit = boundary:
      if isRuntimeProvided(cls) then break()
      val s = stateOf(cls)
      if s.isInstantiated then break()
      s.isInstantiated = true
      enqueue(Work.ReachClass(cls))
      // Three classes of methods must be kept on any instantiated class
      // even if no Scala-side call site mentions them:
      //   1. The no-arg constructor (signature `()V`) — the emitter's
      //      synthesized `__init__(self)` delegates to it for module-
      //      class lazy initialization, and Python's data model invokes
      //      `cls()` calls through `__init__` at any unanticipated
      //      `cls()` site. Other ctor overloads are NOT auto-rooted: they
      //      must be reached by an explicit `PyNew` (which logs the
      //      specific encoded ctor) or by `super.<init>(...)` /
      //      `this(...)` call sites. This is what enables per-constructor
      //      DCE; runtime arity / type-guard dispatch is gone.
      //   2. Every Python dunder (`__call__`, `__str__`, `__iter__`,
      //      `__enter__`, comparison hooks, etc.). Those are invoked
      //      by the Python runtime — not by any tree we walk — so
      //      the analyzer has no explicit edge for them. Without this
      //      rule, a user-facing callable class like Timer's
      //      `RunOnce(task)` would have its `__call__` pruned and the
      //      timer would hang forever.
      //   3. Scala `toString`: `_scpy_to_str` implements String.valueOf
      //      by reflectively calling `toString__Ljava_lang_String` when
      //      present, so the method has no explicit PyIR call edge.
      val locallyKeptSimples = mutable.HashSet.empty[String]
      classByName.get(cls).foreach { cd =>
        for m <- cd.methods do
          val ns    = m.flags.namespace
          val simp  = m.name.simple.name
          val isNoArgCtor =
            ns == PyMemberNamespace.Constructor && m.name.paramTypeRefs.isEmpty
          if isNoArgCtor || isPythonDunder(simp) || isScalaToString(m.name) then
            enqueue(Work.AnalyzeMethod(cls, m.name))
            if isInstanceMethod(ns) then locallyKeptSimples += simp
      }
      // Inherited dunders / `toString`: when the instantiated class
      // doesn't define its own dunder, the emitter will rebind to the
      // ancestor's slot at emission time (`maybeRebindInheritedHash`,
      // facade fallthroughs). For that rebind to find a live body, the
      // ancestor's method must survive DCE — but the local-only loop
      // above misses it. Walk the ancestor chain and enqueue any
      // matching method on its declaring class. This is the t4122
      // `Seq[Char].##` case: `WrappedString` and `List` inherit
      // `Seq.__hash__`; without this walk it gets pruned and the
      // emitter rebinds to the identity hash on `_scpy_Object` instead.
      for ancestor <- ancestorsOf(cls) do
        classByName.get(ancestor).foreach { ad =>
          for m <- ad.methods do
            val ns   = m.flags.namespace
            val simp = m.name.simple.name
            if isInstanceMethod(ns)
               && !locallyKeptSimples.contains(simp)
               && (isPythonDunder(simp) || isScalaToString(m.name))
            then
              enqueue(Work.AnalyzeMethod(ancestor, m.name))
        }
      // Replay every accumulated virtual-call log on the new vtable.
      val chain = cls +: ancestorsOf(cls).toSeq
      for a <- chain do
        state.get(a).foreach { as =>
          for m <- as.virtualCallLog do
            resolveInstanceMethod(cls, m).foreach { case (owner, method) =>
              enqueue(Work.AnalyzeMethod(owner, method))
            }
        }

    private def analyzeMethod(owner: PyClassName, method: PyMethodName): Unit = boundary:
      if isRuntimeProvided(owner) then break()
      val s = stateOf(owner)
      if !s.reachableMethods.add(method) then break()
      enqueue(Work.ReachClass(owner))
      classByName.get(owner).flatMap(_.methods.find(_.name == method)) match
        case Some(mdef) => mdef.body.foreach(walkTree)
        case None =>
          // Not a direct member — either inherited only, or missing.
          // For exact / super dispatch that lands here by mistake we
          // stay lenient, mirroring `PyLinker.requireInstanceMethod`'s
          // Java-provided pass-through.
          ()

    private def readField(owner: PyClassName, field: PyFieldName): Unit =
      reachField(owner, field)(_.readFields)

    private def writeField(owner: PyClassName, field: PyFieldName): Unit =
      reachField(owner, field)(_.writtenFields)

    private def reachField(owner: PyClassName, field: PyFieldName)(
        select: MutableState => mutable.HashSet[PyFieldName]
    ): Unit = boundary:
      if isRuntimeProvided(owner) then break()
      val s = stateOf(owner)
      if !select(s).add(field) then break()
      enqueue(Work.ReachClass(owner))

    // --- Virtual dispatch -------------------------------------------

    /** Log a virtual call against `staticRecv` and every ancestor, and
     *  immediately dispatch to any currently-instantiated descendant.
     *  Subsequent instantiations will replay via [[instantiate]].
     *
     *  Additionally resolves the call against `staticRecv`'s current
     *  method table and enqueues the resolver. Without this, a call
     *  with no instantiated receiver would never pull in the fallback
     *  method body, and the linker's ancestor-walk in
     *  [[PyLinker.requireInstanceMethod]] would fail validation.
     *
     *  Runtime-provided receivers (e.g. `scala.FunctionN`) are *not*
     *  short-circuited: when stdlib code calls `pred.apply(elem)` on a
     *  `Function1` argument, the static receiver is the runtime-provided
     *  `Function1` interface but the implementation lives on instantiated
     *  subclasses (`HashMap`, `HashSet`, `partialNotApplied`, anonymous
     *  closures, ...). Skipping the log here was a previous source of
     *  silent DCE that pruned `apply` overrides on Function-extending
     *  collection classes. See
     *  `notes/issue-{hashmap-apply-mcii-sp,set-intersect-apply-object,
     *  list-collect-applyorelse-default}-missing.md`. We still avoid
     *  enqueuing analysis on runtime-provided owners. */
    private def logVirtualCall(staticRecv: PyClassName, m: PyMethodName): Unit =
      // Log against the receiver and every ancestor so that a future
      // instantiation of a deeper descendant (whose ancestors transit
      // through `staticRecv`) still sees this call.
      stateOf(staticRecv).virtualCallLog += m
      for a <- ancestorsOf(staticRecv) do stateOf(a).virtualCallLog += m
      // Static-receiver fallback: keep whichever class currently owns
      // the default definition in the reachable set.
      if !isRuntimeProvided(staticRecv) then
        resolveInstanceMethod(staticRecv, m).foreach { case (owner, method) =>
          if !isRuntimeProvided(owner) then
            enqueue(Work.AnalyzeMethod(owner, method))
        }
      // Dispatch to already-instantiated descendants.
      val candidates = staticRecv +: gatherDescendants(staticRecv).toSeq
      for d <- candidates do
        state.get(d).foreach { ds =>
          if ds.isInstantiated then
            resolveInstanceMethod(d, m).foreach { case (owner, method) =>
              if !isRuntimeProvided(owner) then
                enqueue(Work.AnalyzeMethod(owner, method))
            }
        }
      // The `library-py`-compiled `scala.FunctionN.pyir` emits each
      // `apply_mc<X><Y>_sp__...` specialized variant as a body that
      // calls `this.apply__Ljava_dlang_dObject*__Ljava_dlang_dObject`
      // (boxed) and then unboxes the result via `_scpy_unbox_or_default`.
      // For that body to reach a concrete target, the unspecialized
      // boxed `apply` must survive DCE — but no static call site
      // references it directly when the user code only goes through
      // the specialized form (e.g. `m(k)` on a primitive map). For
      // every `apply*` virtual call we ALSO log the unspecialized
      // boxed form on the same receiver so it stays reachable on the
      // class that ultimately resolves the dispatch.
      applyBoxedFallback(m).foreach { boxed =>
        if boxed != m then
          stateOf(staticRecv).virtualCallLog += boxed
          for a <- ancestorsOf(staticRecv) do stateOf(a).virtualCallLog += boxed
          if !isRuntimeProvided(staticRecv) then
            resolveInstanceMethod(staticRecv, boxed).foreach { case (owner, method) =>
              if !isRuntimeProvided(owner) then
                enqueue(Work.AnalyzeMethod(owner, method))
            }
          for d <- candidates do
            state.get(d).foreach { ds =>
              if ds.isInstantiated then
                resolveInstanceMethod(d, boxed).foreach { case (owner, method) =>
                  if !isRuntimeProvided(owner) then
                    enqueue(Work.AnalyzeMethod(owner, method))
                }
            }
      }

    /** When `m` is an `apply` variant of a Scala `FunctionN` (boxed,
     *  primitive-specialized `apply_mc..._sp`, or any `apply<suffix>`
     *  shape), return the canonical unspecialized boxed form
     *  `apply__Ljava_lang_Object*__Ljava_lang_Object` of the same
     *  arity. Returns `None` if `m`'s simple name is not an `apply*`
     *  variant. */
    private def applyBoxedFallback(m: PyMethodName): Option[PyMethodName] =
      val simpleName = m.simple.name
      if simpleName != "apply" && !simpleName.startsWith("apply_") && !simpleName.startsWith("apply$") then
        None
      else
        val objectRef = PyClassRef(PyClassName.ObjectClass)
        val arity = m.paramTypeRefs.size
        Some(PyMethodName(
          PySimpleMethodName("apply"),
          List.fill(arity)(objectRef),
          objectRef
        ))

    private def gatherDescendants(cls: PyClassName): Set[PyClassName] =
      val seen = mutable.HashSet.empty[PyClassName]
      val stack = mutable.ArrayDeque.empty[PyClassName]
      directDescendants.get(cls).foreach(_.foreach(stack += _))
      while stack.nonEmpty do
        val d = stack.removeHead()
        if seen.add(d) then
          directDescendants.get(d).foreach(_.foreach(stack += _))
      seen.toSet

    /** Walk `start`'s superchain (including `start`) and return the
     *  first class that defines an instance method with name `m`.
     *  Returns `None` if the method is resolved only via a
     *  runtime-provided ancestor or isn't in the bundle.
     *
     *  Resolution order mirrors typical JVM virtual dispatch:
     *
     *    1. Walk the superclass chain from `start` upward. The first
     *       class that locally declares `m` wins.
     *    2. If no class on the chain declares `m`, fall back to
     *       interface-default lookup. We walk `start`'s interfaces
     *       *transitively*: each interface's own super-interfaces are
     *       searched too, with cycle protection via a `seen` set.
     *
     *  Walking interface-of-interface chains is important for
     *  default-method discovery in deeply layered hierarchies (e.g.
     *  `LinearSeqOps` -> `SeqOps` -> `IterableOps`); without the
     *  transitive walk, an interface default living two hops away
     *  would never be linked, even though the JVM would dispatch to
     *  it. The transitive walk is breadth-first so the closest default
     *  wins; cycles (mutual-recursive interface tangles introduced by
     *  ScalaPy support libraries) are guarded by `seen`. */
    private def resolveInstanceMethod(
        start:  PyClassName,
        m:      PyMethodName
    ): Option[(PyClassName, PyMethodName)] =
      // First walk the superclass chain.
      var cur: Option[PyClassName] = Some(start)
      var found: Option[(PyClassName, PyMethodName)] = None
      while cur.isDefined && found.isEmpty do
        val cn = cur.get
        classByName.get(cn) match
          case Some(cd) =>
            cd.methods.find(md => md.name == m && isInstanceMethod(md.flags.namespace)) match
              case Some(_) => found = Some((cn, m))
              case None    => cur = cd.superClassName
          case None =>
            cur = None
      // Fall back to a transitive interface-default lookup. BFS through
      // `start`'s interface graph (interface-of-interface chains
      // included) with cycle protection. We seed the queue from every
      // class on the superclass chain so super-classes' interfaces are
      // also reachable, matching JVM virtual-dispatch semantics for
      // default methods.
      if found.isEmpty then
        val seen = mutable.HashSet.empty[PyClassName]
        val queue = mutable.ArrayDeque.empty[PyClassName]
        var hop: Option[PyClassName] = Some(start)
        while hop.isDefined do
          val cn = hop.get
          classByName.get(cn) match
            case Some(cd) =>
              cd.interfaces.foreach(queue += _)
              hop = cd.superClassName
            case None => hop = None
        while queue.nonEmpty && found.isEmpty do
          val ifaceName = queue.removeHead()
          if seen.add(ifaceName) then
            classByName.get(ifaceName).foreach { ifd =>
              ifd.methods.find(md => md.name == m && isInstanceMethod(md.flags.namespace)) match
                case Some(_) => found = Some((ifd.name, m))
                case None    => ifd.interfaces.foreach(queue += _)
            }
      found

    private def isInstanceMethod(ns: PyMemberNamespace): Boolean =
      ns == PyMemberNamespace.Public || ns == PyMemberNamespace.Private

    /** When `className` is a non-ModuleClass that has a `<className>_`
     *  ModuleClass companion in the bundle, return the companion module
     *  name. Mirrors `PyIREmitter.routeToModuleVar`: the emitter routes
     *  `PyLoadModule(C)` to `<C>_` whenever such a module class exists,
     *  so reachability must keep that module class alive too. */
    private def loadModuleCompanion(className: PyClassName): Option[PyClassName] =
      classByName.get(className) match
        case Some(c) if c.kind == PyClassKind.ModuleClass => None
        case _ =>
          val underscored = PyClassName(className.nameString + "_")
          classByName.get(underscored) match
            case Some(c) if c.kind == PyClassKind.ModuleClass => Some(underscored)
            case _ => None

    /** Python dunder predicate: `__foo__` with length ≥ 5 AND in the
     *  explicit allow-list of dunders the Scala-to-Python emission can
     *  actually produce at user-class scope. Mirrors the convention in
     *  `PyNames.PyMethodName.isDunder` for the shape, but is stricter
     *  about *which* dunders we keep alive on every instantiated class.
     *
     *  Why an explicit allow-list and not "any dunder":
     *
     *    * `PyMethodName.encoded` only renders a Scala-source method as
     *      a bare dunder when the source author explicitly names it
     *      `__foo__`. The set of names actually used is small.
     *    * Constructors are handled by the `Constructor` namespace
     *      branch, not by name match.
     *    * `__getattr__`, `__setattr__`, `__getattribute__`, `__class__`,
     *      `__mro__`, `__name__`, `__new__` are Python-internal and
     *      never emitted as Scala-defined methods we'd want to keep.
     *
     *  See [[PyReachability.KeptDunders]] for the canonical list and
     *  the rationale for each entry. */
    private def isPythonDunder(name: String): Boolean =
      KeptDunders.contains(name)

    private def isScalaToString(name: PyMethodName): Boolean =
      name.simple.name == "toString" &&
        name.paramTypeRefs.isEmpty &&
        name.resultTypeRef == PyClassRef(PyClassName.StringClass)

    // --- Tree visitor ----------------------------------------------

    private def walkTree(tree: PyTree): Unit = tree match
      case t: PyVarDef         => walkTree(t.rhs)
      case PyAssign(PySelect(qualifier, field), rhs) =>
        walkTree(qualifier)
        walkTree(rhs)
        enqueue(Work.WriteField(field.owner, field))
      case PyAssign(PySelectStatic(field), rhs) =>
        walkTree(rhs)
        enqueue(Work.WriteField(field.owner, field))
      case t: PyAssign         => walkTree(t.lhs); walkTree(t.rhs)
      case t: PyReturn         => walkTree(t.value)
      case t: PyWhile          => walkTree(t.cond); walkTree(t.body)
      case _: PySkip           => ()
      case t: PyIf             => walkTree(t.cond); walkTree(t.thenp); walkTree(t.elsep)
      case t: PyTryCatch       => walkTree(t.block); walkTree(t.handler)
      case t: PyTryFinally     => walkTree(t.block); walkTree(t.finalizer)
      case t: PyMatch          =>
        walkTree(t.selector)
        t.cases.foreach { case (_, body) => walkTree(body) }
        walkTree(t.default)
      case t: PyBlock          => t.stats.foreach(walkTree); walkTree(t.expr)
      case t: PyLabeled        => walkTree(t.body)
      case t: PyLabelReturn    => walkTree(t.value)
      case _: PyVarRef         => ()
      case _: PyThis           => ()

      case t: PySelect =>
        walkTree(t.qualifier)
        enqueue(Work.ReadField(t.field.owner, t.field))

      case t: PySelectStatic =>
        enqueue(Work.ReadField(t.field.owner, t.field))

      case t: PyApply =>
        walkTree(t.receiver)
        t.args.foreach(walkTree)
        enqueue(Work.ReachClass(t.className))
        // Constructors can surface as `PyApply` from the uniform-call
        // lowering. Dispatch is exact — not virtual — so route them as
        // static regardless of the dispatch field.
        if t.method.simple.isConstructor then
          enqueue(Work.AnalyzeMethod(t.className, t.method))
        else t.dispatch match
          case PyDispatch.Virtual =>
            logVirtualCall(t.className, t.method)
          case PyDispatch.Static =>
            enqueue(Work.AnalyzeMethod(t.className, t.method))

      case t: PyApplyStatic =>
        t.args.foreach(walkTree)
        enqueue(Work.ReachClass(t.className))
        enqueue(Work.AnalyzeMethod(t.className, t.method))

      case t: PyApplyExternal =>
        // Opaque — callee is a Python builtin / runtime helper.
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
        enqueue(Work.Instantiate(t.className))
        enqueue(Work.AnalyzeMethod(t.className, t.ctor))

      case t: PyLoadModule =>
        // Module-class singletons are "allocated" on load via the
        // lazy-module wrapper; their Constructor/<clinit> run on first
        // access. Force the class instantiated and analyze whichever
        // init entry points exist.
        //
        // When the loaded class is a non-ModuleClass (e.g. `java.lang.Void`,
        // a JVM-style Java class for which dotc treats `Void.TYPE` as a
        // static field on the *class*) and there is a `<className>_`
        // ModuleClass in the bundle that holds the corresponding Scala-side
        // members (the static field forwarder synthesized by
        // `GenPython.genStaticFieldForwarders` is a `PublicStatic` field
        // on the class, but its actual value is initialized by the
        // module's ctor), the emitter routes the `PyLoadModule` through
        // the module variable (see `PyIREmitter.routeToModuleVar`).
        // Without seeding the module here, DCE drops the module class
        // and the emitter falls back to the bare class identifier, which
        // has no value for the static field — `AttributeError: type
        // object 'java_lang_Void' has no attribute 'TYPE'` at runtime.
        // This unblocks every code path that discriminates on
        // `java.lang.Void.TYPE` (`Array.copyAs`, `ArrayBuilder.make`),
        // i.e. essentially every primitive-specialized ArraySeq /
        // grouped / sliding operation.
        enqueue(Work.ReachClass(t.className))
        enqueue(Work.Instantiate(t.className))
        classByName.get(t.className).foreach { cd =>
          for m <- cd.methods do
            val ns = m.flags.namespace
            if ns == PyMemberNamespace.Constructor || ns == PyMemberNamespace.StaticConstructor then
              enqueue(Work.AnalyzeMethod(t.className, m.name))
        }
        loadModuleCompanion(t.className).foreach { mod =>
          enqueue(Work.ReachClass(mod))
          enqueue(Work.Instantiate(mod))
          classByName.get(mod).foreach { cd =>
            for m <- cd.methods do
              val ns = m.flags.namespace
              if ns == PyMemberNamespace.Constructor || ns == PyMemberNamespace.StaticConstructor then
                enqueue(Work.AnalyzeMethod(mod, m.name))
          }
        }

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

      case t: PyTupleValue =>
        fromType(t.tpe)
        t.elems.foreach(walkTree)

      case t: PyDictValue =>
        fromType(t.tpe)
        t.entries.foreach { case (k, v) =>
          walkTree(k); walkTree(v)
        }

      case t: PyListValue =>
        fromType(t.tpe)
        t.elems.foreach(walkTree)

      case t: PyRawTupleValue =>
        fromType(t.tpe)
        t.elems.foreach(walkTree)

      case t: PyUnaryOp =>
        walkTree(t.lhs)

      case t: PyBinaryOp =>
        walkTree(t.lhs); walkTree(t.rhs)

      case t: PyClosure =>
        walkTree(t.body)
        // Closure bodies emit references to their target class directly
        // through `PyApplyStatic` / `PyApplyDynamic` / `PyAttrAccess`
        // (see `GenPython.genClosure`). When the target lives on a
        // ModuleClass, the renderer routes the call through
        // `_scpy_mod_<cls>_` (the lazy-module proxy), which only exists
        // when `PyIREmitter.moduleClasses` selects the class for
        // singleton emission — and that filter requires both
        // `kind == ModuleClass` AND a no-arg constructor surviving DCE.
        // Without an Instantiate edge, the no-arg ctor is pruned and the
        // emitter never binds `_scpy_mod_<cls>_`; the closure raises
        // `NameError` on first call. Walk the closure body a second time
        // and explicitly seed Instantiate for every ModuleClass referenced
        // through the same `_scpy_mod_*_` routing the emitter uses.
        seedModuleAccessorsInClosure(t.body)
        // The closure renders as `_scpy_FnN(lambda ...)`; the carrier
        // class `_scpy_FnN` extends `scala.FunctionN` (the nominal
        // interface). After Phase 3b of `notes/shrink-runtime.md`,
        // `scala.FunctionN` is supplied by `library-py`'s `.pyir`
        // rather than the runtime prelude, so the carrier-emit step
        // (`PyIREmitter.emitClosureCarriers`) only generates the
        // arity-N carrier when `scala.FunctionN` is in `knownClasses`.
        // Seed the nominal class explicitly so it survives DCE and
        // the carrier's parent reference resolves at bundle load.
        // Above-22 arities use the bare `_scpy_Fn` carrier, which has
        // no FunctionN parent.
        val arity = t.params.length
        if 0 <= arity && arity <= 22 then
          enqueue(Work.ReachClass(PyClassName(s"scala.Function$arity")))

      case t: PyClassOf =>
        fromTypeRef(t.typeRef)

      case _: PyLiteral =>
        ()

    private def fromTypeRef(ref: PyTypeRef): Unit = ref match
      case PyClassRef(name)    => enqueue(Work.ReachClass(name))
      case PyArrayRef(base, _) => fromTypeRef(base)
      case PyPrimRef(_)        => ()

    private def fromType(tpe: PyType): Unit = tpe match
      case PyClassType(name) => enqueue(Work.ReachClass(name))
      case _                 => ()

    /** True iff `cn` is bound in this bundle as a `ModuleClass`. The
     *  `PyIREmitter.moduleClasses` filter only emits a `_scpy_mod_<cn>_`
     *  lazy-module variable when the class is a ModuleClass with a
     *  no-arg constructor; the no-arg ctor in turn survives DCE only
     *  when the class is `Instantiate`d. We use this predicate to decide
     *  whether a closure-body reference needs the module-load seed. */
    private def isBundledModuleClass(cn: PyClassName): Boolean =
      classByName.get(cn).exists(_.kind == PyClassKind.ModuleClass)

    /** Mirror the `Instantiate` + ctor / static-ctor enqueues that
     *  `PyLoadModule` performs, including the `loadModuleCompanion`
     *  rerouting. Used by the closure-body walker to keep
     *  `_scpy_mod_<cn>_` bound when a closure-body call targets a
     *  ModuleClass through the `routeToModuleVar` path. Idempotent
     *  through `instantiate`'s "already instantiated" guard. */
    private def seedModuleLoad(cn: PyClassName): Unit =
      enqueue(Work.ReachClass(cn))
      enqueue(Work.Instantiate(cn))
      classByName.get(cn).foreach { cd =>
        for m <- cd.methods do
          val ns = m.flags.namespace
          if ns == PyMemberNamespace.Constructor || ns == PyMemberNamespace.StaticConstructor then
            enqueue(Work.AnalyzeMethod(cn, m.name))
      }
      loadModuleCompanion(cn).foreach { mod =>
        enqueue(Work.ReachClass(mod))
        enqueue(Work.Instantiate(mod))
        classByName.get(mod).foreach { cd =>
          for m <- cd.methods do
            val ns = m.flags.namespace
            if ns == PyMemberNamespace.Constructor || ns == PyMemberNamespace.StaticConstructor then
              enqueue(Work.AnalyzeMethod(mod, m.name))
        }
      }

    /** Recursive walk over a closure body (or any subtree of one) that
     *  seeds an extra `Instantiate` edge for every ModuleClass referenced
     *  through the static-on-module routing the emitter uses. The
     *  primary trigger is `PyApplyStatic(<ModuleClass>, …)`: that node
     *  emits `_scpy_mod_<cls>_.<method>(…)` via
     *  `routeToModuleVar`, but the regular `walkTree` only enqueues
     *  `ReachClass` + `AnalyzeMethod` for it, never `Instantiate`. The
     *  `PyApplyDynamic` / `PyAttrAccess` cases are covered for symmetry
     *  in case future codegen lowers a Scala-source module-accessor
     *  reference through one of those shapes; the `walkTree` for those
     *  already recurses into their subtrees, so we only need to seed the
     *  module-load edge when their callee/obj resolves to a ModuleClass.
     *  Nested closures are handled by recursing into `PyClosure.body`
     *  again (but `walkTree`'s `PyClosure` case will also do its own
     *  seed pass when reached through the worklist). */
    private def seedModuleAccessorsInClosure(tree: PyTree): Unit = tree match
      case t: PyApplyStatic =>
        if isBundledModuleClass(t.className) then seedModuleLoad(t.className)
        t.args.foreach(seedModuleAccessorsInClosure)
      case t: PyApply =>
        seedModuleAccessorsInClosure(t.receiver)
        t.args.foreach(seedModuleAccessorsInClosure)
      case t: PyApplyDynamic =>
        seedModuleAccessorsInClosure(t.callee)
        t.args.foreach(seedModuleAccessorsInClosure)
        t.kwargs.foreach((_, v) => seedModuleAccessorsInClosure(v))
      case t: PyApplyExternal =>
        t.args.foreach(seedModuleAccessorsInClosure)
      case t: PyAttrAccess =>
        seedModuleAccessorsInClosure(t.obj)
      case t: PyNew =>
        t.args.foreach(seedModuleAccessorsInClosure)
      case t: PySelect =>
        seedModuleAccessorsInClosure(t.qualifier)
      case _: PySelectStatic    => ()
      case t: PyVarDef          => seedModuleAccessorsInClosure(t.rhs)
      case t: PyAssign          => seedModuleAccessorsInClosure(t.lhs); seedModuleAccessorsInClosure(t.rhs)
      case t: PyReturn          => seedModuleAccessorsInClosure(t.value)
      case t: PyWhile           => seedModuleAccessorsInClosure(t.cond); seedModuleAccessorsInClosure(t.body)
      case t: PyIf              => seedModuleAccessorsInClosure(t.cond); seedModuleAccessorsInClosure(t.thenp); seedModuleAccessorsInClosure(t.elsep)
      case t: PyTryCatch        => seedModuleAccessorsInClosure(t.block); seedModuleAccessorsInClosure(t.handler)
      case t: PyTryFinally      => seedModuleAccessorsInClosure(t.block); seedModuleAccessorsInClosure(t.finalizer)
      case t: PyMatch           =>
        seedModuleAccessorsInClosure(t.selector)
        t.cases.foreach { case (_, body) => seedModuleAccessorsInClosure(body) }
        seedModuleAccessorsInClosure(t.default)
      case t: PyBlock           =>
        t.stats.foreach(seedModuleAccessorsInClosure)
        seedModuleAccessorsInClosure(t.expr)
      case t: PyLabeled         => seedModuleAccessorsInClosure(t.body)
      case t: PyLabelReturn     => seedModuleAccessorsInClosure(t.value)
      case t: PyIsInstanceOf    => seedModuleAccessorsInClosure(t.expr)
      case t: PyAsInstanceOf    => seedModuleAccessorsInClosure(t.expr)
      case t: PyNewArray        => seedModuleAccessorsInClosure(t.length)
      case t: PyArrayValue      => t.elems.foreach(seedModuleAccessorsInClosure)
      case t: PyArraySelect     => seedModuleAccessorsInClosure(t.array); seedModuleAccessorsInClosure(t.index)
      case t: PyTupleValue      => t.elems.foreach(seedModuleAccessorsInClosure)
      case t: PyDictValue       =>
        t.entries.foreach { case (k, v) =>
          seedModuleAccessorsInClosure(k); seedModuleAccessorsInClosure(v)
        }
      case t: PyListValue       => t.elems.foreach(seedModuleAccessorsInClosure)
      case t: PyRawTupleValue   => t.elems.foreach(seedModuleAccessorsInClosure)
      case t: PyUnaryOp         => seedModuleAccessorsInClosure(t.lhs)
      case t: PyBinaryOp        => seedModuleAccessorsInClosure(t.lhs); seedModuleAccessorsInClosure(t.rhs)
      case t: PyClosure         => seedModuleAccessorsInClosure(t.body)
      case t: PyLoadModule      =>
        // `walkTree`'s PyLoadModule case already seeds the module load,
        // but we recursively visit through the same node here so the
        // structural recursion stays total even if a parent dispatched
        // straight into us without going through `walkTree`.
        if isBundledModuleClass(t.className) then seedModuleLoad(t.className)
      case _: PyVarRef          => ()
      case _: PyThis            => ()
      case _: PyExternalRef     => ()
      case _: PyClassOf         => ()
      case _: PyLiteral         => ()
      case _: PySkip            => ()
