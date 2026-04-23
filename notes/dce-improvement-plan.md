# DCE Improvement Plan — Remaining Work

## Status as of commit `f1c70bd77f` ("Improve DCE")

The Python-backend linker now does method-level DCE (steps 1–4 of the original plan). `testPyCompilation` is green.

Hello-world bundle: **9.3 MB / 1,897 classes → 1.78 MB / 601 classes** (5.5× smaller). Further shrinking is bounded by the items below.

### What landed

1. **Roots refactor** (`GenPython.linkAndWrite`, `PyClasspathLoader`, `PyLinker.collectClasses`): User inputs = the in-memory classes produced by the current compile. Output-dir `.pyir` (including stale files from prior compiles) is tagged Support. User × Support class-name collisions silently drop the Support copy.
2. **Per-class reachability facts** (`PyReachability.ClassReachability`): `isReachable`, `isInstantiated`, `reachableMethods`, `reachableFields`, `virtualCallLog`.
3. **Worklist analyzer** with Scala.js-style dispatch-log snapshot for virtual dispatch. Handles `PyApply`/`PyApplyStatically`/`PyApplyStatic`/`PyNew`/`PyLoadModule`/`PySelect`/`PySelectStatic`/type-tests. Proactively analyzes all ctors and Python dunders (`__call__`, `__str__`, `__iter__`, ...) on instantiated classes — those are entry points the Python runtime can invoke with no Scala-side edge.
4. **Prelude-call seeding** (`PyIRRuntime.preludeCalls`): static list of `(owner, method)` pairs that the hand-written Python preamble invokes reflectively (currently `ThrowablesSupport.throwIllegalMonitorState()` for `Object.wait/notify`, and `Charset.encode(String): ByteBuffer` for `String.getBytes(charset)`).
5. **Method pruning** (`PyLinker.applyReachability`): drop non-reachable methods; keep `<clinit>` on any reachable class, keep all ctors of instantiated classes (for `emitConstructorDispatcher`), keep abstract stubs.

---

## Remaining steps

### Step 5 — Field pruning  *(not yet started)*

Today `applyReachability` only prunes methods; every field of a kept class is retained. Field pruning should track reads and writes separately (matching Scala.js's `BaseLinker` shape).

**Scope:**
- Extend tree edges: `PyAssign(PySelect/PySelectStatic, rhs)` is a *write*; otherwise a `PySelect`/`PySelectStatic` is a *read*. (Distinguishing lets a future V3 drop dead writes too.)
- `PyLinker.pruneClass.keepField`:
  - static fields: keep if read or written;
  - instance fields: keep if read/written **and** owner is instantiated.
- Seeding: for every class in `userClasses` (preserve user verbatim), enqueue `ReachField` on every declared field as today.

**Expected fallout — watch for:**
- ModuleClass default-None initializers: `PyIREmitter` emits `name = None` at class scope for each field before the init sets it. If a field is pruned, the ctor assignment to `self.name = ...` is also pruned; make sure the emitter doesn't emit a dangling `None` declaration for a field that no longer exists.
- Static forwarder fields (synthesized by `emitWithStaticForwarders` for top-level Scala objects): these carry field-level references. Verify their `ReachField` edges fire.
- `scala.Console$`, `SystemProperties` module singletons with lazy init — the field carries the singleton; if dropped, subsequent lookups explode.

**Verification:** `testPyCompilation` must stay green. Expected additional hello-world reduction: ~10–20 %.

---

### Step 6 — Regression tests  *(not yet started)*

Three concrete tests, mirroring the plan:

1. **`ScalaPyBundleSizeTest`** — compile `tests/pos-py/hello.scala`, assert `hello.py < 300 KB` and `class ` count `< 100`. (Current measurement is 1.78 MB / 601 classes with stdlib classpath; tighten the threshold after field pruning lands.)

2. **`ScalaPyStaleIRTest`** — two-compile proxy. Compile `tests/pos-py/stale-probe/unused.scala` into a temp out-dir, then compile `tests/pos-py/stale-probe/main.scala` into the *same* out-dir, assert the second bundle does not contain `unused.scala`'s top-level class name. Guards the roots refactor.

3. **Unused-stdlib grep assertions** — inside the size test, grep `hello.py` for known-dead identifiers (e.g. `scala_collection_mutable_BitSet`, `scala_collection_mutable_HashMap_HashMapIterator`, `scala_collection_immutable_BitSet`). Fail if any appear.

---

## V3 / follow-up items discovered during implementation

### A. Per-constructor DCE
`PyIREmitter.emitConstructorDispatcher` routes `new X(...)` at runtime by scanning *every* ctor's arity/isinstance guards. Step 4 therefore keeps all ctors of any instantiated class. To drop unused ctor overloads, either:
- rewrite the dispatcher to use only the surviving ctor set (requires emitter cooperation), or
- give each ctor a distinct Python name and eliminate the dispatcher altogether.
Expected gain: small (ctors are thin) but occasionally pulls in support classes via default-init RHS expressions.

### B. Prelude-call auto-extraction
`PyIRRuntime.preludeCalls` is hand-maintained. A new stub in the Python prelude that calls into a Scala module method won't be seeded until a human updates that list — silent pruning → runtime hang / `AttributeError`. Options:
- regex-scan `PyIRRuntime.prelude`'s source for `_scpy_mod_<class>__.<method>__<sig>(` and emit the set at phase-init time;
- move the prelude to a separate `.py` resource and embed a declarative "this prelude uses …" header that the backend parses.
Either removes a class of drift bugs that cost ~10 minutes each to diagnose.

### C. Python-dunder rule — review scope
`instantiate(cls)` currently enqueues every method whose simple name matches `__*__` (length ≥ 5). That's correct for the runtime-callable dunders (`__call__`, `__str__`, `__iter__`, `__enter__`, comparison/arithmetic hooks) but may be too loose — e.g. user code naming a helper `__private_helper__` would also be preserved. Consider tightening to a positive allow-list if bundle-size measurements warrant it.

### D. Interior DCE inside kept method bodies
Even after method-level DCE, many kept modules' `<clinit>` bodies contain branches that never fire under a given program (e.g. `Predef`'s init chain initializes `Map`, `Set`, `List`, `Manifest`, `NoManifest` regardless of what the user uses). A pass that constant-folds reachable branches and drops dead sub-expressions would cascade into more class-level pruning. This is the biggest remaining lever on absolute bundle size — probably another 3–5× reduction on hello-world — but is a much larger piece of work than the per-method/per-field story.

### E. `@export`-style user-method DCE
Current seeding preserves every user-class method verbatim. A follow-up could introduce a `@scalapy.export` (or reuse `@main`) to narrow the user root set to declared entry points, matching Scala.js's module-main convention. Would shave the user portion of the bundle but is irrelevant to stdlib bloat.

### F. Test gap — virtual dispatch coverage
No unit test currently exercises the dispatch-log replay path. Add a `PyReachabilityTest` that builds a small class hierarchy (`Animal` with abstract `speak`, `Dog` / `Cat` subclasses, user calls `animal.speak()` on an `Animal`-typed reference), instantiates only `Dog`, and asserts `Dog.speak` is reachable but `Cat.speak` is pruned.

---

## Critical files

- `compiler/src/dotty/tools/backend/python/PyReachability.scala`
- `compiler/src/dotty/tools/backend/python/PyLinker.scala` — `applyReachability`, `pruneClass`
- `compiler/src/dotty/tools/backend/python/PyIRRuntime.scala` — `preludeCalls`
- `compiler/src/dotty/tools/backend/python/PyIREmitter.scala` — `emitConstructorDispatcher` (for V3.A)
- `py-compiler-tests/test/scala/dotty/tools/dotc/` — new regression tests go here

## Verification commands

```bash
sbt --client "testPyCompilation"           # full ScalaPy suite
./bin/scpyc hello.scala && wc -c hello.py  # ad-hoc bundle-size check
grep -c '^class ' hello.py                 # class count
```
