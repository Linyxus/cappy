# Wave 5 → Wave 6 roadmap (2026-05-02)

Source: `notes/wave5-sweep.md` (204 failing fixtures clustered).
This roadmap orders the remaining work by ROI (failing fixtures
addressed per unit of investigation).

## Numbered priority

The **Kind** column distinguishes failures that surface at compile/link
(user code never runs) from failures that surface at run time (CPython
raises an exception or stdout diverges from `.check`).

| # | Theme | Kind | Fixture span | Owner note | Risk |
|---|---|---|---:|---|---|
| 1 | Reflection metadata threading | run | ~53 | `issue-reflection-class-introspection.md` (extend) | medium — touches codegen + runtime |
| 2 | Assertion-error cluster split + fixes | run | ~71 | new sub-notes per shape | low/medium |
| 3 | `java.util.concurrent.locks` port | compile | ~14 | new note | medium — locking semantics need real `threading` mapping |
| 4 | Excludelist JVM-only surface | compile | ~7 | mechanical | low |
| 5 | `subSequence` on string/charseq | run | ~5 | small fix | low |
| 6 | Exception ctor `<init>():V` adds | compile | ~5 | small pylib add | low |
| 7 | `int2double__I__D` numeric cast dispatch | run | 4 | codegen lookup | low |
| 8 | `test__I__I` fixture-local dispatch | run | 4 | encoding investigation | low |
| 9 | `run.diff.value_mismatch` per-fixture | run | ~14 | per-fixture | low |
| 10 | `compile.other` longtail | compile | ~14 | per-fixture | low |
| ~~11~~ | ~~Pre-existing flakes / encoder bugs~~ ✅ resolved | mixed | 2 | `t6888` → `f1ed010ca4`, `main-functions` → `a23ecd6c3b` | — |

Sums to ~204 (priorities overlap on the assertion cluster — see #2).
Totals by kind from the cluster table in `notes/wave5-sweep.md`:
**compile = 44**, **run = 160**.

## #1 — Reflection metadata threading (~53 fixtures, highest ROI)

**Where it shows up:** `getDeclaredMethod`, `getDeclaredField`,
`getMethod`, `getField`, `getEnumConstants`, `getDeclaredClasses`,
`getEnclosingMethod`, `getGenericInterfaces`, `getConstructors`,
`subSequence`, `loadClass` raise `AssertionError` from placeholder
stubs in `_scpy_Class`.

**Fix shape:**
1. At `_scpy_register_class` time, capture per-class lists already
   present in PyIR / class definitions:
   - declared field names (Scala simple name + erased type)
   - declared method names (with `__<encoded params>__<encoded ret>`)
   - declared inner-class names
   - enum-constant list (for enum classes)
2. Materialize them as `_scpy_Array` of `_scpy_Field` / `_scpy_Method`
   / `_scpy_Class` instances (already present in pylib's `java.lang.reflect`
   surface — see `pylib-py/src/java/lang/reflect/`).
3. Have the existing stub bodies (currently raising `AssertionError`)
   look up the captured list and return matching wrappers, with
   `NoSuchFieldException` / `NoSuchMethodException` for misses.

**Plumbing pieces involved:**
- `compiler/src/dotty/tools/backend/python/PyIREmitter.scala` — emit
  metadata literal at `emitClassDef` time alongside the existing
  `_scpy_register_class` call (search for `_scpy_register_class`).
- `pylib-py/src/scala/runtime/_scpy_Class.py` (or wherever the
  registry lives) — accept and store the metadata.
- `pylib-py/src/java/lang/reflect/{Field,Method,Constructor}.py` —
  ensure constructable from the metadata snapshot.

**Validation:** rerun the 12 fixtures in `cluster Reflection: Class
introspection` from the cluster table; confirm they pass without
spurious `_scpy_Field.<x>` attribute errors.

**Risk:** there is overlap between Cluster 1 (assertions) and this
work because many `assert`s in user code follow a reflection
introspection that returns the wrong shape. Expect the cluster-1
count to drop further once the metadata threading is real, not just
a stub.

## #2 — Assertion cluster split (~71 fixtures, second ROI)

The 71 fixtures are a heterogeneous cluster. Substructure from
frame analysis:

| Sub-shape | Count | Source / followup |
|---|---:|---|
| boundary/break codegen gap (`break-opt`, `breaks`) | ~5 | label-keyed exception runtime redesign (Layer 5 followup) |
| Murmur3 / numeric-hash mismatch (`caseClassHash`, `hashhash`, `hashCodeDistribution`, `t13033`, `equality`) | ~5 | `layer6-assertion-failures.md` |
| reflection-stub-driven asserts (subset overlapping with #1) | ~20 | recheck after #1 lands |
| Unbound-local / NameError shapes inside assertions | ~5 | per-fixture |
| Genuine semantic asserts (NaN compare, BigDecimal.isWhole, lambda class identity, module-init cycles, weak-conformance widening) | remainder | per-fixture surgical |

**Fix shape:** Split into per-sub-shape work items. Don't try to
fix all 71 in one pass.

## #3 — `java.util.concurrent.locks` port (~14 fixtures)

Compile error class: `Unresolved java.util.concurrent.locks.ReentrantReadWriteLock`,
`AbstractQueuedSynchronizer`, etc.

**Fix shape:** Inventory which fixtures actually exercise locking
semantics vs. just import them.
- Exercise locking → port `ReentrantReadWriteLock` /
  `ReentrantLock` to pylib-py using Python's `threading.RLock` /
  `threading.Lock` underneath. Semantic fidelity is partial (CPython
  GIL changes contention behavior); document the gap.
- Just imports → excludelist with reason tag `jvm-only-locks`.

## #4 — Excludelist JVM-only surface (~7 fixtures)

Mechanical: add to `py-compiler-tests/test/run-py-tests.excludelist`
with reason tags. Targets:

| Symbol family | Fixtures | Reason tag |
|---|---:|---|
| `sun.misc.Unsafe` | 1 | `jvm-only-sun` |
| `javax.swing` / `java.awt` / `javax.imageio` | 1 | `jvm-only-ui` |
| `java.beans` | ~1 | `jvm-only-beans` |
| `java.security` | ~1 | `jvm-only-security` |
| `java.lang.invoke` (LambdaMetafactory, MethodHandles) | 2 | `jvm-only-invoke` |
| `java.lang.reflect.Parameter` (singular) | 1 | depends — see #1 |

## #5 — `subSequence` on `_scpy_String` (~5 fixtures)

Method missing on the Python class our `String` erases to. Likely a
~5-line addition; mirror the JDK `CharSequence.subSequence(start, end)`
contract by slicing.

**Fix location:** `pylib-py/src/java/lang/String.py` (or wherever
`_scpy_String` is implemented).

## #6 — Exception constructor `<init>():V` (~5 fixtures)

Missing no-arg constructors:
- `java.lang.OutOfMemoryError.<init>():V`
- `java.lang.IndexOutOfBoundsException.<init>():V`
- `java.lang.AssertionError.<init>():V`

**Fix shape:** add the no-arg ctors in pylib-py exception ports.
Tiny additions.

## #7 — `int2double__I__D` numeric cast dispatch (4)

Generated code calls a primitive-cast helper that doesn't exist.
Could be a missing prelude method in the runtime, or a missing
codegen path in `PyEncoding`/`PyOps`.

**Investigation:** grep for `int2double` in
`compiler/src/dotty/tools/backend/python/` and in the runtime; one
of the two ends is missing.

## #8 — `test__I__I` fixture-local method dispatch (4)

Method exists in the fixture but call site looks for an encoded
mangling that doesn't match the declaration. Likely encoding
mismatch (e.g., overloaded resolution picking the wrong encoded
suffix).

**Investigation:** dump the emitted `.py` for `Course-2002-09.scala`,
diff the call site mangling against the declaration mangling; isolate
the encoder branch that diverges.

## #9 — `run.diff.value_mismatch` (~14 per-fixture)

Pure stdout diffs without exception. Common causes:
- class-name leakage (`Foo__anon_1` vs `Foo$$anon$1`)
- `getClass.getName` formatting
- floating-point rounding (`2.0714285714285716` vs JVM's
  `2.0714285714285714`)
- `LazyList.toString` rendering (`<not computed>` vs `<lazy>`)

**Fix shape:** per-fixture; small fixes likely.

## #10 — `compile.other` longtail (~14 per-fixture)

Compile errors not matching structured patterns. Per-fixture
investigation; mostly linker/encoding edges.

## #11 — Pre-existing flakes / encoder bugs ✅ resolved

| Fixture | Symptom | Status |
|---|---|---|
| `t6888.scala` | `Duplicate class 'C___abc_'` | ✅ Fixed in `f1ed010ca4`: encoder `_scpy_d` escape so a non-module class ending in `$` can't alias onto a module class slot, plus a `byName`-aware gate on synthetic-forwarder emission. |
| `main-functions.scala` | Non-deterministic `@main` selection | ✅ Fixed in `a23ecd6c3b`: deterministic `PyCodeGenSupport.pickMainEntry` (lex-min over `PyClassName.nameString`, optional `-Xmain-class` override). |

## Resolved items (delete from active queue)

- ✅ Bug A `ScalaRunTime.isArray` infinite recursion — `ae7540d54f`
- ✅ Perf C label-class hoist — committed
- ✅ Perf B 60s → 90s harness cap — committed
- ✅ Perf B' fixtures excludelisted — committed
- ✅ a3b `Enumeration.toString` plumbing — `c74824700a`
- ✅ a2 `ObjectInputStream` port — `d3231b1d3b`
- ✅ a1 reflection stubs (NoSuchMethodException-shaped) — `bd229b6839`
- ✅ a3 `Enumeration.nextName` — `0e0b8293ff`
- ✅ #11 t6888 class+companion `$`-sanitize collision — `f1ed010ca4`
- ✅ #11 main-functions non-deterministic `@main` — `a23ecd6c3b`

## Out of scope for Wave 5

- Module-scope label hoist (deferred from Perf C plan).
- Bug A residual `Array[Long]` element-tag bug — tracked in
  `issue-primitive-array-element-tag.md`.
- `t6584` / `UnrolledBuffer` perf (bound by `_scpy_is_assignable`
  walks, not label-class redefinition).

## Tracking

Move per-cluster progress into the issue notes referenced in the
table above. This roadmap document is the index; do not duplicate
fixture lists into the issue notes — link back here.
