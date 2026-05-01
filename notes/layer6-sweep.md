# Layer 6 — fresh PyStockRunTests sweep (2026-05-01)

Per the Layer 6 entry in `notes/pyrun-fix-roadmap.md`: re-run the
end-to-end PyStockRunTests sweep after Layers 0–5 trickled down, then
rebuild the failure manifest.

Sweep run: `py-compiler-tests/scripts/sweep.sh /tmp/pyrun-analysis-layer6`
Build state: HEAD = `fd4c8daedc PyLinker: drop Support-main fallback in collectMainEntry`,
support jars freshly rebuilt before the sweep.

## Headline numbers

|                  | Failing | Total fixtures | Failure rate |
|------------------|--------:|---------------:|-------------:|
| Post-Layer-2 baseline (commits ≤ d3b27f284) | 578 | 1708 | 33.8% |
| 2026-04-30 sweep (pre-encoding-fix snapshot) | 571 | 1708 | 33.4% |
| **Layer 6 sweep** (post-encoding-fix + Layer 5.1.d/e/3/4/6/7 + linker cleanup) | **191** | 1708 | **11.2%** |

**Net flips since Layer 2: 387 fixtures green; 2 net regressions.**
**Net flips since the 2026-04-30 sweep: 380 green; 3 fixtures now red that were green.**

The bulk of the flips come from the encoding fix (owner-aware private-
field mangling and `__`-prefix method guard, `d121b4e586`), Layer 5.7
(super-ctor before copyParams, `ee3ea31348`), and Layer 5.1.d
(inherited dunder walk in DCE, `b8fb9d7ccc`).

## Regressions vs post-Layer-2 baseline

Two fixtures regressed (passed at Layer 2, fail at Layer 6):

- **`i763.scala`** — `AttributeError: 'Test_' object has no attribute 's'`.
  Inheritance chain `A → B → Test`; the parent ctor `_scpy_ctor_A` reads
  `self.s__I()` which dispatches to `Test`'s accessor reading `self.s`,
  but `s` is set later by the subclass ctor. Same family as Layer 5.7
  (override-val ctor-order) but the fix did not cover this access
  pattern.
- **`Signals2.scala`** — `AttributeError: 'NoneType' object has no
  attribute 'apply__Ljava_dlang_dObject__Ljava_dlang_dObject'`.
  `frp.Signal.AbstractSignal`'s primary ctor calls `eval(self)` which
  reads `self.expr`; the subclass `Var(expr)` sets `expr` via
  `_scpy_ctor_frp_Signal_Var__Lscala_dFunction1__V`, but the super-ctor
  call runs first under the JVM-style chaining. Same root cause as
  i763.

These should be rolled into Layer 5.7's coverage as a follow-up issue.
The shared symptom is *parent ctor reads child-set field too early*.

## Failure clusters (191 fixtures)

Counts use the FIRST traceback's terminal exception per fixture; bleed
between fixtures in the same bucket is filtered out. Full listing:
`/tmp/layer6-clusters-v5.txt`. Per-fixture error blocks:
`/tmp/layer6-errors/`.

### Reflection — `Class.getX` family — **76 fixtures**

Maps to Layer 4.1 (`issue-reflection-class-introspection.md`).
The sub-clusters all hit the `_scpy_Class.__getattr__` fallback that
raises `AttributeError(name)` for unimplemented reflection methods.

| Symptom | Count | Sample fixtures |
|---|---:|---|
| `getDeclaredFields` | 23 | nothing-{val,var,lazy-val}, null-{val,var,lazy-val}, unit-{val,var,lazy-val,volatile-var}, paramForwarding{,_separate,_together,_together_b}, capturing, i11052{a,b}, i11367, i2883, parameterized-type, t6793, t7214, variable-pattern-access |
| `getDeclaredMethods` | 14 | mixin-bridge-methods, no-useless-forwarders, refined-signature, t6260-delambdafy, t6380, t8177f, value-class-array-signature, erased-inline-vals, i21346, i23479b, i24272, i4523, junitForwarders, returned-context-function-signature |
| `getField` | 12 | i14340, i18612-{a,b,c,d}, i4496{a,b}, i4528, i9404, selectable-new, structural{,-compat} |
| `getMethods` | 8 | forwarder, i24553, i9155, mixin-signatures, t7120b, t7932, traitNoInit, unit_erasure |
| `getMethod`, `getDeclaredMethod`, `getDeclaredField` | 8 total | t4024, throws-annot, i13703, i22991, i19270, i23882, i1692, i1692b |
| `getEnclosingMethod`, `getEnumConstants`, `getDeclaredClasses`, `getGenericInterfaces`, `getConstructors`, `Modifier`, `InvocationTargetException`, `ClassLoader.loadClass` | 11 total | i18701{,.fixed}, i1387, t4023, t8931, i22497, i6834, 16405, i10846, unit-var |

The dominance of this cluster (40% of remaining failures) means **Layer
4.1 is now the single highest-leverage piece of work**. Stubbing each
method to a sensible "absent reflection" return (empty arrays, `null`,
or `_scpy_TODO_reflection`) should flip the bulk in one PR; the
fine-grained shape returned only matters for fixtures that actually
inspect the result.

### Test-internal `assert` failures — **18 fixtures**

`break-opt`, `breaks`, `equality`, `getclass`, `hashCodeDistribution`,
`hashhash`, `i10527`, `i12976`, `i19224`, `i4659b`, `i6710`, `i8314`,
`is-valid-num`, `loops`, `properties-version-string`, `t13033`, `t7912`,
`weak-conformance`.

These run to a Scala-level `assert(...)` that fails. They are
semantic-correctness regressions in codegen — not a single common root
cause; each needs individual investigation. Most likely surface area:
`hashCode` / `equals` semantics on collections, `getClass` boxed-class
identity, integer/numeric coercion edges.

### `ObjectOutputStream() takes no arguments` — **18 fixtures**

`case-class-serializable`, `defaults-serizaliable-{no,with}-forwarders`,
`enums-serialization-compat`, `i20856`, `i4446`, `i8033`, `i9881`,
`inlineAddDeserializeLambda`, `lambda-serialization-security`,
`serialize{,-stream}`, `t10232`, `t3038d`, `t5262`, `t5590`, `t5974`,
`t8188`.

Maps to Layer 3.2 read side (`issue-objectoutputstream-no-args.md`).
The write side landed (`1817f82bfa`); the runtime stub still has a
no-arg constructor mismatch with Scala's `ObjectOutputStream(out)`.
Fix is contained to `PyIRRuntime.scala`'s OO* class.

### 60s subprocess timeouts — **14 fixtures**

`UnrolledBuffer`, `array-erasure`, `caseClassHash`, `collections`,
`i10930`, `i14693`, `i20145`, `kmpSliceSearch`, `t2755`, `t2818`,
`t3502`, `t493`, `t6584`, `t8893`.

The subprocess is killed by the `maxDuration=60` cap in `PyRun`. Some
of these (`UnrolledBuffer`, `kmpSliceSearch`, `t8893`) are
inherently long-running; others (`caseClassHash`, `array-erasure`)
finish in seconds on the JVM and might indicate a real perf regression
in generated Python (e.g. `_scpy_call_to_string` walking long lists).

Recommended split: profile two representative fixtures (`array-erasure`
quick-fail, `caseClassHash` mid-fail) before concluding "raise the
timeout" — the 60s cap exists because legit hangs would otherwise
freeze CI.

### `Anonfun missing self` (NameError on `self`) — **10 fixtures**

`ConfManagement`, `Parser`, `byname-varargs`, `erased-lambdas`,
`for-desugar-strawman`, `given-eta`, `i24201a`, `i9507`,
`quoted-sematics-1`, `tuple-ops`.

Maps directly to Layer 2.1 (`issue-anonfun-static-self-unbound.md`).
The roadmap deferred this because the broad heuristic regressed
pos-py; the body-walking discriminator described in the issue note is
the next step.

### `Enumeration.nextName` missing — **12 fixtures**

`enums`, `iterator-from`, `t2111`, `t3186`, `t3616`, `t3687`, `t3719`,
`t4570`, `t5588`, `t5612`, `t8346`, `t8611b`, `t949`.

All produce `AttributeError on Test_<EnumName>_: nextName`. Maps to
Layer 3.3 (`issue-enumeration-nextname-missing.md`). Self-contained;
fix is on `scala.Enumeration` in pylib.

### `Function0..N.tupled / .curried / .andThen` — **5 fixtures**

`tupled-function-{andThen,apply,compose,extension-method}`, `i6109`.
Missing methods on the synthesized `Function*` runtime. Maps to a gap
in the Function family of `PyIRRuntime.scala` (or a missing `.pyir`
override; `notes/shrink-runtime.md` Phase 3 covers this surface).

### Unresolved-class compile errors — **7 fixtures**

`enums-serialization-compat` and `view-iterator-stream` need
`ReentrantReadWriteLock`. Several other fixtures pull in
`tests/run/<dir>` siblings whose Java sources reference
`java.lang.reflect.{Modifier,Type,TypeVariable,Constructor,Parameter}`.
Most are downstream of Layer 4.1.

### Output mismatch — bleed-only (false positives)

The earlier "37 output mismatch" cluster in v3 was almost entirely
cross-fixture bleed in the bucket logs. After the v5 forward-scan
re-cluster, no fixture's primary error is a real `.check.out`
mismatch.

### Long tail (1–3 each)

`SyntaxError: invalid decimal literal` (16405, 9416 — class names
starting with a digit are not valid Python identifiers; need a sanitize
in `PyEncoding.classIdentifier`).
`SyntaxError: too many nested parentheses` (StringConcat — generated
expression nests too deep; flatten or build via `__iadd__` chain).
`MatchError` (i11050, i1284, safeThrowsStrawman2 — mostly capture-
checked code).
`RuntimeException` (Course-2002-13, t7763, t8601d).
`NullPointerException` (i2772, nullAsInstanceOf — leftover from Layer
5.1.e residual).
`NoSuchElementException` (infiniteloop, t153).
`TypeError: ord() expected string of length 1, but int found`
(CollectionTests, colltest6 — `Char.char2int` getting an int because
of CHar/Int boxing collision).
`AttributeError on float: isNaN__Z` (blame_eye_triple_eee-{double,
float} — Float/Double dispatch missing `isNaN`).
`AttributeError on str: subSequence` (ReplacementMatching,
t6406-regextract — `String.subSequence`/`CharSequence` glue missing).
`TypeError: must be real number, not java_lang_Double` (numbereq —
boxed-Double escapes into a math op).
`NameError: name '_scpy_mod_<X>__'` (IArrayOps `_scpy_mod_scala_IArray_package__`,
t12348 `_scpy_mod_java_lang_invoke_MethodHandles_`,
t7269 `_scpy_mod_scala_collection_convert_JavaCollectionWrappers__` —
linker reachability pruning the wrong module, or DCE dropping a
needed module-init seed).

Plus a small number of one-off ctor-order, lazy-val, and recursion
cases (full list in the cluster file).

## Action items / proposed Wave 4 ordering

Sized by both blast radius and fix complexity:

1. **Layer 4.1 reflection stubs** — flips ~76 fixtures with a
   contained PR to `PyIRRuntime._scpy_Class`. Stub each `getX` to
   return empty/None and gate on a single feature flag for opt-out.
2. **Layer 3.2 read-side `ObjectInputStream`/`ObjectOutputStream(out)`** —
   flips ~18.
3. **Layer 3.3 `Enumeration.nextName`** — flips ~12. Self-contained
   pylib edit.
4. **Layer 2.1 anonfun-self body-walking** — flips ~10. Higher risk
   because the prior heuristic regressed pos-py; needs careful
   discriminator design.
5. **Function family `.tupled` / `.curried` / `.andThen`** — flips ~5.
   Tied into Phase 3 of `shrink-runtime.md`.
6. **Layer 5.7 follow-up: parent-ctor-reads-child-field** — flips 2
   (i763, Signals2). New issue note needed; this is a regression so
   should land before Wave 4 is closed.
7. **Investigate the 18 `AssertionError` fixtures individually** —
   no single root cause; group as Wave 4-residual.
8. **Class-name sanitize for digit-leading identifiers** — flips 2
   (16405, 9416).
9. **Profile / triage the 14 timeouts** — split between perf
   regressions (potentially a couple) and inherently slow tests
   (excludelist with reason tag).

After (1)–(3) land, the manifest should drop from 191 to roughly
**~85 failing fixtures**, putting the failure rate under 5%.

## Reproducing

```bash
sbt --client "scala-pylib-py/clean; scala-pylib-py/Compile/packageBin; \
              scala-library-py/clean; scala-library-py/Compile/packageBin"
py-compiler-tests/scripts/sweep.sh /tmp/pyrun-analysis-layer6

# Build the failing-fixture manifest:
grep -hoE "Test 'tests/run/[^']+\.scala' failed" \
  /tmp/pyrun-analysis-layer6/bucket-*.log \
  | sed -E "s|Test 'tests/run/(.+)' failed|\1|" | sort -u \
  > /tmp/pyrun-analysis-layer6/failed-fixtures.txt

# Diff against post-Layer-2 baseline:
grep -oE 'tests/run/[^ ]+\.scala' notes/post-layer2-baseline.md \
  | sort -u | sed 's|tests/run/||' > /tmp/post-layer2.list
comm -23 /tmp/post-layer2.list /tmp/pyrun-analysis-layer6/failed-fixtures.txt  # green flips
comm -13 /tmp/post-layer2.list /tmp/pyrun-analysis-layer6/failed-fixtures.txt  # regressions
```

Per-fixture error extraction and clustering:
`/tmp/extract-errors.py` and `/tmp/cluster-v5.py` (see
`/tmp/layer6-errors/` and `/tmp/layer6-clusters-v5.txt` for outputs of
this run).
