# Wave 5 — post-Bug-A / Perf-C / 90s-harness sweep (2026-05-02)

Sweep run: `py-compiler-tests/scripts/sweep.sh /tmp/pyrun-analysis-postperfc`
HEAD: `0cf3aeb765 Excludelist Perf B' + raise harness cap to 90s`
Support jars: rebuilt clean immediately before the sweep.

## Headline numbers

|                  | Failing | Notes |
|------------------|--------:|---|
| Post-Layer-2 baseline | 578 | from `post-layer2-baseline.md` |
| Layer 6 sweep (2026-05-01) | 272¹ | `layer6-sweep.md` quoted 191 with a narrower extraction |
| Wave 4 sweep (2026-05-01 evening) | 217¹ | extraction-consistent re-count |
| **Wave 5 sweep (2026-05-02)** | **204** | this sweep |

¹ All three counts above are recomputed with the **same** extraction
script (`grep -ohE "tests/run/[^[:space:]]+(\.scala| ) failed"`) so they
compare apples to apples. The original 191 figure used only
`Test 'tests/run/X' failed`, which missed compilation-error and
run-failure shapes.

**Net flips since Wave 4: -13 failing.**

### Newly passing (vs Wave 4)

| Source | Fixtures |
|---|---|
| Bug A `ScalaRunTime.isArray` fix | `array-erasure`, `i10930`, `t2755`, `t493` (+ `i14693` residual still failing on primitive-array tag bug) |
| Perf B 60s → 90s harness | `i20145`, `t6584`, `UnrolledBuffer`, `t2818` |
| Perf B' excludelisted (with reason tags) | `collections`, `kmpSliceSearch`, `t3502`, `t8893` |
| a3b `Enumeration.toString` plumbing (downstream effects) | `enums`, `t3616`, `t3687` |

### Apparent "regressions" — both resolved

- ✅ `t6888.scala` — `Duplicate class 'C___abc_'` compile error.
  Encoder-side bug in class+companion collision under the
  `$`-sanitize path. Fixed in `f1ed010ca4` (encoder `_scpy_d`
  escape + GenPython synthetic-forwarder gate).
- ✅ `main-functions.scala` — non-deterministic `@main` selection.
  Was the `GenPython.scala:263-264` last-class-wins overwrite.
  Fixed in `a23ecd6c3b` (deterministic `PyCodeGenSupport.pickMainEntry`).

## Failure clusters (204 fixtures)

Methodology: per-fixture fingerprint extracted from bucket logs by walking
20 KB of context after each fixture's primary error marker
(`Compilation failed for: ...`, `Output from ... did not match`, or
`Test ... failed with output:`) and matching against a set of stable
signatures (exception type, unresolved-symbol category, traceback frame
name). Script: `/tmp/postperfc-fingerprints/extract2.py`.

### Summary table

The **Kind** column distinguishes failures that surface during compile/link
(user code never runs) from failures that surface at run time (CPython
raises an exception or stdout diverges from `.check`).

| Cluster | Kind | Count | Sample fixture | Issue note |
|---|---|---:|---|---|
| `run.assert.user_code` — bare `assert` in fixture body / library function | run | 71 | `breaks.scala`, `caseClassHash.scala` | partial: `layer6-assertion-failures.md` |
| `run.assert.reflection_or_charseq` — assert raised inside reflection / charseq stub | run | 53 | `i18701.scala`, `i9404.scala` | `issue-reflection-class-introspection.md` |
| `run.diff.value_mismatch` — pure stdout diff, no exception | run | 14 | `Course-2002-08`, `i3006`, `t1987` | (new) |
| `compile.unresolved.java_concurrent` — `ReentrantReadWriteLock`, `AbstractQueuedSynchronizer`, etc | compile | 14 | `Parser.scala` | (new — Layer 4 follow-up) |
| `compile.other` — heterogeneous compile errors | compile | 14 | mixed | (new) |
| `compile.unresolved.exception_ctor` — `OutOfMemoryError`/`IndexOutOfBoundsException`/`AssertionError` constructor missing | compile | 5 | `tryPatternMatch.scala` | (new) |
| `run.attr.subSequence` — `subSequence` missing on String/CharSequence | run | 5 | `i19394.scala` | (new) |
| `run.attr.other (int2double__I__D)` — primitive numeric cast dispatch missing | run | 4 | `i19396.scala` | (new) |
| `run.attr.other (test__I__I)` — fixture-local method dispatch missing | run | 4 | `Course-2002-09.scala` | (new) |
| `run.type_error` — Python `TypeError` raised | run | 4 | `string-switch.scala` | (new) |
| `compile.unresolved.java_invoke` — `LambdaMetafactory`, `MethodHandles` | compile | 2 | `lambda-serialization.scala` | (excludelist candidate) |
| `compile.unresolved.{sun,swing_awt,java_beans,java_security}` — JVM-only surface | compile | 4 | various | (excludelist candidates) |
| `run.attr.other` (small unique attributes) | run | 6 | various | per-fixture |
| `run.npe`, `run.match_error`, `run.name_error` | run | 3 | various | per-fixture |
| `compile.unresolved.other` (longtail unique symbols) | compile | 5 | various | per-fixture |

Sums to 204. Totals by kind: **compile = 44**, **run = 160**.

### Cluster 1 — `run.assert.user_code` (71 fixtures)

The largest cluster, but heterogeneous. Sub-shape from frame analysis:
- ~11 are `assert` at the top of `main` (e.g. `break-opt`, `breaks`) —
  these are the boundary/break codegen gap from
  `layer6-assertion-failures.md`.
- ~12 are `assertFailed` with no clear frame structure ("other AssertionError").
- The rest hit `assertFailed` from various library entry points
  (`error__Ljava_dlang_dString`, `result`, `__getitem`, `requireNonNull`,
  `char2int`, `selectDynamic`, etc) — these are mostly cases where a
  user-written assert lives in fixture-driven test scaffolding and the
  fixture's tested method goes wrong somewhere upstream.

This cluster is too large for a single fix. Suggested split:
- 5 boundary/break (Layer 5 followup; redesign label-keyed exception runtime)
- ~5 Murmur3 / numeric-hash (caseClassHash, hashhash, hashCodeDistribution, t13033, equality) — already in `layer6-assertion-failures.md`
- The remainder need per-fixture investigation; many overlap with the
  reflection cluster once their stub responses are fleshed out.

### Cluster 2 — `run.assert.reflection_or_charseq` (53 fixtures)

Stubs in `_scpy_Class.getDeclaredFields/Methods/Field/Method` and
friends raise `AssertionError` (placeholder) instead of returning real
metadata. This is the natural follow-on to Layer 4.1 (which landed
NoSuchMethodException-shaped stubs) but where the test actually
*inspects* the result.

Maps to `notes/issue-reflection-class-introspection.md`.

Subset by frame:
- `getDeclaredMethod`/`getDeclaredField` (~15) — most common
- `getMethod`/`getMethods` (~10)
- `getField`/`getFields` (~8)
- `getEnumConstants`/`getDeclaredClasses` (~6)
- `getEnclosingMethod`/`getGenericInterfaces`/`getConstructors` (~8)
- `subSequence` (5; charseq, separate concern)
- `loadClass` (3)

Fix shape: capture per-class metadata at `_scpy_register_class` time
(field list, method list, declared-class list) and have the reflection
methods return matching `_scpy_Array` instances. The metadata is
already available in the PyIR/class definition; needs threading through
codegen → runtime → reflection method bodies.

### Cluster 3 — `run.diff.value_mismatch` (14 fixtures)

Pure stdout diffs without an uncaught exception. Likely root causes:
- class-name leakage (e.g. `Foo__anon_1` instead of `Foo$$anon$1`)
- `getClass.getName` formatting
- floating-point rounding (`2.0714285714285716` vs JVM's `2.0714285714285714`)
- `LazyList.toString` rendering (`<not computed>` vs `<lazy>`)

Per-fixture investigation; small fixes likely.

### Cluster 4 — `compile.unresolved.java_concurrent` (14 fixtures)

`ReentrantReadWriteLock`/`AbstractQueuedSynchronizer` etc. The fixture
chain pulls in concurrency primitives that aren't ported to pylib.
Excludelist candidates if not actually needed for the fixture's logic;
otherwise port the missing types.

### Cluster 5 — `compile.other` (14 fixtures)

Compile errors not matching the structured patterns. Need per-fixture
look but mostly likely linker/encoding edges.

### Cluster 6 — `compile.unresolved.exception_ctor` (5 fixtures)

`OutOfMemoryError`/`IndexOutOfBoundsException`/`AssertionError`
constructor `<init>():V` missing. Tiny additions to pylib's exception
ports.

### Cluster 7 — `run.attr.subSequence` (5 fixtures)

`subSequence` method missing on `_scpy_String` (or whatever Python
class our String erases to). Probably a 5-line fix.

### Clusters 8–9 — `int2double__I__D` (4) and `test__I__I` (4)

Both are method-dispatch failures. `int2double__I__D` is a primitive
numeric cast helper missing from generated code. `test__I__I` is a
fixture-local method — likely an encoding mismatch between declaration
and call site. Need two specific investigations.

### Excludelist candidates (~7 fixtures)

`compile.unresolved.{sun,swing_awt,java_beans,java_security,java_invoke}`
— JVM-only surface area not portable to Python. Add to
`run-py-tests.excludelist` with reason tags.

## Spurious / non-cluster

- ✅ `t6888.scala` — `Duplicate class 'C___abc_'`. Resolved in
  `f1ed010ca4` (encoder `_scpy_d` escape so a non-module class
  whose source name ends in `$` cannot alias onto a module class
  slot, plus a `byName`-aware gate on the synthetic-forwarder
  emission so an iteration-order race no longer doubles the
  companion's encoded name).
- ✅ `main-functions.scala` — non-deterministic `@main` selection.
  Resolved in `a23ecd6c3b` (deterministic
  `PyCodeGenSupport.pickMainEntry` replaces the last-class-wins
  overwrite at `GenPython.scala:263-264`).

## Reproducing

```bash
# Inputs
ls /tmp/pyrun-analysis-postperfc/bucket-*.log
cat /tmp/pyrun-analysis-postperfc/failed-fixtures-v2.txt

# Re-extract clusters
python3 /tmp/postperfc-fingerprints/extract2.py
cat /tmp/postperfc-fingerprints/all.txt | cut -d'|' -f2 | sort | uniq -c | sort -rn
```
