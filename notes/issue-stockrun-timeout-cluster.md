# PyStockRunTests 60s timeout cluster — root cause analysis

Investigation of the 14 fixtures listed in `notes/layer6-sweep.md` under
"60s subprocess timeout". Sweep date: 2026-05-01 against HEAD `fd4c8daedc`.
Investigation date: 2026-05-02 against HEAD `c74824700a`.

The 14 fixtures split cleanly into **two root causes**, with some
follow-on perf observations:

Per-fixture status from a 75s-cap rerun on 2026-05-02 against HEAD `c74824700a`:

| Fixture | 75s exit | Wall (s) | Output check | Cluster |
|---|---:|---:|---|---|
| `i14693.scala`        | 124 | 75 | hang, no output      | **Bug A** |
| `t493.scala`          | 124 | 75 | hang, no output      | **Bug A** |
| `t2755.scala`         | 124 | 75 | 5 of 21 prints, hang | **Bug A** |
| `array-erasure.scala` | 124 | 76 | hang, no output      | **Bug A** |
| `i10930.scala`        | 124 | 75 | hang, no output      | **Bug A** |
| `i20145.scala`        |   0 | 52 | matches `.check`     | **Perf B** (correct, exceeds 60s harness) |
| `t6584.scala`         |   0 | 63 | matches `.check`     | **Perf B** (correct, exceeds 60s harness) |
| `UnrolledBuffer.scala`|   0 | 66 | (asserts only, no stdout) | **Perf B** (correct, exceeds 60s harness) |
| `t2818.scala`         |   0 | 43 | matches `.check`     | **Perf B** (borderline; may pass at 60s with low load) |
| `caseClassHash.scala` |   0 | <1 | wrong Murmur3 hash   | numeric-hash cluster (`layer6-assertion-failures.md`) |
| `t8893.scala`         | 124 | 75 | hang, no output      | **Perf B'** (still timeout at 75s) |
| `t3502.scala`         | 124 | 75 | hang, no output      | **Perf B'** (still timeout at 75s) |
| `collections.scala`   | 124 | 76 | 7 of 10 collections done | **Perf B'** (still timeout at 75s) |
| `kmpSliceSearch.scala`| 124 | 75 | hang, no output      | **Perf B'** (still timeout at 75s) |

Buckets:
- **5 fixtures (Bug A)** flip from a single fix in
  `library-py/src/scala/runtime/ScalaRunTime.scala`.
- **3 fixtures (Perf B)** are correctness-clean and pass at 75s but exceed
  the harness's 60s cap; `t2818` is right at the edge (43s local, was
  timing out under harness load).
- **4 fixtures (Perf B')** still time out at 75s — genuinely too slow
  for any reasonable harness budget under CPython interpretation.
- **1 fixture (`caseClassHash`)** is no longer a timeout — it completes
  in <1s but produces a wrong Murmur3 hash. The earlier `Warmup.` partial
  output was a snapshot from before commit `fd4c8daedc` ("PyLinker: drop
  Support-main fallback in collectMainEntry"); pre-fix the harness ran
  `Timing.main` (the 100M-iter benchmark) instead of `Test.main`. At
  current HEAD `Test.main` is selected, and the remaining failure is the
  case-class hashCode mismatch tracked in `layer6-assertion-failures.md`.

## Bug A — `ScalaRunTime.isArray` infinite recursion

### Symptom

Five fixtures hang for the full 60s harness window with **no partial
output**. Tracing CPython frame calls reveals a steady cycle:

```
ScalaRunTime_.isArray            ← while True: with tail-call label
  BoxesRunTime.equals(a, None)   ← null check
  PyBuiltins.equal(a, None)      ← operator.eq
  raise _scpy_lbl_1()            ← tail-call jump
  except _scpy_lbl_1: pass       ← loop continuation
```

~12.7M iterations in 20s. The body never produces a result.

### Root cause

`compiler/src/dotty/tools/dotc/transform/TypeTestsCasts.scala:337-340`
rewrites every `expr.isInstanceOf[Array[?]]` (where the element type is
generic) into:

```scala
ref(defn.runtimeMethodRef(nme.isArray)).appliedTo(arg, Literal(Constant(ndims)))
```

i.e. a call to `scala.runtime.ScalaRunTime.isArray(arg, ndims)`.

Our pylib override at
`library-py/src/scala/runtime/ScalaRunTime.scala:112-113` reads:

```scala
def isArray(a: Any, atLevel: Int = 1): Boolean =
  a != null && a.isInstanceOf[Array[?]]
```

The `&& a.isInstanceOf[Array[?]]` re-enters `ScalaRunTime.isArray(a, 1)`
during erasure → infinite recursion turned into infinite tail-call loop.
The compiler's tail-call rewrite + the Python codegen's
label-class-in-while-loop pattern means each iteration:
1. Checks `a != null`
2. Sets `atLevel := 1`
3. Raises `_scpy_lbl_1` (tail jump)
4. Catches, restarts loop

No exit path. `&&`'s short-circuit RHS was supposed to be the exit but
got rewritten into a recursive `isArray(a)` call.

### Fix shape

The override needs an array test that does **not** go through any
type-test-rewrite path. Two correct options:

**Option 1 — primitive runtime helper.** Add `_scpy_is_array(value)` to
the `PyIRRuntime` prelude:

```python
def _scpy_is_array(value):
    return isinstance(value, _scpy_Array)
```

Bridge from Scala via `@extern("__main__", "_scpy_is_array")`, mirroring
the existing `_scpy_simple_name_of` / `class_simple_name` pattern in
`pylib-py/src/scala/python/runtime/PyBuiltins.scala`. The override
becomes:

```scala
def isArray(a: Any, atLevel: Int = 1): Boolean =
  a != null && PyBuiltins.isArray(a)  // calls _scpy_is_array via @extern
```

For `atLevel > 1` (multi-dim arrays), peek at component type via
`_scpy_Array._scpy_class._scpy_component_type` and recurse — no
`isInstanceOf[Array[?]]` involved.

**Option 2 — `getClass.isArray`.** Mimic upstream JVM stdlib:

```scala
def isArray(a: Any, atLevel: Int = 1): Boolean =
  a != null && isArrayClass(a.getClass, atLevel)

private def isArrayClass(c: jClass[?], atLevel: Int): Boolean =
  c.isArray && (atLevel == 1 || isArrayClass(c.getComponentType, atLevel - 1))
```

This matches the upstream stdlib exactly. Requires `_scpy_Class.isArray`
and `_scpy_Class.getComponentType` to behave correctly for our wrapped
arrays. The runtime already exposes both (`PyIRRuntime.scala:805` and
`:1141`); they should work but need a quick verification.

Option 2 is preferable because it removes our deviation from the upstream
stdlib body and reuses existing class-introspection plumbing. Option 1 is
the fastest patch if `_scpy_Class.isArray` turns out to need fixing.

### Validating fixtures

Status after applying Option 2 (verified 2026-05-02):

| Fixture | Before | After | Notes |
|---|---|---|---|
| `tests/run/t493.scala`          | timeout 75s | exit=0 in <1s | flips clean |
| `tests/run/t2755.scala`         | timeout 75s, 5/21 prints | exit=0 in <1s, full `.check` match | flips clean |
| `tests/run/array-erasure.scala` | timeout 76s | exit=0 in 1s | flips clean |
| `tests/run/i10930.scala`        | timeout 75s | exit=0 in <1s | flips clean |
| `tests/run/i14693.scala`        | timeout 75s | exit=0 in <1s, **wrong output** (`Failure!` vs `Success!`) | residual bug — see below |

**i14693 residual: `Array[Long]` element loses Long type tag.**
Reading an element of a primitive `Array[Long]` returns a raw Python
int, which `_scpy_class_of_instance` classifies as `java.lang.Integer`.
The pattern `case Array(i: Long)` therefore misses, and the match falls
through to `case _ => "Failure!"`. Confirmed via:

```scala
val a: Array[Long] = Array(1L)
val any: Any = a(0)
println(any.isInstanceOf[Long])           // false (should be true)
println(any.getClass.getName)             // java.lang.Integer (should be java.lang.Long)
```

Likely fix site: `_scpy_Array.__getitem__` already special-cases the
`char[]` boxing path (`PyIRRuntime.scala:1248-1252`); extend the same
pattern to box reads from primitive `long[]`/`byte[]`/`short[]`/`int[]`
arrays back to their `java.lang.*` boxed types — at least when the
element flows into a value that needs to satisfy `isInstanceOf[Long]`.
This is a different bug from the isArray loop and warrants its own
issue note before action.

### Regression guard

Add `tests/pos-py/array-isinstance.scala`:

Landed at `tests/pos-py/array-isinstance.scala` — exercises the
top-level rewrite path (`xs.isInstanceOf[Array[?]]`) and the direct
helper (`ScalaRunTime.isArray(_, atLevel)`) for Array, non-Array, null,
and a 2D array. Avoids the residual primitive-element bug by not
relying on `case Array(i: Long)` extractors. `.check` was generated
from the actual post-fix output (10 lines).

## Perf B — borderline fixtures (correct, 43–66s)

Three fixtures complete with the right output but exceed the harness's
hard 60s `runProcess` cap (`py-compiler-tests/test/scala/dotty/tools/dotc/PyRun.scala:16`,
`maxDuration: Duration = Duration.Inf` per call but invoked at 60s by
`ScalaPyTestSuite`); a fourth is right at the edge:

| Fixture | Wall (75s rerun) | Hot loop |
|---|---:|---|
| `i20145.scala`        | 52s | 10M-iter tailrec via `return` inside lambda |
| `t6584.scala`         | 63s | 4 × 100K-element `tabulate` + `.distinct` over Array/Vector/List/LazyList |
| `UnrolledBuffer.scala`| 66s | ~10× `assertCorrect(u1)` on N=1000 (each pass: 4×1000 `apply`/`update` + map+iterator+toSeq compare) |
| `t2818.scala`         | 43s | 1M-iter `List.foldRight` (borderline; may pass under low CI load) |

`caseClassHash.scala` was in the original timeout list but completes in
<1s now. It's a different failure class (wrong Murmur3 hash); see the
table above.

## Perf B' — fixtures that exceed even 75s

Four fixtures still time out at 75s. These won't be rescued by a
modest harness-budget bump:

| Fixture | Hot loop |
|---|---|
| `t8893.scala`         | 2 × `tick(10000000)` — 20M tail-recursive iterations |
| `t3502.scala`         | LazyList prime sieve up to ~1.4M with per-element `n % _ == 0` check |
| `collections.scala`   | 10 collection types × (`s ++ List.range(0, 5000)` + 10001-element contains check) |
| `kmpSliceSearch.scala`| 100×10×5 nested `indexOfSlice`/`lastIndexOfSlice` over 4 backings, dominated by `_scpy_is_assignable` class-hierarchy walks (~16M+ frame entries observed in 15s) |

## Perf C (related codegen win) — hoist `_scpy_lbl_1` out of `while True`

The Python codegen emits the tail-jump label class **inside** the loop:

```python
def foo(self, i: int) -> int:
    i_tailLocal1 = i
    while True:
        class _scpy_lbl_1(BaseException):           # ← redefined each iteration
            __slots__ = ("value",)
            def __init__(self, v=None): self.value = v
        try:
            ...
            raise _scpy_lbl_1()
        except _scpy_lbl_1:
            pass
```

CPython runs the `class` body — including `__init__` def — every
iteration. For 10M iterations that's 10M class objects created and
discarded. Hoisting it once per function would halve `i20145`'s wall
time, taking it from 51s (borderline-pass) to ~25s, and would reduce
overhead in every tail-recursive method emitted by `GenPython`.

Site to fix: search for `class _scpy_lbl_` in `PyIREmitter.scala`
output. The label must be emitted at the surrounding function/method
scope, not inside the `while True:` body. `_scpy_lbl_1` is method-local
already (different methods get different label classes), so hoisting one
level outward is safe.

This perf fix is independent of Bug A — both can land in one PR.

## Recommended sequencing

1. **Bug A fix** in `library-py/.../ScalaRunTime.scala`. Five fixtures
   (`i14693`, `t493`, `t2755`, `array-erasure`, `i10930`) flip from
   timeout to pass with no source edits. Adopt Option 2 (upstream's
   `getClass.isArray` body) — the runtime already has the support. Add
   the `tests/pos-py/array-isinstance.scala` regression fixture above.
2. **Perf C codegen win** (hoist `_scpy_lbl_1` out of `while True:`).
   Independent of Bug A. Halves the per-iteration cost of every emitted
   tail-recursive method. Likely flips `i20145` (52s → ~25s) and shaves
   ~10–20% off `UnrolledBuffer` and `t6584`. Won't help the
   `_scpy_is_assignable`-bound `kmpSliceSearch` or the genuinely-large-N
   `t8893`/`t3502`/`collections`.
3. **Excludelist Perf B'** with reason tags in
   `py-compiler-tests/test/run-py-tests.excludelist`:
   ```
   tests/run/t8893.scala          # 20M tailrec iterations, perf-bound under CPython
   tests/run/t3502.scala          # 1.4M-element LazyList prime sieve
   tests/run/collections.scala    # 10 collection types × 5K-iter test loops
   tests/run/kmpSliceSearch.scala # 100×10×5 nested KMP, dominated by class-hierarchy walks
   ```
4. **Decide on Perf B**. After Perf C, `i20145`/`t2818` should pass at
   60s. `t6584`/`UnrolledBuffer` will probably still need ~50–60s — a
   90s harness cap (instead of 60s) for the run-py suite would cover
   them; a path-prefix override is overkill. Either bump the cap to 90s
   (cost: real hangs take 30s longer to surface), or excludelist
   `t6584`/`UnrolledBuffer` with the same reason tags as Perf B'.

## Out of scope

- **caseClassHash output mismatch** (wrong Murmur3 hash) is the
  numeric-hash cluster from `layer6-assertion-failures.md`. Don't
  re-investigate here.
- The `class _scpy_lbl_N` hoist (Perf C) helps every tailrec-emitted
  method, not just timeout fixtures. It's a general codegen improvement;
  scope it as a small standalone PR if Bug A lands first.
