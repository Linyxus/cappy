# PyStockRunTests fix roadmap

This roadmap orders the issues found in the PyStockRunTests sweeps into
**six layers**, lowest first. Each layer is independently shippable and has
its own verification step. Layer N's verification is a precondition for
starting Layer N+1.

The notes referenced below all live in `notes/issue-*.md`.

---

## Layer 0 — Harness foundations ✅

Goal: make the test harness reliable enough to do the rest of the work
without manual subprocess kills, OOM-driven worker reductions, or arbitrary
20-min cutoffs.

| # | Issue | Status |
|---|-------|--------|
| 0.1 | PyRun.runProcess swallows `InterruptedException`, leaks workers | **Landed** |
| 0.2 | Vulpix 20-min `executeTestSuite` cap truncates the sweep | **Landed** (chunked `runPyTests_00`–`_15` with per-bucket sbt invocations) |
| 0.3 | PyReachability heap pressure on long sweeps | Open — `issue-pyreachability-heap-pressure.md` |

The bucketed harness lets the full 1708-fixture sweep finish without manual
kills; per-bucket sbt invocations keep heap pressure bounded.

---

## Layer 1 — Linker / classpath ✅

| # | Issue | Status |
|---|-------|--------|
| 1.1 | `java.util.jar.Attributes.Name` cascade | **Landed** |
| 1.2 | Port `java.util.WeakHashMap` to pylib | **Landed** |
| 1.3 | Port `java.io.PushbackReader` to pylib | **Landed** |

---

## Layer 2 — Codegen correctness

| # | Issue | Status |
|---|-------|--------|
| 2.1 | Anonfun static synthesis loses captured `self` | **Deferred to Layer 5** — `issue-anonfun-static-self-unbound.md` (broad heuristic regressed pos-py; needs body-walking discriminator) |
| 2.2 | `Object.getClass()` on Python primitives | **Landed** |
| 2.3 | `_scpy_fn_specialized_forward` cyclic forward (Function0) | **Landed** |

---

## Layer 3 — Stdlib / pylib gaps

| # | Issue | Status |
|---|-------|--------|
| 3.1 | `Throwable.getMessage` missing `message` field | **Landed** (originally a workaround rename; superseded by Layer 5 encoding fix) |
| 3.2 | `ObjectOutputStream(out)` write side | **Landed**; round-trip needs `ObjectInputStream` — `issue-objectoutputstream-no-args.md` |
| 3.3 | `scala.Enumeration.nextName` | **Deferred to Layer 5** — `issue-enumeration-nextname-missing.md` |

---

## Layer 4 — Excludelist housekeeping

| # | Issue | Status |
|---|-------|--------|
| 4.1 | Reflection class introspection (13 fixtures) | Open — `issue-reflection-class-introspection.md` |
| 4.2 | Concurrency / threading primitives | Open (cross-ref in `issue-reflection-class-introspection.md`) |
| 4.3 | `sun.misc.Unsafe` | Open |
| 4.4 | Method-handle / lambda-metafactory | Open |
| 4.5 | Swing | Open |

Apply all excludelist additions in one PR to
`py-compiler-tests/test/run-py-tests.excludelist`, each line carrying a
`# reason` tag.

---

## Layer 5 — Research / deferred

| # | Issue | Status |
|---|-------|--------|
| 5.1 | NoneType umbrella (split per fixture) | Most landed; **5.1.d** and **5.1.e** open — `issue-nonetype-value-class-accessor.md` |
| 5.1.a | `AttributeError` from None-receiver dispatch → `NullPointerException` (`exceptions-2`) | **Landed** (Wave 3) |
| 5.1.b | Function specialization bridge does not unbox `null` (`lambda-null`) | **Landed** (Wave 2) |
| 5.1.c | Overloaded `equals` collapses to `__eq__` (`numbereq`) | **Landed** (Wave 1) |
| 5.1.d | `Seq[Char].##` disagreement across backings (`t4122`) — DCE drops inherited dunder | **Landed** |
| 5.1.e | `null.asInstanceOf[Primitive]` does not unbox (`lambda-null` residual) | **Landed** |
| 5.2 | `transparent-object` missing inherited member | **Landed** (cross-class This handler in `genNormalApply`) |
| 5.3 | `Mirror.SingletonProxy.fromProduct` | **Landed** — handwritten `Mirror_*` runtime methods used a placeholder `__O` suffix instead of the erased `__Ljava_dlang_dObject` shape; renamed to match call sites |
| 5.4 | Lazy-implicit cache field | Open — `issue-lazy-implicit-cache-field.md` (blocked by `dce-improvement-plan.md`) |
| 5.5 (encoding) | Owner-aware private-field mangling + `__`-prefix method guard | **Landed** — fixed CommandLineParser regression and t6888 inner-class accessor mangle in one shot |

**New surfacings (post-encoding-fix):**

| # | Issue | Status |
|---|-------|--------|
| 5.6 | `Class.getResourceAsStream` missing on `_scpy_Class` runtime | **Landed** — null-returning stubs for `getResourceAsStream`/`getResource` (matches JVM "absent resource" path; Properties.scalaProps falls through to defaults) |
| 5.7 | `override val` ctor-order collision under JVM-style chaining | **Landed** — `genConstructor` reorders super/this-ctor call to first; pos-py guard `override-val-ctor-order` |

---

## Layer 6 — Sweep again, find the next wave

Once Layers 0–5 trickle down, schedule a fresh end-to-end PyStockRunTests
sweep and rebuild the failure manifest. Layer 0.2's bucketed harness now
makes this trivial to repeat.

---

## Quick reference

| Layer | Theme | Status |
|-------|-------|--------|
| 0 | Harness foundations | 0.1, 0.2 landed; 0.3 open |
| 1 | Linker / pylib gaps | All landed |
| 2 | Codegen correctness | 2.2, 2.3 landed; 2.1 deferred |
| 3 | Stdlib / pylib runtime | 3.1, 3.2 (write) landed; 3.2 (read) and 3.3 open |
| 4 | Excludelist housekeeping | Pending PR |
| 5 | Research / blocked | Most landed; 5.4 (lazy-implicit cache, blocked) remains |
| 6 | Wave 2 sweep | Re-run after Layer 4 lands |
