# PyRunTests fix roadmap

This roadmap orders the issues found in `/tmp/pyrun-analysis/SUMMARY.md` into
**six layers**, lowest first. Each layer is independently shippable and has
its own verification step. Layer N's verification is a precondition for
starting Layer N+1.

The notes referenced below all live in `notes/issue-*.md`.

---

## Layer 0 — Harness foundations (fix infrastructure first)

Goal: make the test harness reliable enough to do the rest of the work
without manual subprocess kills, OOM-driven worker reductions, or arbitrary
20-min cutoffs. Without this, every subsequent layer's verification is
unreliable.

| # | Issue | Note |
|---|-------|------|
| 0.1 | PyRun.runProcess swallows `InterruptedException`, leaks workers | `issue-pyrun-interrupt-swallow.md` |
| 0.2 | Vulpix 20-min `executeTestSuite` cap truncates the sweep | `issue-vulpix-test-suite-timeout.md` |
| 0.3 | PyReachability heap pressure on long sweeps | `issue-pyreachability-heap-pressure.md` |

**Verification (after Layer 0)**:

1. Run the full PyRunTests sweep once with no exclusions and no manual
   process kills:
   ```bash
   sbt --client "pyCompilerTests/testOnly dotty.tools.dotc.PyRunTests"
   ```
2. Expect: all 1708 fixtures complete (or are individually timed out and
   marked failed); no hung Python subprocesses left after sbt exits;
   `testlogs/last-failed.log` is non-empty and contains the actual failure
   list. Either `numberOfWorkers = 5` works again at default heap, or the
   `Xmx`/worker tuning is no longer load-bearing. Run-time should land in
   the 25–40 min range.
3. Save a baseline failure list as `/tmp/pyrun-analysis/baseline-failed.log`
   for diffing against later layers.

If 0.3 turns out to be deeper than expected, ship 0.1 and 0.2 alone — they
restore the ability to *finish* a sweep (with the existing
`-Xmx16g`/`workers=2` workarounds), which is enough to validate later
layers.

---

## Layer 1 — Linker / classpath (single biggest unblock)

Goal: stop the dominant compile-failure cascade so we can see the real
runtime failures hiding behind it.

| # | Issue | Note | Impact |
|---|-------|------|--------|
| 1.1 | `java.util.jar.Attributes.Name` missing — blocks ~80 fixtures | `issue-attributes-name-cascade.md` | ~80 fixtures |
| 1.2 | Port `java.util.WeakHashMap` to pylib | `issue-pylib-weakhashmap-port.md` | small |
| 1.3 | Port `java.io.PushbackReader` to pylib | `issue-pylib-pushbackreader-port.md` | small |

**Verification (after Layer 1)**:

1. Rebuild support jars (mandatory after pylib changes):
   ```bash
   sbt --client "scala-pylib-py/clean; scala-pylib-py/Compile/packageBin; \
                 scala-library-py/clean; scala-library-py/Compile/packageBin"
   ```
2. Re-run the sweep and diff against the baseline:
   ```bash
   sbt --client "pyCompilerTests/testOnly dotty.tools.dotc.PyRunTests"
   diff /tmp/pyrun-analysis/baseline-failed.log testlogs/last-failed.log
   ```
3. Expect: the diff shows ~80 fixtures going from failing to passing (the
   `Attributes_Name` cohort). No new failures introduced.

If a smaller cohort flips, inspect the still-failing fixtures — they may
have a *different* root cause that was masked by the linker error.

---

## Layer 2 — Codegen correctness (the high-impact bugs)

Goal: fix the codegen patterns that cause runtime errors in otherwise
well-formed fixtures. These touch `compiler/src/dotty/tools/backend/python/`.

| # | Issue | Note | Impact | Status |
|---|-------|------|--------|--------|
| 2.1 | Anonfun static synthesis loses captured `self` | `issue-anonfun-static-self-unbound.md` | 6+ fixtures incl. Parser.scala | **Deferred to Layer 5** — broad heuristic regressed 215 pos-py fixtures; needs body-walking discriminator |
| 2.2 | `Object.getClass()` on Python primitives | `issue-getclass-on-primitives.md` | 2 fixtures | Landed |
| 2.3 | `_scpy_fn_specialized_forward` cyclic forward (Function0) | `issue-specialization-forward-recursion.md` | 1 fixture (potentially generic) | Landed |

**Verification (after Layer 2)**:

1. Re-run the sweep, diff against the post-Layer-1 baseline.
2. Expect: `matchable` and `string-switch` flip to passing; `t603` flips to
   passing. The 6+ NameError fixtures (`given-eta`, `i9507`, `i24201a`,
   `quoted-sematics-1`, `t7396`, `t7763`, `Parser`) remain failing — see
   the deferred 2.1 item.
3. As a sanity check, recompile and run the existing
   `tests/pos-py/` and raw-pylib suites to confirm no regression:
   ```bash
   sbt --client "pyCompilerTests/testOnly dotty.tools.dotc.ScalaPyCompilationTests"
   sbt --client "pyCompilerTests/testOnly dotty.tools.dotc.PylibTest"
   ```

---

## Layer 3 — Stdlib / pylib gaps (mid-impact runtime fixes)

Goal: close the remaining pylib/library-py holes that cause concrete
runtime errors on otherwise compile-clean fixtures.

| # | Issue | Note | Impact |
|---|-------|------|--------|
| 3.1 | `Throwable.getMessage` missing `message` field | `issue-throwable-message-missing.md` | 2 fixtures |
| 3.2 | `ObjectOutputStream(out)` constructor missing | `issue-objectoutputstream-no-args.md` | 5 fixtures |
| 3.3 | `scala.Enumeration.nextName` missing | `issue-enumeration-nextname-missing.md` | 3 fixtures |

**Verification (after Layer 3)**:

1. Rebuild support jars (per Layer 1 verification).
2. Re-run sweep; expect ~10 more fixtures flip to passing.
3. Spot-check serialization fixtures by manually running one bundled `.py`
   under `uv run` to confirm output matches the `.check`.

---

## Layer 4 — Excludelist housekeeping

Goal: stop wasting analysis time on fixtures whose features are out of
scope for the Python backend by design.

| # | Issue | Note | Impact |
|---|-------|------|--------|
| 4.1 | Reflection class introspection — 13 fixtures | `issue-reflection-class-introspection.md` | 13 fixtures (excluded, not fixed) |
| 4.2 | Concurrency / threading primitives (AbstractQueuedSynchronizer, ReentrantReadWriteLock, Thread_UncaughtExceptionHandler, ForkJoinPool) | (in `issue-reflection-class-introspection.md` cross-ref) | small |
| 4.3 | JDK internals: `sun.misc.Unsafe` | (linker-cat-B) | 1 fixture |
| 4.4 | Method-handle / lambda-metafactory: `MethodType`, `MethodHandle`, `SerializedLambda` | (linker-cat-B) | 1–2 fixtures |
| 4.5 | Swing: `javax.swing.AbstractListModel` | (linker-cat-B) | 1 fixture |

The full list of excludelist additions is enumerated in
`issue-reflection-class-introspection.md` (Cat E) and
`/tmp/pyrun-analysis/cat-b-other-linker.md` (Cat B linker excludelist).
Apply them all in one PR to
`py-compiler-tests/test/run-py-tests.excludelist`, each line carrying a
`# reason` tag.

**Verification (after Layer 4)**:

1. Re-run sweep. The excluded fixtures should be skipped (not counted).
2. Confirm the sweep's "remaining failure" set no longer contains any
   reflection / threading / Swing / MethodHandle / Unsafe entries.
3. The remaining failures are now the *real* backlog of Python-backend
   issues — no longer drowned in scope-mismatch noise.

---

## Layer 5 — Research / deferred (single-fixture or blocked work)

Goal: track and gradually resolve the lower-volume issues whose fixes are
deeper and don't have clear quick-win shapes.

| # | Issue | Note | Status |
|---|-------|------|--------|
| 5.1 | NoneType value-class accessor pattern (4 fixtures) | `issue-nonetype-value-class-accessor.md` | needs per-fixture inspection |
| 5.2 | `transparent-object` missing inherited member (1 fixture) | `issue-transparent-object-missing-method.md` | needs DCE inspection |
| 5.3 | `Mirror.SingletonProxy.fromProduct` (1 fixture) | `issue-mirror-singletonproxy-fromproduct.md` | likely library-py overlay gap |
| 5.4 | Lazy-implicit cache field (1 fixture) | `issue-lazy-implicit-cache-field.md` | **blocked by `notes/dce-improvement-plan.md`** |

**Verification (after Layer 5)**:

Per-fixture: confirm the targeted fixture flips to passing without
regressing others. Track each in its own commit so the bisect history is
clean.

---

## Layer 6 — Sweep again, find the next wave

Goal: pick up the patterns that were hiding behind the Layer-1 cascade.

After Layers 0–4 land, ~115 of the 132 failures we saw in run4 should be
resolved (80 from 1.1, 6 from 2.1, 2 from 2.2, 1 from 2.3, 2 from 3.1, 5
from 3.2, 3 from 3.3, ~13 from 4.1, plus the small Cat-B handfuls). The
remaining ~17 are Layer 5 work.

But run4 only completed 334 of 1708 fixtures. The other ~1370 will surface
their own patterns once the harness can finish. Schedule a fresh
end-to-end PyRunTests sweep after Layer 4 and rebuild the manifest from
the new failure list. Likely outcomes:

- High overlap with already-known categories (Attributes_Name was a
  universal blocker, so its absence in the new run will reveal the *next*
  most common cascade — possibly another stdlib class).
- New runtime-error patterns we didn't see in the first 334.
- A handful of new excludelist candidates.

Fold the new findings into a "wave-2" version of `notes/pyrun-roadmap.md`
and repeat the layered process: harness → linker → codegen → library →
excludelist → research → sweep again.

---

## Quick reference

| Layer | Theme | Verification |
|-------|-------|--------------|
| 0 | Harness foundations | full sweep finishes in 25–40 min, no manual kills |
| 1 | Linker / pylib gaps | ~80 fixtures flip to passing |
| 2 | Codegen correctness | 9+ fixtures flip; pos-py/Pylib unchanged |
| 3 | Stdlib / pylib runtime | ~10 fixtures flip |
| 4 | Excludelist housekeeping | scope-mismatch fixtures removed from failure list |
| 5 | Research / blocked | per-fixture progress, tracked individually |
| 6 | Wave 2 sweep | new manifest covers fixtures we couldn't reach in wave 1 |

Aggregated specialist evidence lives at `/tmp/pyrun-analysis/SUMMARY.md`.
