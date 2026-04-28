# Vulpix's hard-coded 20-min `executeTestSuite` cap truncates large sweeps

## Minimal example for reproducing

```bash
sbt --client "pyCompilerTests/testOnly dotty.tools.dotc.PyRunTests"
```

With ~1705 fixtures, 2 workers, and ~5–8s per fixture (compile → link →
`uv run python` → diff), the full sweep needs ~30–35 min. Vulpix kills the
suite at exactly 20 min.

## Output vs Expected Behaviour

```
java.util.concurrent.TimeoutException: Compiling targets timed out,
  remaining targets: tests/run/virtpatmat_literal.scala, tests/run/Parser.scala,
  tests/run/defunctionalized.scala, tests/run/deadlock.scala, ...
  [HUGE LIST OF UNSTARTED FIXTURES]
  took 1249.207 sec
```

Expected: all fixtures run to completion and a normal pass/fail report is
emitted.

## Quick Analysis

`compiler/test/dotty/tools/vulpix/ParallelTesting.scala:759` (approximately,
inside `executeTestSuite`) has a hard-coded `pool.awaitTermination(20,
TimeUnit.MINUTES)`. The cap is fixed regardless of the test corpus size.

This is fine for the JVM backend's smaller per-suite groups (a few hundred
fixtures), but the Python backend's full `tests/run/` sweep is much larger
than any existing single test method, and we're using a single
`@Test def runPyTests` for the whole thing.

**Fix shape — pick one**:

1. **Chunked `@Test def`s (recommended)**. Split `tests/run/` into multiple
   test methods in `PyRunTests`, each covering a lexicographic slice
   (`runPyTests_a_d`, `runPyTests_e_h`, ..., `runPyTests_u_z`). Each gets
   its own 20-min budget. The slicing primitive is `compileFilesInDir`
   with a custom `FileFilter` that accepts only fixtures matching the
   slice prefix. Each slice runs to completion or hits its own timeout.

2. **Bump the Vulpix cap**. One-line change at
   `ParallelTesting.scala:759` to make the timeout configurable
   (e.g. read from a system property or scale with input size). Less
   invasive but masks real hangs.

3. **Per-method timeout via Vulpix configuration**. If
   `ParallelTesting.maxDuration` is per-fixture (60s in our case) and the
   suite-level cap is `numberOfWorkers × maxDuration × sourceCount /
   numberOfWorkers + overhead`, the cap could be derived rather than
   constant.

Recommend option 1 because it preserves the timeout guard against actual
hangs while letting large suites finish. The PyRunTests file would gain
~6 short test methods that all share the existing infrastructure.

Specialist report: `/tmp/pyrun-analysis/harness-issues.md` (Issue #3).
