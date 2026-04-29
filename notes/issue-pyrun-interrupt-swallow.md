# `PyRun.runProcess` swallows `InterruptedException`, leaks workers on hangs

## Minimal example for reproducing

Any fixture whose generated Python hangs (e.g. infinite loop, uncaught
`@tailrec` blow-up, very slow nested-loop). The three currently excluded:

- `tests/run/i14693.scala` — `Array[Long]` pattern match.
- `tests/run/kmpSliceSearch.scala` — nested-loop search.
- `tests/run/i20145.scala` — tailrec to 10_000_000.

```bash
sbt --client "pyCompilerTests/testOnly dotty.tools.dotc.PyStockRunTests"
# Watch a worker get stuck on one of the above; Vulpix never reclaims it.
```

## Output vs Expected Behaviour

Vulpix's `maxDuration = 60s` per-fixture timeout fires, the worker thread is
interrupted, but the Python child process keeps running. The harness reports
the test as still in flight forever; eventually all `numberOfWorkers` slots
are exhausted, throughput drops to zero, and the run only ends when an
operator kills the process tree by hand.

Expected: the timeout kills the child, the test is marked failed, the worker
slot is reclaimed, and the run continues.

## Quick Analysis

`py-compiler-tests/test/scala/dotty/tools/dotc/PyRun.scala:113-119`:

```scala
var interrupted = false
var done        = false
while !done do
  try
    exitCode = process.waitFor()
    done = true
  catch
    case _: InterruptedException => interrupted = true
if interrupted then Thread.currentThread.nn.interrupt()
```

The loop catches `InterruptedException` and re-loops `waitFor()` without
killing the subprocess. The comment explains it as a defense against
"ForkJoinPool can interrupt workers in blocking I/O while reshuffling near
pool shutdown" — i.e. a defense against spurious interrupts. Unfortunately
it conflates spurious interrupts with legitimate Vulpix-timeout cancellation.

**Fix shape**: track an absolute deadline using `System.nanoTime()`. On the
first `InterruptedException`:
1. call `process.destroy()` (sigterm-equivalent);
2. give the child a short grace period (e.g. 2s) via
   `process.waitFor(2, TimeUnit.SECONDS)`;
3. if still alive, call `process.destroyForcibly()` (sigkill-equivalent).
4. preserve the interrupt flag so upstream sees the cancellation.

This kills the hung child on legitimate timeouts while still tolerating
spurious ForkJoinPool interrupts (the grace-period wait re-checks the
process state and exits cleanly if nothing was actually wrong).

Specialist report: `/tmp/pyrun-analysis/harness-issues.md` (Issue #1).
