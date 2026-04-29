# `PyReachability$Analyzer` exhausts heap on long sweeps

## Minimal example for reproducing

```bash
# With the default ScalaPyTestSuite numberOfWorkers = 5 and JVM default heap:
sbt --client "pyCompilerTests/testOnly dotty.tools.dotc.PyStockRunTests"
```

Reliably OOMs around fixture ~340/1700.

The current PyStockRunTests workaround:
- `Test / javaOptions += "-Xmx16g"` in `project/Build.scala` for
  `pyCompilerTests`;
- `numberOfWorkers = 2` override in
  `py-compiler-tests/test/scala/dotty/tools/dotc/PyStockRunTests.scala`.

Even with that, run4 hit the OOM during a particular fixture's link phase
(`tests/run/i6219.scala` and `runtime-richChar.scala`).

## Output vs Expected Behaviour

```
java.lang.OutOfMemoryError: Java heap space: failed reallocation of scalar
  replaced objects
  at dotty.tools.backend.python.PyReachability$Analyzer.resolveInstanceMethod
    (PyReachability.scala:523)
  at dotty.tools.backend.python.PyReachability$Analyzer.instantiate
    (PyReachability.scala:341)
  ...
```

Expected: the linker phase for each fixture peaks at a bounded, modest heap
footprint (similar to the JVM backend's per-CU peak). 5 parallel linker runs
should fit comfortably in a few GB.

## Quick Analysis

`PyReachability$Analyzer` retains transitive state per fixture:

- **`directDescendants`** map (PyReachability.scala lines 198-203) is
  precomputed eagerly at `Analyzer` construction and held for the lifetime
  of the analyzer.
- **`state`** HashMap (line 171) holds a `MutableState` for every reachable
  class. Each `MutableState` contains four nested HashSets: `reachableMethods`,
  `readFields`, `writtenFields`, `virtualCallLog`.
- **`ancestorsOf()`** (lines 209-223) is uncached and rebuilds the transitive
  closure on every call from `logVirtualCall()` (lines 397-449).

For 1700 fixtures running in parallel, if each `Analyzer` instance is not
promptly released after `analyze()` returns, the per-class `MutableState`
sets accumulate across fixtures. Even if each is released, the per-fixture
peak (especially the uncached `ancestorsOf()` work-set) is large enough that
5 simultaneous analyzers OOM the test fork.

**Fix shape**:
1. Memoize `ancestorsOf()` at the `Analyzer` level — turns repeated O(n²)
   transitive walks into a single O(n) precomputation.
2. Verify that `Analyzer` and its `state` map are dropped after
   `analyze()` returns: walk `PyReachability.Result` and confirm it doesn't
   pin a back-reference to the analyzer.
3. Consider lazy / on-demand computation of `directDescendants` instead of
   eager construction.
4. Profile peak heap before vs after one fixture's linker phase to confirm
   the analyzer is actually released.

If those reduce per-fixture peak sufficiently, the `numberOfWorkers = 2`
and `-Xmx16g` workarounds in PyStockRunTests can be reverted.

Specialist report: `/tmp/pyrun-analysis/harness-issues.md` (Issue #2).
