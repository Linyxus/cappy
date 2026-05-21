# bench-py

Self-contained micro-benchmarks for the experimental **Python backend** of the
Scala 3 compiler (`-scalapy`). Each benchmark is a small Scala program compiled
to Python and timed with [`pyperf`](https://pyperf.readthedocs.io/).

The aim of this mirco-benchmark suite is for developping an optimization pass on
the Python IRs.

## Quick start

```bash
# From the repo root. Builds the support jars on first use if missing.
sbt "bench-py/runBenchPy"
```

This compiles every benchmark in the catalog through `bin/spc`, runs each
through pyperf, and writes the results (see [Output](#output)).

The full catalog is large (~90 benchmarks × several ops × several sizes), so a
full run takes a long time. While iterating, restrict and shorten it:

```bash
# Only the `mathalg` category, with a tiny pyperf budget.
BENCH_QUIET=1 BENCH_VALUES=2 BENCH_MIN_TIME=0.1 BENCH_FILTER=mathalg sbt "bench-py/runBenchPy"
```

### Configuration (environment variables)

All optional; defaults shown.

| Var | Default | Meaning |
| --- | --- | --- |
| `BENCH_PROCESSES`   | `1`  | pyperf `--processes` (worker subprocesses) |
| `BENCH_WARMUPS`     | `3`  | pyperf `--warmups` |
| `BENCH_VALUES`      | `5`  | pyperf `--values` (measurements per process) |
| `BENCH_MIN_TIME`    | `1`  | pyperf `--min-time` in seconds (fractional ok, e.g. `0.1`) |
| `BENCH_INNER_LOOPS` | `10` | manual unroll factor inside the timing function |
| `BENCH_FILTER`      | —    | substring match against a benchmark's `<category>.<ClassName>` id |
| `BENCH_QUIET`       | —    | any non-empty value passes `--quiet` to pyperf |

## Output

A run writes (paths relative to the repo root):

| File | Contents |
| --- | --- |
| `notes/bench-py.md`                    | Markdown table: `op/s` per `bench.op` (largest size). **Overwritten** each run. |
| `bench-py-results.jsonl`               | One JSON row per `bench.op@size`. **Overwritten** each run. |
| `bench-py/target/pyperf-results/all.json` | Raw pyperf `BenchmarkSuite`. **Deleted at the start** of each run. |
| `bench-py/target/py-bundles/<cat>-<Name>/<Name>.py` | The compiled Python bundle that actually ran. |

"op/s" is throughput = `1 / mean_per_call_latency`, where one "call" is one
invocation of the operation closure at the configured `size` (the whole loop,
not per element).

### Comparing two commits (e.g. before/after an optimizer pass)

There is intentionally no flag-based A/B harness. Compare across commits using
pyperf's built-in significance-tested comparison — just save the raw suite
before it's overwritten:

```bash
git checkout <baseline>;  sbt "bench-py/runBenchPy" && cp bench-py/target/pyperf-results/all.json /tmp/base.json
git checkout <optimized>; sbt "bench-py/runBenchPy" && cp bench-py/target/pyperf-results/all.json /tmp/opt.json
uv run --project . --no-sync python -m pyperf compare_to /tmp/base.json /tmp/opt.json
```

## Domains

| Category | # | What it covers |
| --- | --- | --- |
| `numeric`     | 1 | Integer hot loops — sum, multiply-accumulate, div/mod |
| `recursion`   | 2 | Method-call recursion vs iteration; `@tailrec` |
| `patmat`      | 1 | Sealed-trait ADT walk — `eval` + `render` |
| `closures`    | 1 | Higher-order combinators over a `Vector` |
| `strings`     | 1 | `StringBuilder`, interpolation, `mkString` |
| `collections` | 1 | Basic immutable `List` / `Vector` ops |
| `mathalg`     | 8 | Numeric & math algorithms — sieve, GCD, modpow, integer sqrt, bit-twiddling, matrix multiply, Collatz, dot/prefix |
| `sorting`     | 7 | Sorting & searching — insertion/quick/merge/counting sort, binary/linear search, stdlib `.sorted`, record sorts |
| `datastruct`  | 7 | User-defined data structures — cons list, BST, heaps, union-find, persistent stack, ring buffer |
| `graphdp`     | 8 | Graph traversal & dynamic programming — BFS/DFS, Floyd–Warshall, knapsack, edit distance, coin change, LIS, union-find |
| `text`        | 7 | Text processing — char histogram/scan, reverse, Caesar cipher, run-length encode, substring search, word split |
| `pipelines`   | 8 | Functional collection pipelines — map/filter/fold, lazy `view` vs strict, `flatMap`, `groupBy`, `zip`, `scanLeft`/`sliding`, partition+sort, for-comprehensions |
| `interp`      | 7 | Interpreters / VMs / state machines — stack VM, bytecode VM, recursive-descent parser, Brainfuck, NFA matcher, FSM, cellular automaton |
| `dispatch`    | 8 | Polymorphism & virtual dispatch — monomorphic vs megamorphic, trait defaults, deep inheritance, visitor, function vs method, mixins, decorator chains, `isInstanceOf` |
| `errorflow`   | 7 | `Option`/`Either`/`Try` & error-handling control flow — monadic chains, validation, parsing, throw/catch vs value-wrappers, `boundary`/`break` |
| `hasheq`      | 8 | Hashing, equality & ordering — case-class `hashCode`/`equals`, hand-rolled hash, case-class-keyed sets/maps, custom `Ordering`, `groupBy`, dedup |
| `controlflow` | 8 | Control-flow & iteration lowering — `while` vs `foreach` vs `foldLeft` vs `@tailrec`, range strides, early exit, nested loops, match-in-loop, non-local return, `boundary`/`break` |

Many `dispatch`, `controlflow`, `errorflow`, `pipelines`, and `hasheq` benches
deliberately group equivalent-but-differently-expressed variants in one op map
(e.g. monomorphic vs megamorphic, `while` vs `foreach`, exception vs `Either`),
so a single run gives a direct before/after-style comparison.

## Checking benches without a full timing run

Two helper scripts run the heavyweight compiler far less than a full pyperf run
and surface compile/runtime errors quickly:

```bash
bench-py/sweep_compile.sh [category-filter]   # compile every bench, report PASS/FAIL
bench-py/sweep_all.sh                          # compile + call every op once (OK/EXC), parallel
```

`sweep_all.sh` uses `python-shim/survey_run.py`, which calls each op once under a
per-op `SIGALRM` timeout (`BENCH_TIMEOUT`, default 20s) so a non-terminating op
is reported instead of hanging. Results land in `/tmp/benchsweep/` (ephemeral).

## Layout

```
bench-py/
  driver-src/
    Driver.scala       # JVM driver: compile each bench, run pyperf, aggregate
    Catalog.scala      # the benchmark registry
  python-shim/
    run_bench.py       # pyperf entry point (one bundle -> bench_time_func per op,size)
    results_to_md.py   # pyperf JSON -> notes/bench-py.md + bench-py-results.jsonl
    survey_run.py      # correctness survey: call each op once under a timeout
  src/main/scala/dotty/tools/benchmarks/py/<category>/*.scala
  sweep_compile.sh     # compile-only sweep
  sweep_all.sh         # compile + runtime-survey sweep
```
