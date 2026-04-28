# CLAUDE.md

This file provides guidance to coding agents when working with code in this repository.

## Project focus

This branch develops an experimental Python backend for the Scala 3 compiler. It is enabled with `-scalapy`, installs `ScalaPyPlatform`, runs alongside the Scala.js and JVM backend phases, emits one `.pyir` file per compilation unit, and then links those IR files plus classpath support IR into a bundled `.py` file unless `-scpy-ir-only` is set.

The current backend is no longer a direct "typed tree to one Python file" experiment. Treat it as: post-erasure Scala trees -> typed PyIR -> serialized `.pyir` artifacts -> classpath loader -> reachability/link validation -> bundled Python source.

## Commands

```bash
# Build the Python support libraries, compiler-side tests, and all py tests.
sbt --client "testPyCompilation"

# Run only the Python test harness. Requires support jars to already be built.
sbt --client "pyCompilerTests/test"

# Rebuild support jars explicitly.
sbt --client "scala-pylib-py/Compile/packageBin"
sbt --client "scala-library-py/Compile/packageBin"

# Force support jars to be regenerated after backend/runtime changes.
sbt --client "scala-pylib-py/clean; scala-pylib-py/Compile/packageBin; scala-library-py/clean; scala-library-py/Compile/packageBin"

# Run py compiler harness suites.
sbt --client "pyCompilerTests/testOnly dotty.tools.dotc.ScalaPyCompilationTests"
sbt --client "pyCompilerTests/testOnly dotty.tools.dotc.ScalaPyCompilationTests -- --tests=runScalaPy"
sbt --client "pyCompilerTests/testOnly dotty.tools.dotc.ScalaPyCompilationTests -- --tests=negScalaPy"
sbt --client "pyCompilerTests/testOnly dotty.tools.dotc.PylibTest"
sbt --client "pyCompilerTests/testOnly dotty.tools.dotc.PyRunTest"

# Compiler-side Python backend unit tests.
sbt --client "scala3-compiler-bootstrapped/testOnly dotty.tools.backend.python.PyLinkerTest dotty.tools.backend.python.PyReachabilityTest dotty.tools.backend.python.ir.pyir.serialization.PyIRSerializationTests"

# Ad hoc: compile a source to .pyir + bundled .py.
sbt --client "scala3-compiler-bootstrapped/runMain dotty.tools.dotc.Main -scalapy -d /tmp/out foo.scala"

# Ad hoc: emit .pyir only, useful for support-library style debugging.
sbt --client "scala3-compiler-bootstrapped/runMain dotty.tools.dotc.Main -scalapy -scpy-ir-only -d /tmp/out foo.scala"

# Execute generated Python through the repo's pinned uv project.
uv sync --frozen
uv run --project . --no-sync python /tmp/out/foo.py

# Stdlib benchmark suite (Python backend, pyperf-driven).
sbt --client "stdlib-bench-py/runBench"
```

The Python-side bench harness is `pyperf` running through
`stdlib-bench-py/python-shim/run_bench.py`; the JVM-side `Driver.scala`
just compiles bundles, fans out one shim invocation per bundle, and
finally calls `python-shim/results_to_md.py` to write `notes/benchmark.md`
plus `results.jsonl`. Configurable via env vars (defaults shown):
`BENCH_PROCESSES=1 BENCH_WARMUPS=3 BENCH_VALUES=5 BENCH_MIN_TIME=1
BENCH_INNER_LOOPS=10`. Match against JMH with the same per-cell budget:
`Jmh / run -f $BENCH_PROCESSES -wi $BENCH_WARMUPS -i $BENCH_VALUES
-w $BENCH_MIN_TIME -r $BENCH_MIN_TIME -rf json -rff target/jmh.json`,
then re-invoke `runBench` with `JMH_RESULTS_JSON=target/jmh.json` to fold
the ratio column into the markdown.

Generated Python must run through `uv run --project <repo-root> --no-sync python`, never bare `python3`. `PyRun.scala` enforces this and checks `uv sync --frozen --check` before running harness output.

Stale ScalaPy artifacts can produce false positives or false negatives. The support libraries emit `.pyir`, and that `.pyir` depends on backend codegen/linker/runtime behavior, not just on `pylib-py/` or `library-py/` source timestamps. After changing `compiler/src/dotty/tools/backend/python/`, PyIR serialization, `PyIRRuntime`, `PyEncoding`, `GenPython`, or Python support-library APIs, rebuild `scala-pylib-py` and `scala-library-py`; use the clean rebuild command above when results differ between `./bin/scpyc`, direct fresh-output runs, and the harness.

## Backend Architecture

The compiler backend lives under `compiler/src/dotty/tools/backend/python/`.

- `GenPython.scala` is the backend phase. It forces `PyDefinitions`, lowers post-erasure `TypeDef`s to `PyClassDef`s, skips `@extern` facades, synthesizes static forwarders for top-level module classes, writes the current CU's `.pyir`, and invokes the linker unless `-scpy-ir-only` is set.
- `PyEncoding.scala` is the symbol/type/name boundary. It owns class, method, field, local, label, type, facade, `@extern`, and `@name` encoding. Do not duplicate string-based name logic elsewhere.
- `PyDefinitions.scala` caches symbols for `scala.python.*` and validates that they are real classpath entries, not stub symbols. Match on these symbols, not on `showFullName`.
- `PyClasspathLoader.scala` deserializes `.pyir` files from the compile classpath and the output directory. It tags all loaded inputs as `Support`; the current CU is passed to the linker in memory as `User`.
- `PyLinker.scala` validates nominal references, partitions `User` vs `Support`, applies reachability pruning, resolves main entries, drops runtime-provided class collisions from support inputs, and orders classes so Python base classes are defined first.
- `PyReachability.scala` is link-time DCE. User classes are preserved as roots; support classes are retained only when reached. It tracks reachable classes, instantiated classes, reachable methods/fields, virtual dispatch logs, module loads, and prelude call seeds from `PyIRRuntime.preludeCalls`.
- `PyIREmitter.scala` renders the linked bundle to Python. It emits the runtime prelude, extern imports, class definitions, class metadata/registration, lazy module singletons, constructor dispatch, closures, labels/non-local returns, and the main guard.
- `PyIRRuntime.scala` is the hand-written runtime contract and prelude. It now keeps only irreducible runtime/JDK-provided classes in `providedClasses`; most Scala/JDK surface is supplied by `.pyir` from the support libraries.

PyIR nodes live in `compiler/src/dotty/tools/backend/python/ir/pyir/`. `PyIR.scala` defines the sealed tree hierarchy; `PyNames.scala`, `PyTypes.scala`, `PyOps.scala`, and `PyPosition.scala` hold supporting value types; `PyIRPrinter.scala` is diagnostic output. `ir/pyir/serialization/` owns the stable binary format (`PyIRFormat`, `PyIRTags`, `PyIRSerializer`, `PyIRDeserializer`). Add new serialized node variants by appending tags and bumping the format version as directed in `PyIRTags.scala`.

Two IR worlds are intentionally separate:

- `PyApplyExternal` + `PyExternalName` are flat calls to runtime helpers and Python builtins such as `_scpy_i32`, `hash`, and `_scpy_to_str`. Do not use them for user code or facade references.
- `PyExternalRef(module, path)`, `PyApplyDynamic`, and `PyAttrAccess` model structured Python interop for `@extern` facades and `scala.python.Dynamic`. They are linker-opaque leaves except for walking argument/value subtrees.

## Libraries

There are two Python-side library projects in `project/Build.scala`.

- `scala-pylib-py` sources live in `pylib-py/`. This is the foundational platform layer compiled with `-scalapy -scpy-ir-only`: `scala.python.{PyAny, PyDynamic, Dynamic, native, @extern, @name}`, `scala.python.runtime.*` wrappers, and handwritten `java.**`/javalib implementations. Its packaged jar strips `java/**.class` and `java/**.tasty` so downstream typechecking still sees the real JDK, while preserving `java/**.pyir` for link time.
- `scala-library-py` sources live in `library-py/`. It compiles the Scala standard library sources plus local overrides to `.pyir`, consuming the packaged `scala-pylib-py` jar only as an unmanaged jar. It intentionally does not `dependsOn(scala-pylib-py)`, because the raw class directory would shadow JDK classes during typer.

`PathResolver` gives `-scalapy` classpath precedence over the JDK so ported `java.*` classes can shadow JRT classes where needed. The test classpath is assembled in `compiler/test/dotty/tools/vulpix/TestConfiguration.scala` as `scalaPyOptions`, `scalaPyRawPylibOptions`, and `scalaPyNegOptions`.

Facades are Scala declarations typechecked by the frontend but not emitted as Python classes:

```scala
import scala.python.*

@extern("numpy") object np extends PyAny:
  @name("zeros_like") def zerosLike(a: PyAny): PyAny = native
```

`@extern("module", "path", ...)` becomes a `PyExternalRef`; member calls and `Dynamic.module`/`Dynamic.attr` lower to `PyApplyDynamic`/`PyAttrAccess`. `applyDynamicNamed` requires literal-string keyword names that are valid Python identifiers.

## Tests

`py-compiler-tests/` contains the JUnit harness.

- `ScalaPyCompilationTests.scala` compiles and runs positive tests in `tests/pos-py/` against the packaged pylib jar, excluding cases that need the raw pylib class directory. Its `negScalaPy` path compiles `tests/neg-py/` with `-scpy-ir-only` so negative tests are not polluted by link-time diagnostics.
- `PylibTest.scala` runs the raw-pylib subset listed in `PylibTest.rawPylibEntries`.
- `ScalaPyTestSuite.scala` integrates with Vulpix and delegates execution to `PyRun`.
- `PyRun.scala` finds the generated bundled `.py`, validates the locked uv environment, and executes it with `uv run --project <repo-root> --no-sync python -W ignore`.

`tests/pos-py/` is now broad coverage, not just facade smoke tests. It includes basics, case classes, functions, labels/matches, module initialization, Python facades/dynamic calls/kwargs, and many javalib areas: `java.lang`, `java.io`, `java.math`, `java.net`, `java.nio`, `java.util`, regex, functions, collections, concurrency, atomics, timers, charsets, and runtime wrappers. Directory-style tests are supported when a scenario has multiple source files.

Positive tests normally pair runnable `.scala` sources with `.check` expected stdout files. `.check.out` files are generated/updated comparison artifacts; do not treat their presence as a new fixture format. Negative tests currently cover malformed externs, duplicate facade names, and non-literal keyword arguments.

When adding or updating a positive test, compile it with `-scalapy`, run the generated bundle through `uv run --project . --no-sync python`, update the `.check` from real output, then confirm through the harness. Prefer `testPyCompilation` when touching linker/runtime/library behavior because it rebuilds support jars before running tests.

## Repo Caveats

- The branch contains scratch files and working notes at the repo root, `inbox/`, `notes/`, `out/`, and similar directories. Do not clean them up unless asked.
- `notes/dce-improvement-plan.md` records the current DCE status and remaining reachability work.
- Scala.js-related projects such as `library-js/`, `sjs-compiler-tests/`, and `scaladoc-js/` are unrelated to the Python backend except as architectural references.

## Important Instructions

- When asked to make an issue note, follow the template in `notes/issue-template.md`.
