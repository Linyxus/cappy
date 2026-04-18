# CLAUDE.md

This file provides guidance to coding agents when working with code in this repository.

## Project focus

This branch (`scala-py`) develops an experimental **Python backend** for the Scala 3 compiler, enabled by the `-scalapy` flag. It hooks into the compiler's phase pipeline at the same point as the JVM (GenBCode) and Scala.js (GenSJSIR) backends and emits `.py` files.

## Commands

```bash
# Run the full ScalaPy test suite (positive + negative).
sbt --client "pyCompilerTests/testOnly dotty.tools.dotc.ScalaPyCompilationTests"

# Convenience command for running all ScalaPy related tests
sbt --client "testPyCompilation"

# Run a single method of the suite.
sbt --client "pyCompilerTests/testOnly dotty.tools.dotc.ScalaPyCompilationTests -- --tests=runScalaPy"
sbt --client "pyCompilerTests/testOnly dotty.tools.dotc.ScalaPyCompilationTests -- --tests=negScalaPy"

# Unit tests for the PyRun helper.
sbt --client "pyCompilerTests/testOnly dotty.tools.dotc.PyRunTest"

# Ad-hoc: compile a file with the backend and inspect output.
sbt --client "scala3-compiler-bootstrapped/runMain dotty.tools.dotc.Main -scalapy -d /tmp/out foo.scala"

# Execute generated Python via the repo's pinned uv project.
uv sync --frozen            # once, to populate .venv from uv.lock
uv run --project . --no-sync python /tmp/out/foo.py
```

Generated Python must always be executed through `uv run --project <repo-root> --no-sync python`: never bare `python3`. `PyRun.scala` already does this; keep new harness code consistent.

## Architecture

The backend lives in three places:

### 1. Compiler phase — `compiler/src/dotty/tools/backend/python/`

Pipeline: Scala typed AST → **PyIR** → Python source.

- **`GenPython.scala`** — the `GenPython` phase. `genCompilationUnit` walks `TypeDef`s; `genExpr`/`genStat` recurse over post-erasure trees; `genApply` dispatches method calls; `genPrimitiveOp` maps `ScalaPrimitivesOps` codes to Python IR.
- **`PyEncoding.scala`** — Scala symbol/type → Python name/type mapping, plus facade machinery: `externBindingOf`, `externMemberNameOf`, `hasExternAnnotation`, `isFacadeSymbol`, `isValidPyAttrName`, `validateFacadeMemberNames`, deduped malformed-extern reporter.
- **`PyDefinitions.scala`** — symbol cache for `scala.python.*` (analogous to `jsdefn`). Force it with `PyDefinitions.force()` during phase setup; downstream code pattern-matches on symbol identity (never on `showFullName`).
- **`PyIREmitter.scala`** — pretty-prints PyIR to Python source. Also collects extern-import aliases via a pre-traversal walker and renders them at the top of the bundle.
- **`PyLinker.scala`** — whole-bundle validation. `PyExternalRef`, `PyApplyDynamic`, and `PyAttrAccess` are **linker-opaque leaves** — no class/member existence checks. Facade classes never reach the linker because `genCompilationUnit` skips them.
- **`PyIRRuntime.scala`** — the runtime class whitelist (`providedClass`).

PyIR nodes live in `ir/pyir/` (`PyIR.scala`, `PyNames.scala`, `PyOps.scala`, `PyTypes.scala`, `PyPosition.scala`). Statements and expressions are separate enums (Python enforces the distinction); `ExprStmt` bridges into statement position.

**Two IR worlds, don't mix them:**
- `PyApplyExternal` + `PyExternalName` — flat, runtime-intrinsic-only (`_scpy_i32`, `hash`, `_scpy_to_str`, …). Do **not** add facade/user-code things here.
- `PyExternalRef(module, path)` + `PyApplyDynamic(callee, args, kwargs)` + `PyAttrAccess(obj, name)` — structured, for facades and `scala.python.Dynamic` interop. Opaque to the linker; emitter renders as `<alias>.<path…>` / `<obj>.<name>` / call form.

### 2. Sidecar library `library-py/src/scala/python/`

`PyAny`, `PyDynamic`, `Dynamic`, `@extern`/`@name`, `def native`. These sources are merged into `scala-library-bootstrapped` and `scala3-compiler-bootstrapped` via `unmanagedSourceDirectories` in `project/Build.scala` (~lines 1570 and 1706). No separate sbt project today — the sidecar packaging is deferred work.

Facades declare bindings like:

```scala
@extern("numpy") object np extends PyAny:
  @name("zeros_like") def zerosLike(a: PyAny): PyAny = native
```

Facades are type-checked by the frontend but never emitted as Python classes — `genCompilationUnit` skips any `TypeDef` whose symbol (or an override) carries `@extern`.

### 3. Test harness — `py-compiler-tests/`

- **`ScalaPyCompilationTests.scala`** — JUnit entry. `runScalaPy` compiles and runs every `.scala` in `tests/pos-py/` and diffs against the matching `.check`; `negScalaPy` compiles everything in `tests/neg-py/` expecting errors.
- **`PyRun.scala`** — executes generated Python. Always through `uv run --project <repo-root> --no-sync python`. Requires `uv sync --frozen` to have populated `.venv`.
- **`PyRunTest.scala`** — unit tests for the helper.
- sbt project key: `pyCompilerTests` (defined in `project/Build.scala` ~line 2579, depends on `scala3-compiler-bootstrapped`).

The `-scalapy` flag is registered in `compiler/src/dotty/tools/dotc/config/ScalaSettings.scala`; the extra compile flags used by the test harness live in `compiler/test/dotty/tools/vulpix/TestConfiguration.scala:93` as `scalaPyOptions`.

## Test layout

- `tests/pos-py/` — positive tests. Each scenario is a `<name>.scala` (runnable, with an entry point) plus a `<name>.check` containing the exact expected stdout. Current coverage: hello, test1–3, facade0, facade-builtins, facade-ctor, extern-def-builtins, dynamic-builtin, dynamic-attr, update-dynamic, multipath-extern, keyword-attrs, kwargs-basic, kwargs-mixed, nested-facade.
- `tests/neg-py/` — compile-error tests: `duplicate-extern-name`, `kwargs-non-literal`, `malformed-extern`.

When adding a test: run it standalone via `sbt … runMain dotty.tools.dotc.Main -scalapy -d /tmp/out <file>.scala`, execute the `.py` through `uv run`, paste the real output into `.check`, then add it to the harness run to confirm.

## Repo layout caveats

- The `scala-py` branch accumulates scratch files at the repo root (e.g. `hello*.scala`, `test*.scala`, `cc-fluid-*`, `inbox/`, `notes/`, `mkissue.sh`). These are in-progress exploration — **do not clean up** without being asked.
- Other active notes live in `notes/` (phase reports, interop plan, issue write-ups). They're historical but useful context for ongoing work.
- This branch also contains the unrelated `compiler-js/` Scala.js-compiler experiment. Don't confuse the two: Python backend work never touches `compiler-js/`.

## Important Instructions

- When asked to make a issue note, always follow the template in notes/issue-template.md
