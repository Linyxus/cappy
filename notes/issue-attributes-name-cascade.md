# `java.util.jar.Attributes.Name` is missing from pylib, blocking ~80 fixtures

## Minimal example for reproducing

Any trivial program that pulls in `scala.util.Properties` via Predef will
reproduce. Pick any small fixture in `tests/run/`, e.g.:

```scala
// tests/run/extension-methods.scala (pre-existing)
@main def Test = println("hello")
```

```bash
sbt --client "scala3-compiler-bootstrapped/runMain dotty.tools.dotc.Main \
  -scalapy -d /tmp/dbg/x tests/run/extension-methods.scala"
```

## Output vs Expected Behaviour

Compilation aborts at link time:

```
Unresolved class 'java.util.jar.Attributes_Name'
Compilation failed for: 'tests/run/extension-methods.scala'
```

Expected: bundled `.py` is produced and runs cleanly. The reference is to the
inner class `java.util.jar.Attributes.Name`, PyIR-encoded as `Attributes_Name`.

## Quick Analysis

Root reference: `library/src/scala/util/Properties.scala:18`:

```scala
import java.util.jar.Attributes.{Name => AttributeName}
```

and a downstream call site instantiates `new AttributeName(...)` for the
`scalaCompilerVersion` constant. Because `scala.util.Properties` is part of the
core stdlib and is pulled in transitively by Predef, every reachable set
includes the dangling `Attributes_Name` reference. PyRunTests run4 logged 83
distinct `Unresolved class` events for this single class, blocking ~80 fixtures.

`pylib-py/src/java/util/jar/` is empty — there is no Python-side `Attributes`
or its inner `Name`.

**Fix shape**: minimal port to `pylib-py/src/java/util/jar/Attributes.scala`.
Inner class encoded as a top-level `class Attributes_Name(name: String)` per
the underscore-naming convention. Methods: `toString` (returns name), `equals`
(JDK semantics: case-insensitive for the registered standard names,
case-sensitive otherwise — check `java.util.jar.Attributes$Name` source for the
exact contract; for our use the simple case-sensitive form is enough), and
`hashCode` (standard string-derived). ~15 lines.

Verification: rebuild `scala-pylib-py`, recompile `scala-library-py`, re-run
PyRunTests — expect ~80 fewer compile failures.

Specialist report: `/tmp/pyrun-analysis/cat-a-attributes-name.md`.
