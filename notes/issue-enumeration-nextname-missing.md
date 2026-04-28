# `scala.Enumeration` instances missing `nextName` — Value() factory broken

## Minimal example for reproducing

```scala
object MyEnum extends Enumeration:
  val A, B, C = Value

@main def Test = println(MyEnum.values.toList)
```

Affected fixtures: `tests/run/i9482.scala`, `tests/run/t4570.scala`,
`tests/run/t5612.scala`.

## Output vs Expected Behaviour

```
AttributeError: 'L_' object has no attribute 'nextName'
AttributeError: 'Test_' object has no attribute 'nextName'
```

Expected: prints `List(A, B, C)`.

## Quick Analysis

`scala.Enumeration.Value` factory walks `nextNameOrNull()` against an
internal `nextName` iterator/state to assign the symbolic name to each
declared `Value`. The pylib / library-py port of `scala.Enumeration` is
either:

(a) Incomplete — the `nextName` field/iterator is stubbed away or not
    initialized in the generated Python class for `MyEnum`.

(b) Reachability-pruned — DCE dropped the field because reads/writes happen
    via an inherited `Enumeration`-method body that the analyzer didn't tie
    back to the user-facing class.

Inspect `library-py/src/` (or `library-py/overrides-3/`) for the
`scala.Enumeration` overlay; if there's no override, Scala's stdlib
`Enumeration.scala` is being compiled to PyIR and the bug is somewhere in
the field-initialization codegen for trait-mixed-in fields.

**Fix shape**: easiest is to add a library-py override for `scala.Enumeration`
that uses Python iteration semantics directly, sidestepping the JVM-style
`nextName` iterator entirely. Alternatively, fix the underlying
field-initialization issue (likely overlaps with the lazy-implicit cache
field issue tracked in `issue-lazy-implicit-cache-field.md`).

Specialist report: `/tmp/pyrun-analysis/cat-d-attr-errors.md` (D5).
