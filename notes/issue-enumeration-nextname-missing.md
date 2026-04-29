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

## Failed first attempt — Layer 3 (deferred)

Tried a `library-py/src/scala/Enumeration.scala` override that used
`@extern("builtins", "dir")` / `getattr` / `len` / `type` and
`@extern("operator", "getitem")` to walk the Python instance dict and
derive symbolic names. The override compiled but at link time produced
`Unresolved instance method 'scala.EnumerationPy_.pyDir(...)'` etc. — i.e.
`@extern` on a `def ... = native` body inside library-py is not being
resolved as a facade, even though the same pattern works inside pylib-py
(see `pylib-py/src/scala/python/runtime/PyStruct.scala`). The linker
treats those declarations as ordinary instance methods of the encoded
module class.

Putting them inside the companion of `class Enumeration` failed for the
same reason (renamed to a sibling `EnumerationPy` object — same error).
The discrepancy between library-py and pylib-py @extern handling is a
deeper question; until that's resolved, this override can't ship.

Three plausible follow-ups:
1. Investigate why pylib-py's `@extern` def-on-method emits a facade ref
   while library-py's emits an instance method, and align them.
2. Move the override to pylib-py (unconventional, but pylib does have the
   plumbing already). Probably forces awkward circular-import shapes.
3. Use `scala.python.Dynamic` instead of `@extern` for the introspection.
   That bypasses the @extern path entirely.

Defer to Layer 5 (research). The Enumeration fixtures stay failing.

Specialist report: `/tmp/pyrun-analysis/cat-d-attr-errors.md` (D5).
