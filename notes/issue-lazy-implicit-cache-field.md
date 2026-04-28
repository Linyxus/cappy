# Lazy implicit cache field is never created in generated Python class

## Minimal example for reproducing

`tests/run/lazy-implicit-lists.scala` (full fixture; uses recursive lazy
implicits to build a `shaped[L, ...]` typeclass). Reduced shape:

```scala
trait Box[T] { def value: T }

object Test:
  given lazyInt: Box[Int] = new Box[Int] { def value = 7 }
  // The compiler synthesizes a lazy implicit cache for transitively-lazy
  // implicits; this fixture's full shape exercises the bidirectional Sum/Prod
  // derivation that triggers `_Test___lazy_implicit__N` synthesis.
```

## Output vs Expected Behaviour

```
AttributeError: 'Test___lazy_implicit__2_1' object has no attribute
  '_Test___lazy_implicit__1__L...'
```

Expected: assertion in fixture passes; lazy implicit graph is materialized.

## Quick Analysis

The Scala compiler synthesizes hidden cache fields with names matching
`_Test___lazy_implicit__N` to memoize a recursive lazy-implicit chain. The
generated Python class for `Test` is missing the corresponding instance
attribute. Either:

(a) Linker DCE prunes the field because the reachability analyzer doesn't see
    a write site (the field is initialized via the lazy-getter's first call,
    which the analyzer may not follow correctly).

(b) PyIREmitter's class-body emission isn't including all `PyFieldDef` entries
    for synthesized lazy-cache fields.

This is closely related to (and probably blocked by) the field-pruning work
already tracked in `notes/dce-improvement-plan.md`. The DCE plan calls out
field reachability as known-incomplete; this fixture is concrete evidence.

**Fix shape**: defer until the DCE field-pruning effort lands. After that,
re-run the fixture; if it still fails, investigate PyIREmitter class-body
emission for synthetic-cache fields.

Specialist report: `/tmp/pyrun-analysis/cat-d-attr-errors.md` (D7).
