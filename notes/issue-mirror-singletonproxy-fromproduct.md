# Compiler-derived `Mirror.SingletonProxy` is missing `fromProduct`

## Minimal example for reproducing

`tests/run/i13332intersection.scala` (full fixture; the failing path involves
hierarchical-ADT mirror derivation). A reduced repro that exercises the same
synthesis is:

```scala
import scala.deriving.Mirror

case object Foo

@main def Test =
  val m = summon[Mirror.Of[Foo.type]]
  println(m.fromProduct(EmptyTuple))  // expect: Foo
```

## Output vs Expected Behaviour

```
AttributeError: 'Mirror_SingletonProxy' object has no attribute
  'fromProduct__Lscala_dProduct__Ljava_dlang_dObject'
```

Expected: prints `Foo` (singleton `Mirror.fromProduct(_)` returns the singleton
value regardless of argument).

## Quick Analysis

`Mirror.SingletonProxy` is one of the synthetic mirror classes the compiler
creates for case objects / singleton ADT cases. Its `fromProduct` method is
trivial (return the singleton), but the synthesis happens inside `scalac`'s
mirror-creation phase, *before* the Python backend runs. Either:

(a) Scala erasure / synthetic-method generation produces `fromProduct` for
    `SingletonProxy` and the Python backend's `GenPython` skips/drops it for
    some reason (e.g. it has a body shape PyIREmitter doesn't handle).

(b) The pylib `library-py` overlay defines a partial `Mirror.SingletonProxy`
    that does not include `fromProduct`, shadowing the compiler-synthesized
    version.

Check `library-py/overrides-3/scala/deriving/` (or wherever Mirror is
overridden) to see if there's a hand-written `SingletonProxy` that's missing
the method. If yes, add `def fromProduct(p: Product): Any = Mirror.singletonValue`
or similar. If no, this is a codegen bug — inspect the `.pyir` for the affected
fixture to see whether `fromProduct` is in the IR.

**Fix shape**: most likely a missing line in the library-py mirror overlay.
Lower priority — single fixture (`i13332intersection`) and an unusual mirror
shape. Defer unless mirror-derivation support is in scope.

Specialist report: `/tmp/pyrun-analysis/cat-d-attr-errors.md` (D6).
