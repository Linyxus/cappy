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

## Investigation update (Layer 5 Wave 3)

Inspecting the generated `out/.../i13332intersection.py` reveals the real
root cause: a **mangled-name divergence** in PyEncoding's handling of
abstract type members.

- `Mirror.Product.fromProduct(p): MirroredMonoType` and `Mirror.Singleton`
  / `SingletonProxy` all encode their `fromProduct` result type as `__O`
  (the abstract-type sentinel) because the upstream
  `type MirroredMonoType` is an abstract type member with no upper bound
  — there's nothing for PyEncoding to erase to a concrete class.
- User-derived case-class mirrors (e.g. `Datatypes_JsonObject_`) receive
  a `fromProduct__Lscala_dProduct__Ljava_dlang_dObject` BRIDGE (full
  erasure to `java.lang.Object`) AND a typed
  `fromProduct__Lscala_dProduct__LDatatypes_uJsonObject` body.
- `SingletonProxy` does not get the bridge because no synthetic case
  class layer fires for it; its `fromProduct__Lscala_dProduct__O`
  exists on the parent (`Mirror_Product`).
- The CALL SITE goes through `m_3.fromProduct__Lscala_dProduct__Ljava_dlang_dObject(...)`
  — the fully-erased shape — and `Mirror_SingletonProxy` has neither the
  `__O` nor the `__Ljava_dlang_dObject` form locally.

Tried (and reverted): a library-py overlay
`library-py/src/scala/deriving/Mirror.scala` declaring
`type MirroredMonoType <: AnyRef`. Idea was to give PyEncoding an upper
bound it could erase to `Object`. **This did not work** — emitted methods
still encoded as `__O`. The encoder treats abstract type members the
same regardless of the upper bound, so the bound never propagates into
the result-type ref.

Real fix lives in `compiler/src/dotty/tools/backend/python/PyEncoding.scala`:
when encoding an abstract type member's result type, walk to its
upper bound (`AnyRef` / `Object`) and emit the concrete encoding
rather than the `O` sentinel. Or alternatively: at call sites that
go through `Mirror.Product.fromProduct`, emit a type-aware bridge
that resolves to the abstract-typed receiver.

Defer to a focused PyEncoding work item.

Specialist report: `/tmp/pyrun-analysis/cat-d-attr-errors.md` (D6).
