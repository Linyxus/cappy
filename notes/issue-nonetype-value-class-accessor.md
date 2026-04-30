# Layer 5.1 — remaining sub-issues

The original umbrella ("value-class accessor on `None` raises `AttributeError`")
was a misdiagnosis. The five fixtures hit five different bugs. This note
tracks only the still-open sub-issues; the landed ones are summarized below
the table.

## Status

| Sub | Fixture | Status |
|---|---|---|
| 5.1 (root) | `t7396.scala` | **Landed** — `_scpy_Object.toString` now dispatches `self.__hash__()` |
| 5.1.b | `lambda-null.scala` | **Landed** (Layer 5 Wave 2) — `_scpy_unbox_or_default` in `genClosure` |
| 5.1.c | `numbereq.scala` | **Landed** (Layer 5 Wave 1) — `isEqualsAnyOverload` in `PyEncoding.specialMethodNameOf` |
| 5.1.a | `exceptions-2.scala` | **Landed** (Layer 5 Wave 3) — `AttributeError` translates to `NullPointerException` |
| 5.1.d | `t4122.scala` | **Open** — DCE drops inherited dunder; needs PyReachability fix |
| 5.1.e | `lambda-null.scala` (residual) | **Open** — `null.asInstanceOf[Primitive]` does not unbox |

## 5.1.d — `Seq[Char]` hashCode disagreement across collection backings

Reproducer: `tests/run/t4122.scala`. `"ab".##`, `Array('a','b').toIndexedSeq.##`,
`Seq('a','b').##`, `"ab".toList.##` should all be equal per the Scala
`Seq.hashCode` contract. On Python they differ.

Root cause is in `compiler/src/dotty/tools/backend/python/PyReachability.scala`,
not in `library-py`. Inspecting the bundled `out/runPyTests/run/t4122/t4122.py`:

- `scala_collection_Seq` emits `__eq__` but not `__hash__`; line ~9340 has
  `__hash__ = _scpy_Object.__hash__` from
  `PyIREmitter.maybeRebindInheritedHash` — i.e. `Seq.hashCode` → `__hash__`
  was DCE-pruned from `cls.methods` before emission.
- `WrappedString` and `List` (`::`) have no `__hash__` either — they
  inherit `_scpy_Object.__hash__` (identity hash).
- `ArraySeq.ofChar` works because it defines `hashCode` LOCALLY, so the
  dunder-keep at `PyReachability.scala:337-345` covers it.

The dunder-keep rule only loops `cd.methods` of the instantiated class; it
misses dunders inherited from trait/abstract parents. The virtual-call-log
replay path (`replayVirtualLog` → `resolveInstanceMethod`) is supposed to
compensate by walking ancestors, but isn't reaching `Seq.hashCode` here.
Suspect a `PyMethodName` mismatch between the `(Object, __hash__, [],
IntRef)` logged at the `x.hashCode` call site in `Statics.anyHash` and the
deserialized `Seq.__hash__` member in `Seq.pyir` (different result-type
encoding `PyClassRef(IntClass)` vs `PyPrimRef.IntRef`).

Fix path: in `PyReachability`, the dunder-keep rule should walk ancestors
when an instantiated class doesn't define its own dunder. Focused fix
probably ~10 lines but needs careful verification that it doesn't reactivate
previously-DCE'd ancestor methods unrelated to the dunder slot.

A library-py workaround (type-dispatch in `Statics.anyHash`) would mask the
real bug and contradicts the layered architecture.

## 5.1.e — `null.asInstanceOf[Primitive]` does not unbox to default

Reproducer (minimal):
```scala
def gen[A]: A = null.asInstanceOf[A]
val r: Int = gen[Int]   // prints "null", r == 0 is false
```

JVM `BoxesRunTime.unboxToInt(null)` returns `0`; our `PyAsInstanceOf` lowering
in `PyIREmitter.scala` is a no-op for everything except `PyCharType`. After
5.1.b landed, the `lambda-null.scala` fixture advances past the specialized
assertion and fails on `assert(genericCall1(if1_generic) == 0)` because of
this cast.

Fix shape: `PyAsInstanceOf` to a primitive type should emit
`_scpy_unbox_or_default(tag, value)` (the helper introduced for 5.1.b), not
just pass the value through.
