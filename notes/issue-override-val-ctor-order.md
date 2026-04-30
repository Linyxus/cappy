# `override val` collides with parent val under JVM-style ctor ordering

## Status

Open. Distinct from the Throwable / ParseError shadow bug that the
encoding fix solved — that one was about a *truly private* parent val.
This one is about *public* `val`s on both sides, where the encoder
correctly emits both at simple-name (so accessor calls work cross-class)
but ctor ordering still clobbers the subclass-set value.

## Symptom

```scala
class Parent(val msg: String):
  override def toString: String = s"Parent($msg)"

class Child(idx: Int, override val msg: String) extends Parent(s"parent-$idx"):
  override def toString: String = s"Child($idx, $msg)"

@main def Test =
  val c = Child(7, "child-msg")
  println(c.msg)         // wanted: child-msg, got: parent-7
  println(c)             // wanted: Child(7, child-msg), got: Child(7, parent-7)
```

The subclass body sets `self.msg = "child-msg"`, then chains to the super
ctor, which sets `self.msg = "parent-7"`, clobbering it.

## Why the encoding fix doesn't cover this

`PyFieldName.encoded` mangles only when the field is private (no public
accessor). `Parent.msg` and `Child.msg` both have public accessors —
they're public `val` parameters — so both encode to the simple name `msg`
and share `self.msg`.

Mangling public fields would break cross-class accessor reads, since
those go directly through `self.msg` in the generated Python (the
accessor body is just `def msg__Ljava_dlang_dString(self): return
self.msg`). And conceptually JVM does NOT keep separate slots for
overriding `val`s — the override replaces the parent's slot. The bug is
specifically the *ordering*: subclass body should run AFTER super
chain, not before.

## Fix shape

Two options:

1. **Codegen reordering**: emit the subclass-body field initializers
   AFTER the super-ctor chain. The current `_scpy_ctor_…` body for a
   subclass is roughly:

   ```python
   def _scpy_ctor_Child___I_Ljava_dlang_dString__V(self, idx, msg):
       self.idx = idx
       self.msg = msg                     # subclass body
       _scpy_java_Parent._scpy_ctor_Parent___Ljava_dlang_dString__V(
           self, _scpy_String_concat("parent-", _scpy_to_str(idx)))
       # ↑ super ctor runs LAST, overwrites self.msg
   ```

   It needs to be:

   ```python
   def _scpy_ctor_Child___I_Ljava_dlang_dString__V(self, idx, msg):
       _scpy_java_Parent._scpy_ctor_Parent___Ljava_dlang_dString__V(
           self, _scpy_String_concat("parent-", _scpy_to_str(idx)))
       self.idx = idx
       self.msg = msg                     # subclass body, AFTER super
   ```

   This matches JVM bytecode order (`invokespecial` of `<init>` runs
   first, then field-init bytecodes for the subclass). The current code
   inverts it.

2. **Override-aware overwrite skip**: in the parent ctor, before
   `self.msg = ...`, check whether the subclass already set `msg`. Too
   fragile — relies on every parent ctor knowing it might have an
   override.

Option 1 is the structural fix and matches JVM semantics. The work lives
in whichever GenPython path emits `_scpy_ctor_…` bodies — likely the
`Block` lowering that produces the prefix `(self.x = a; super.<init>(b);
…)` from the desugared constructor body.

## Investigation pointers

- `compiler/src/dotty/tools/backend/python/GenPython.scala` —
  `genConstructor` / `genCtorBody` (or similar) is where the body is
  lowered. Check whether the super call's position is honoured from the
  desugared tree, or whether something is reordering it.
- `tests/run/Pouring.scala` and `tests/run/main-functions.scala` already
  pass (CommandLineParser ParseError is a *private* parent field), so
  the subset that overlaps with the encoding fix is fine. Search the
  failing-fixtures list for cases that explicitly use `override val` on
  a parameterized class.

## Reproducer in the test tree

A pos-py fixture would need to hit a `class Child(override val foo: T)
extends Parent(...)` shape and assert the override wins after
construction. Pair it with the existing `tests/pos-py/private-field-shadow.scala`
which guards the orthogonal private-parent case.
