# `Class.getResourceAsStream` missing on `_scpy_Class` runtime

## Symptom

After the encoding fix landed and `_scpy_make_lazy_handle` started routing
through correct owner-mangled storage names, lazy-val initializers that
were previously short-circuited by VarHandle infinite-loops now actually
execute. `scala.util.Properties.scalaProps` is one such lazy val; it calls
`getClass.getResourceAsStream("/library.properties")` to read the version
banner, and that method does not exist on the `_scpy_Class` runtime
shim.

```
AttributeError: '_scpy_Class' object has no attribute
  'getResourceAsStream__Ljava_dlang_dString__Ljava_dio_dInputStream'
```

Affected fixtures (re-failing in the post-encoding-fix sweep): `t1381`,
`t6888` (the original Layer 5.2 fixture; encoding fix unblocked the
inner-class accessor but exposed this Properties path).

## Reproduction

```scala
@main def Test =
  val s = classOf[Object].getResourceAsStream("/foo.properties")
  println(s)
```

Compile and run; the call site lowers to a missing method on
`_scpy_Class`.

## Quick analysis

`_scpy_Class` lives in `compiler/src/dotty/tools/backend/python/PyIRRuntime.scala`
and is the runtime stand-in for `java.lang.Class`. It currently exposes
`getName`, `getSimpleName`, `getEnclosingClass`, etc., but no resource
lookup methods. Any caller that expects `getResourceAsStream`,
`getResource`, or the related `ClassLoader` resource APIs hits this gap.

Two viable shapes:

1. **Stub returning `null`** — preserves call-site shape; `Properties`
   then falls through to its `defaults` branch and prints the empty
   version. Quickest unblock for fixtures that don't actually depend on
   the resource. Single-method change.

2. **Real implementation** routed through a Python file lookup
   (`importlib.resources` or a path resolver scoped to the bundled
   support classpath). Necessary for any fixture that actually reads a
   resource file.

For the current run-sweep, (1) is enough — none of the affected fixtures
inspect the returned stream beyond null-checking.

## Why this surfaces now

Pre-encoding-fix, `Properties.scalaProps` lazy-init looped forever in
`_scpy_make_lazy_handle("scalaProps_lzy1").compareAndSet(...)` because
the simple-name lookup did not match the owner-mangled storage. The
infinite loop masked any downstream call. After the encoding fix, the
init body actually runs and immediately reaches the missing
`getResourceAsStream` method.
