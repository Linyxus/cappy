# `java.util.WeakHashMap` is missing from pylib

## Minimal example for reproducing

```scala
import java.util.WeakHashMap

@main def Test =
  val m = new WeakHashMap[String, String]()
  m.put("k", "v")
  println(m.get("k"))
```

Surfaced in PyRunTests run3 logs (specific fixture not isolated; appears in
several stdlib-driven fixtures).

## Output vs Expected Behaviour

Compilation aborts:

```
Unresolved class 'java.util.WeakHashMap'
Compilation failed for: '<fixture>.scala'
```

Expected: prints `v`.

## Quick Analysis

`java.util.WeakHashMap` is straightforward to port — it's semantically a
`Map[K, V]` whose entries are removed when the key is garbage-collected.
Python's `weakref.WeakKeyDictionary` is a close match (note: the JVM type
is weak on the *key*, not on the value, despite the name).

**Fix shape**: add `pylib-py/src/java/util/WeakHashMap.scala` as a thin
wrapper around `weakref.WeakKeyDictionary`. Methods needed by typical
callers: `put(K, V) → V`, `get(Object) → V`, `remove(Object) → V`, `size()`,
`clear()`, `containsKey(Object)`, `entrySet()`, `keySet()`, `values()`. ~20
lines.

The weak-key semantics may not be reachable from generated code (Python's
GC differs from JVM's) — for typical cache-style usage the weakness is a
correctness optimization, not a behavioral guarantee, so a strong-key
fallback is acceptable for v1.

Specialist report: `/tmp/pyrun-analysis/cat-b-other-linker.md` (port sketch).
