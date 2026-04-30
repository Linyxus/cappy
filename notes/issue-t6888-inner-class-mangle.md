# issue: t6888 inner-class accessor name mangled by Python's `__name` rule

**Status: FIXED at the encoding layer.** `PyMethodName.encoded` now
prepends a `_scpy_m` guard whenever the raw `<simple>__<sig>` form
starts with `__`, so encoded names like `___LC_u_u_u` (from a Scala
member named `$` whose `sanitizeName` output is `_`) are emitted as
`_scpy_m___LC_u_u_u` and Python's compile-time private-name mangling
rule never fires. The minimal repro now produces the expected output;
the original `tests/run/t6888.scala` fixture prints its expected
`2\n3\n3` content, though the surrounding `scala.App` lifecycle still
exposes an unrelated reflection gap (`Class.getResourceAsStream`
missing) that keeps the fixture on the failing list — that's a
separate work item.

---


## Symptom

`tests/run/t6888.scala` exercises a `class C { val x; object $ { val y = x+x; class abc$ { def xy = … }; object abc$ { def xy = … } } }` followed by `Test extends App` reading `c.$.y` and `c.$.abc$.xy`. From bucket-14 of the most recent sweep:

```
File ".../t6888.py", in _scpy_ctor_Test___void__V:
  _scpy_module_value(_scpy_mod_scala_Predef__).println__(self.c__LC().___LC_u_u_u().y__I())
AttributeError: 'C' object has no attribute '_Test____LC_u_u_u'.
  Did you mean: '_C___LC_u_u_u'?
```

The accessor `___LC_u_u_u()` (the `$` getter on `C`) is correctly defined on class `C` as `_C.___LC_u_u_u`. The call site is *literally* `c.___LC_u_u_u()`, but at runtime Python looks up `_Test____LC_u_u_u` instead, because the call appears lexically inside `class Test_:`.

## Root cause: Python compile-time private-name mangling, not the Layer 5.2 cross-class This handler

Python rewrites any identifier of the form `__name` (≥ 2 leading underscores, ≤ 1 trailing underscore) inside a class body to `_ClassName__name`. Confirmed standalone:

```
$ python3 -c '
class C:
    def ___foo(self): return 42
class Test_:
    def main(self):
        c = C()
        try: print(c.___foo())
        except AttributeError as e: print(e)
Test_().main()'
'C' object has no attribute '_Test____foo'
```

The encoding chain that produces a triple-underscore method name:

1. Source: `def $: C.this.$.type` (the synthesized getter for `object $`).
2. `PyEncoding.sanitizeName` (`compiler/src/dotty/tools/backend/python/PyEncoding.scala:316-323`) replaces `$` → `_`, giving simple name `_`.
3. `PyMethodName.encoded` (`compiler/src/dotty/tools/backend/python/ir/pyir/PyNames.scala:148-156`) builds `<simple>__<resultRef>` for nullary methods → `_` + `__` + `LC_u_u` = `___LC_u_u`. **Three leading underscores.**
4. `PyIREmitter` emits the call as `${parenthesize(receiver)}.${method.encoded}(…)` (`PyIREmitter.scala:1184`). The text `c.___LC_u_u()` is then handed to the Python compiler, which name-mangles it because the surrounding class body (`class Test_:`) lexically encloses the call.

Result: `c.___LC_u_u_u` becomes `c._Test_____LC_u_u_u` after Python's textual rewrite (one leading underscore + class name `Test_` + the original `___LC_u_u_u`, where Python sees `__LC_u_u_u` as the `__name` part to rewrite to `_Test___LC_u_u_u`, with the leading `_` of the original three preserved). The `_C_u_u_u` member that exists on `C` is never reached.

## Reproduction (smaller than the original t6888)

```scala
// /tmp/t6888_min.scala
class C { val x = 1; object `$` { val y = x + x } }
object Test:
  def main(args: Array[String]): Unit =
    val c = new C(); println(c.`$`.y)
```

```bash
mkdir -p /tmp/out-min
./bin/scpyc -d /tmp/out-min /tmp/t6888_min.scala
uv run --project . --no-sync python /tmp/out-min/t6888_min.py
# AttributeError: 'C' object has no attribute '_Test____LC_u_u_u'.
#   Did you mean: '_C___LC_u_u_u'?
```

The original `tests/run/t6888.scala` *also* hits a separate "Duplicate class 'C___abc_'" linker error under `bin/scpyc` because both `class abc$` and `object abc$` mangle to the same encoded class name. That collision is independent and orthogonal to this bug; for the runtime trace you can drop the `abc$` lines.

## Bisect: not Layer 5.2

Layer 5.2 added a guarded case in `genNormalApply`'s bare-Ident branch (`compiler/src/dotty/tools/backend/python/GenPython.scala:1314-1331`) for `Select(qual: This, _) if qual.symbol != currentClassSym`. Reverting just that case (lines 1314-1331) and rebuilding both support jars, the minimal repro **still fails identically**. Going further back to commit `1817f82bfa` (Layer 3 partial) and even to `493dc2efff` (Layer 1, the post-Layer-2 baseline anchor) reproduces the exact same `AttributeError: 'C' object has no attribute '_Test____LC_u_u_u'`. So the regression is **not** caused by Layer 5.2, and reverting it would also re-break `tests/run/transparent-object.scala` (verified: `'Test_' object has no attribute 'bar__V'` returns).

The bug actually predates the post-Layer-2 baseline and was almost certainly seeded by commit `9d209953b9` ("Address Python backend ad-hoc audit findings"), which introduced the injective `_d` / `_u` FQN encoding — switching `_` to `_u` made result-type refs longer, but more importantly, after that commit `<simple>__<resultRef>` for any method whose simple name *is* a single `_` produces `___…`. The post-Layer-2 baseline list in `notes/post-layer2-baseline.md` does not contain `t6888.scala`, but our re-check at that exact commit shows it failing — the baseline list is therefore incomplete (or t6888 was not exercised in that sweep).

## Suggested fix shape

Two viable angles, both in `PyMethodName.encoded` (`compiler/src/dotty/tools/backend/python/ir/pyir/PyNames.scala:148-156`):

1. **Avoid generating `__`-prefixed Python identifiers.** When `simple.name` starts with `_` (i.e. originated from a `$`-named member after `sanitizeName`), prepend a fixed non-underscore prefix to the encoded form so the emitted call site never matches Python's `__name` pattern. Easiest concrete shape: change `s"${simple.name}__${resultTypeRef.encoded}"` to something like `s"_scpy_m_${simple.name}__${resultTypeRef.encoded}"` only when the simple name begins with `_`, or unconditionally — since the rule has to match between definition site and call site, the simplest correct change is unconditional. The downside is generated-name churn across every method.

2. **Sanitize the simple name to never start with `_`.** In `PyEncoding.sanitizeName`, after the `$` → `_` rewrite, prepend a stable non-underscore guard (`_scpy_n_` or just `n_`) when the result starts with `_`. This is narrower (only affects symbols whose source name was `$`-prefixed or `_`-prefixed) but still requires synchronising every place that mangles a method-or-field simple name from a Scala identifier, including `encodeFieldName`, `encodeLocalName`, `encodeLabelName`, and the static-method special-name path.

Option 2 is the more local fix; option 1 (gating on `simple.startsWith("_")` inside `PyMethodName.encoded` only) is even smaller in scope but does not protect *field* names emitted with similar leading underscores — `PyFieldName.encoded` returns just `simple.name`, so a Scala field literally named `$` would have the same issue at any `c.$` read site that happens to live inside a Python `class` body.

Worth a quick look at any other Scala identifier whose mangle is `_`: `_root_`, accessor symbols generated by macros with names like `$$x`, lifted lambda-helper names with `$anonfun$1`. The injective encoding does not change `_` in *call-site simple names*; it only protects FQN encoding. So this bug is one example of a class of "simple name starting with `_` collides with Python private-name mangling" cases.

## Status

Read-only investigation; working tree is restored, support jars rebuilt at HEAD. No fix attempted. Rec: do not roll back Layer 5.2 — it is needed for `tests/run/transparent-object.scala` and is unrelated to this bug.
