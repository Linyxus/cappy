# `transparent-object` fixture: emitted class missing inherited method

## Minimal example for reproducing

`tests/run/transparent-object.scala`:

```scala
object Test {
  def main(args: Array[String]): Unit = Foo.foo
}

object Foo extends Bar {
  inline def foo: Unit = bar
}

class Bar {
  def bar: Unit = println("bar")
}
```

```bash
sbt --client "scala3-compiler-bootstrapped/runMain dotty.tools.dotc.Main \
  -scalapy -d /tmp/dbg/tobj tests/run/transparent-object.scala"
uv run --project . --no-sync python /tmp/dbg/tobj/transparent-object.py
```

## Output vs Expected Behaviour

```
AttributeError: 'Test_' object has no attribute 'bar__V'
```

Expected: prints `bar`.

## Quick Analysis

After inlining `Foo.foo`, the call `bar` is a reference to `Bar.bar` resolved
through `Foo`'s parent. The generated Python for `Foo` (encoded as `Foo_` or
similar — actually the trace shows `Test_`, suggesting the inlined call
landed in `Test`'s body) is missing the `bar__V` method.

Two candidate roots:

(a) **DCE pruned the method**: the linker's reachability analyzer didn't see
    `Bar.bar` as reached because the call site after inlining doesn't
    syntactically resemble a call to `Bar.bar` — the inlining substituted
    `bar` lexically into `Test`'s scope and the analyzer missed the
    resolution.

(b) **Codegen never emitted the method**: GenPython lowered `Bar` without
    its `bar` member (e.g. because `Bar` was treated as not-instantiated and
    only its methods needed for static dispatch were emitted, but inlining
    requires all instance methods to be available on the singleton's class).

To distinguish: run with `-scpy-ir-only` and inspect the `.pyir` for `Bar`
and `Test`. If `bar__V` is in the IR but not in the bundled `.py`, it's
a DCE issue. If it's not in the IR, it's a codegen issue.

**Fix shape**: depends on which root. Probably a tweak to PyReachability to
mark all instance methods of objects whose lazy module loader is reachable.
Single-fixture issue, low priority but a useful symptom of the broader DCE
correctness story.

Specialist report: `/tmp/pyrun-analysis/cat-d-attr-errors.md` (D4a).
