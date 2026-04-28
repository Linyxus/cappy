# `_scpy_fn_specialized_forward` infinite recursion (`Function0` cycle)

## Minimal example for reproducing

`tests/run/t603.scala` (full fixture; defines a `Susp[+A]` extending
`Function0[A]`). Reduced:

```scala
class Susp[+A](lazyValue: => A) extends Function0[A] {
  override def apply(): A = lazyValue
}

@main def Test = println(new Susp(42)())
```

## Output vs Expected Behaviour

```
RecursionError: maximum recursion depth exceeded
```

Stack trace cycles between `Function0.apply_mcI_sp__I` →
`Function0.apply_mcB_sp__B` → `Function0.apply_mcS_sp__S` → back to
`apply_mcB_sp__B` (any of the specialized `apply_mc*_sp` variants).

Expected: prints `42`.

## Quick Analysis

Scala's `Function0[A]` has primitive specialization variants
(`apply$mcI$sp`, `apply$mcB$sp`, ..., `apply$mcS$sp`) that delegate to one
another and ultimately to the unboxed `apply`. PyIREmitter generates a
`_scpy_fn_specialized_forward` body that's supposed to bottom out in a
concrete implementation. The traceback shows the variants forwarding to each
other in a cycle, never reaching `apply`.

Most likely cause: the specialization-forwarder synthesis in
`PyIREmitter.scala` (search for `_scpy_fn_specialized_forward` or
`_scpy_fn_static_forward`) chooses the next variant based on signature hash
or name pattern, and for `Function0` (which has many `apply_mc*_sp`
variants) the chain doesn't include a base case for the variant the user's
class overrides — so `apply_mcI_sp` forwards to `apply_mcB_sp` which
forwards to `apply_mcS_sp` which forwards back to `apply_mcB_sp`, etc.

**Fix shape**: either
(a) make every specialized forwarder fall through to the unboxed `apply` if
    the receiver does not override the specialized variant; or
(b) make the user's override of `apply` seed the chain so all `_sp` variants
    forward to the user's override directly.

Single-fixture issue but the symptom (cyclic forward) suggests the bug is
general and could surface in any user class extending a specialized SAM.

Specialist report: `/tmp/pyrun-analysis/cat-g-uncategorized.md`.
