# Eta-expanded extension method emits static anonfun that references unbound `self`

## Minimal example for reproducing

`tests/run/i9507.scala`:

```scala
extension (x: Int)
  def add(y: Int): Int = x + y

def newFunction: Int => Int = 4.add

@main def Test = assert(newFunction(1) == 5)
```

Compile and run:

```bash
sbt --client "scala3-compiler-bootstrapped/runMain dotty.tools.dotc.Main \
  -scalapy -d /tmp/dbg/i9507 tests/run/i9507.scala"
uv run --project . --no-sync python /tmp/dbg/i9507/i9507.py
```

Other fixtures with the same pattern: `given-eta`, `i24201a`,
`quoted-sematics-1`, `t7396`, `t7763`, plus the much bigger `Parser.scala`
(parser-combinators). All hit the same root.

## Output vs Expected Behaviour

```
NameError: name 'self' is not defined
```

…raised inside the generated `Term__anonfun_1__LParser` (Parser case) or the
analogous anonfun in each fixture. Expected: the assertion in `i9507` passes;
`newFunction(1)` returns `5`.

## Quick Analysis

Generated Python shape:

```python
def newFunction__Lscala_dFunction1(self):
    return _scpy_Fn1(lambda _scpy_samarg_0:
        _scpy_mod_i9507_package__.newFunction__anonfun_1__I__I(_scpy_samarg_0))

@staticmethod  # WRONG: marked static but body references self
def newFunction__anonfun_1__I__I(y):
    return self.add__I_I__I(4, y)  # NameError: self undefined
```

Root cause is in `compiler/src/dotty/tools/backend/python/GenPython.scala:594-599`,
which faithfully translates the Scala `JavaStatic` flag into
`PyMemberNamespace.PublicStatic` / `PrivateStatic`:

```scala
val isStatic = sym.is(JavaStatic)
val namespace = (isStatic, sym.is(Private)) match
  case (true,  true)  => PyMemberNamespace.PrivateStatic
  case (true,  false) => PyMemberNamespace.PublicStatic
```

`PyIREmitter.scala:654-658` then drops the `self` parameter for static
namespaces:

```scala
private def buildParamList(method: PyMethodDef): String =
  val selfParam: List[String] =
    if method.flags.namespace.isInstance
       || method.flags.namespace == PyMemberNamespace.Constructor
    then List("self") else Nil
```

The bug is that anonfun methods synthesized for eta-expanded extension methods
are marked `@JavaStatic` by Scala erasure even though their bodies still
reference the captured receiver. PyIREmitter then correctly emits a static
method, but the body still has `self` lexically.

**Fix shape (local to GenPython, no PyIR shape change)**: in
`GenPython.genMethod`, when the method name matches the anonfun pattern
(`$anonfun`) and the source closure has a non-empty environment that captures
`this`, override the static flag — emit as instance method and add the
captured receiver as a parameter (or rewrite body refs).

## Failed first attempt — Layer 2 (deferred)

Tried the simple heuristic
`sym.is(JavaStatic) && sym.isAnonymousFunction && sym.owner.is(ModuleClass)`
in three sites (`genMethod`, `genNormalApply`, `genClosure`) — flipping the
namespace from `Public[/Private]Static` to `Public[/Private]`. The minimal
i9507 reproducer was not verified end-to-end before rollout (multi-agent FS
contention prevented a clean test), and a clean rebuild on top of Layer 2.2
+ 2.3 produced **215 failures in `runScalaPy` (pos-py) and 32 in
`runScalaPyPylib`** with linker errors of the shape
`Unresolved module class 'java.lang'` and `Unresolved module class
'<empty>'`.

The heuristic is too broad. It catches genuine lifted lambdas
(`new Thread(() => …)`, etc.) whose bodies do *not* reference the
enclosing receiver — flipping their namespace makes the call site try to
load a module class that does not exist (the empty-package or `java.lang`
$package module). Reverting Layer 2.1 alone restores the suite to clean.

A correct fix needs a tighter discriminator. Two leads:

1. Only flip when the body actually contains a `This(...)` (or `Ident(self)`)
   that is *not* a `dd` parameter. That distinguishes "body uses `self`" from
   "body is genuinely standalone."
2. Look at the closure environment at the LambdaLift site: eta-expanded
   extension methods have a non-empty env that captures `this`, while
   `new Thread(() => …)` lifted lambdas do not.

Both are heavier than the trivial flag flip. Treat as a **Layer 5 research
item** (single-fixture or per-pattern fix) rather than re-attempting the
broad heuristic. Affected fixtures are listed above.

Specialist report: `/tmp/pyrun-analysis/cat-c-name-error.md`.

## Resolution — Layer 2.1 landed (commit `1eddffa163`)

Lead 1 was implemented: a body-walking discriminator
(`needsSelfDespiteStatic` in `GenPython.scala`) detects lifted helpers
whose lifted body still references the enclosing module's receiver.
Demoted symbols are kept in `anonfunDemotedToInstance`, populated by a
pre-pass over the class members in `genClassMembers` *before* any
method body runs, so:

- `genMethod` flips `JavaStatic → Public` (or `PrivateStatic → Private`)
  for the demoted set, emitting an instance method with `self`.
- `genNormalApply` and `genClosure` use the same set when computing
  `isStaticTarget`, so the call site still passes the module receiver
  and Python binds it to `self` (no `@staticmethod` decoration → normal
  receiver binding kicks in). Without this symmetry, the prior naive
  flip produced linker errors of the shape
  `Unresolved module class 'java.lang'` / `'<empty>'` when the def site
  no longer matched the call site.

Gating is restricted to two synthesis paths whose bodies are known to
escape the receiver: `sym.isAnonymousFunction` (LambdaLift) and
`sym.name.toString.contains("superArg$")` (HoistSuperArgs), both
required to live on a `ModuleClass`. The body walker then catches
`This(enclosing)` and bare `Ident(member)` whose owner is the same
module class and which is not a local/parameter/module/package.
Closures are descended into because their `meth` reference is what
controls runtime evaluation of `self`.

Targets verified (commit binary regenerated end-to-end through `bin/scpyc`):
i9507, given-eta, i24201a, tuple-ops, byname-varargs, quoted-sematics-1.
`erased-lambdas` requires `experimental.erasedDefinitions` and is
unrelated to this fix.
