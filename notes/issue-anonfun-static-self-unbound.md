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

Specialist report: `/tmp/pyrun-analysis/cat-c-name-error.md`.
