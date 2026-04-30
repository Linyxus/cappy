# issue: CommandLineParser ParseError loses `msg` after Layer 3.1 Throwable rename

**Status: FIXED.** Owner-aware private-field mangling at the encoding
layer (`PyFieldName.encoded` honors `isPrivate`, set from
`encodeFieldName` reading `sym.getter`'s privacy when an accessor
exists). `Throwable.msg` (truly private — no accessor) encodes as
`_scpy_f_java_dlang_dThrowable__msg`; `ParseError.msg` (val with public
accessor) keeps simple name `msg`. Different Python attribute slots —
parent ctor no longer clobbers the subclass-set value. Verified by
`tests/run/Pouring.scala` and `tests/run/main-functions.scala` both
flipping from failing to passing in the post-encoding-fix sweep, plus a
new pos-py regression guard `tests/pos-py/private-field-shadow.scala`.

---


## Symptom

`tests/run/Pouring.scala` and `tests/run/main-functions.scala` (both `@main`
fixtures with at least one positional/typed argument) used to print the
expected `Illegal command line: more arguments expected` / `Hello, world!`
respectively, but now exit with a Python traceback. The trace shape from
`/tmp/pyrun-analysis-2026-04-30/bucket-14.log` and bucket-15 is:

```
File ".../Pouring.py", in main: parseArgument(args, 0, ...)
File ".../Pouring.py", in parseArgument:
  (lambda: (_ for _ in ()).throw(_scpy_new(scala_util_CommandLineParser_ParseError, ...)))()
scala_util_CommandLineParser_ParseError
During handling of the above exception, another exception occurred:
... Test.main(sys.argv[1:]) ...
```

The "during handling" framing suggests the catch did not fire. It actually
*does* fire — the second exception is the one raised inside `showError`.

## Reproduction

```bash
mkdir -p /tmp/out-pouring
./bin/scpyc -d /tmp/out-pouring tests/run/Pouring.scala
uv run --project . --no-sync python /tmp/out-pouring/Pouring.py
# -> TypeError: can only concatenate str (not "NoneType") to str
#    (raised inside scala.util.CommandLineParser.showError, not at the throw site)
```

The Python-level main-method emission at `/tmp/out-pouring/Pouring.py:2195-2203`
contains a textbook lowering of `try { … } catch { case err: ParseError => showError(err) }`:

```python
def main__ALjava_dlang_dString__V(args):
    try:
        ... parseArgument(args, 0, ...) ...
    except Exception as _scpy_ex:
        if _scpy_is_value_of_type(_scpy_ex, _scpy_class_of_name("scala.util.CommandLineParser_ParseError")):
            error = _scpy_ex
            ...showError(error)
        else:
            (lambda: (_ for _ in ()).throw(_scpy_ex))()
```

So the `_scpy_is_value_of_type` branch matches, `showError(error)` is invoked,
and `showError` does `s"Illegal command line$where: ${err.msg}"`. `err.msg`
returns `None`, hence the `TypeError` — Python re-prints the original ParseError
trace under the "During handling …" header before propagating the new error.

A direct probe confirms it:

```python
e = _scpy_new(ParseError, ParseError._scpy_ctor_..._I_Ljava_dlang_dString__V,
              0, "more arguments expected")
print(e.idx, repr(e.msg))    # -> 0 None
```

## Root cause

Commit `1817f82bfa` ("Layer 3 (partial): Throwable.getMessage + ObjectOutputStream(out)")
renamed the body field of `pylib-py/src/java/lang/Throwables.scala`'s
`Throwable` from `message` to `msg`, to dodge a separate codegen issue
where the field collided with the same-named *auxiliary-ctor parameter*
`message` (see `notes/issue-throwable-message-missing.md`).

`scala.util.CommandLineParser.ParseError` is declared as

```scala
class ParseError(val idx: Int, val msg: String) extends Exception
```

In Python, fields are looked up by simple name (see the comment on
`encodeFieldName` in `compiler/src/dotty/tools/backend/python/PyEncoding.scala:114-123`:
"Python's attribute access uses only the simple name, so shadowing works
via normal Python semantics"). That means subclass and parent fields
sharing a simple name share a single `self.<name>` slot. As soon as
`Throwable` and `ParseError` both call their fields `msg`, they alias.

The constructor sequence in `_scpy_new(cls, ctor, *args)` is:

1. Walk `reversed(cls.__mro__)`, calling each class's zero-arg
   `__init__(obj)` so each level's per-class field defaults run.
2. Call `ctor(obj, *args)` — for `ParseError`, that runs
   `_scpy_ctor_scala_util_CommandLineParser_ParseError__I_Ljava_dlang_dString__V`,
   which sets `self.idx = idx; self.msg = msg` and **then** chains to
   `_scpy_java_Exception._scpy_ctor__scpy_java_Exception__void__V(self)`.
3. The chained super-ctor reaches
   `_scpy_java_Throwable._scpy_ctor__scpy_java_Throwable__...(primary=None, e=None, …)`
   which executes `self.msg = ThrowablesSupport.throwableMessage(None, None)`
   (i.e. `None`). That overwrites the user's `"more arguments expected"`.

So Layer 3.1's rename made the previously distinct names
`Throwable.message` / `ParseError.msg` collide on the same Python attribute,
and the JVM-style "subclass body before super ctor" ordering in the
Scala-generated `_scpy_ctor_…` clobbers the subclass-set value as soon as
the super-ctor chain runs.

## Bisect / Suspects

The candidates listed in the brief (Layer 5 Wave 1/2/3 and Layer 5.1)
are red herrings. `_scpy_is_value_of_type` returns `True` here — the
catch already fires successfully. Confirmed by:

- inspecting the generated Python (`/tmp/out-pouring/Pouring.py:2199`),
- the traceback's "During handling" pointing to a *new* failure inside
  `showError`, not at the catch test, and
- the direct `_scpy_new` probe above showing `msg = None` independent of
  any catch path.

The offending commit is `1817f82bfa` (Layer 3.1). I did not run a literal
revert because the field-collision behavior is reproducible at the codegen
level: simply reading the generated Python and the runtime field-encoding
contract is sufficient.

## Suggested fix shape

Two viable paths, in order of scope:

1. **Targeted (matches the existing fix style).** Rename `Throwable.msg`
   back to something that won't collide with user code, e.g.
   `_throwableMsg` (private, leading underscore avoids the previous
   aux-ctor parameter `message` collision). Update the only two readers
   in `pylib-py/src/java/lang/Throwables.scala` (`getMessage` and the
   field initializer). The original `message`/parameter shadowing bug
   that motivated the rename doesn't reoccur because no aux-ctor
   parameter spells `_throwableMsg`. This is a one-file pylib edit;
   rebuild `scala-pylib-py` + `scala-library-py` and rerun the two
   fixtures.

2. **Structural (broader).** Field-name collisions between subclass
   `val`s and parent `val`s are now a latent footgun for any user class
   that extends a Scala/Java class with a same-named field. The robust
   fix is to qualify field storage names by the declaring owner — e.g.
   `_owner__msg` — at least when a parent class in the same hierarchy
   already declares a field of the same simple name. The
   `encodeFieldName` comment ("shadowing works via normal Python
   semantics") explicitly chose the current behavior; revisiting it is
   a backend-encoding change, not a pylib edit.

For an immediate revert-style restoration of the two fixtures, path 1 is
sufficient. Path 2 is worth a separate plan note because the same shape
will bite any future Scala class that names a `val` after a stdlib parent
field (`getMessage`'s `msg` is the only one currently in pylib, but the
encoding contract makes it a general issue).

## Working tree

`git status` shows only the original untracked files plus this new note;
no compiler or pylib sources were modified during investigation.
