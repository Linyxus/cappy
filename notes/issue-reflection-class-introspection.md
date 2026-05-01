# `Class.getDeclaredFields/Methods/getField/getMethods` partially stubbed

## Status (Layer 4.1 — bd229b6839)

The runtime now exposes empty/null/no-op stubs for the JDK reflection
surface most fixtures touch incidentally:
`getDeclared{Fields,Methods,Constructors,Classes,Annotations}`,
`get{Fields,Methods,Constructors,Classes}`, `getDeclaredField/Method/
Constructor` (raise `NoSuchField/MethodException`),
`getEnclosingMethod/Constructor/Class`, `getEnumConstants`,
`getModifiers`, `getCanonicalName`, `getTypeName`, `getPackageName`,
`getNestHost/Members`, `getGenericInterfaces`, `getGenericSuperclass`,
`getTypeParameters`, plus annotation/modifier predicates and
`asSubclass`, `cast`, `newInstance`. `java.lang.reflect.Modifier`,
`Constructor`, `Type`, `TypeVariable`, `Executable`,
`InvocationTargetException` are wired with `_scpy_register_class` and
linker entries; `ClassLoader.loadClass` falls back to the runtime
registry then `Class.forName`.

Fixtures that only check shape ("no field starting with `foo$`")
flip green. Fixtures that consume actual reflective data
(`paramForwarding` printing `private final int B.theValue`, `i18701`
expecting an enclosing-method `toString`, `t7932` filtering the
`Method[]` array, the `scala.reflect.Selectable` family that
materializes fields via `rcls.getField(...)`) still fail at output-
mismatch / assertion stage because no real reflective metadata is
preserved.

Going further requires threading declared-method/field tables into
`_scpy_register_class` and synthesizing `Field`/`Method`/`Constructor`
instances at class-construction time. Out of scope for this layer.

## Original notes

## Minimal example for reproducing

```scala
@main def Test =
  println(classOf[String].getDeclaredFields.length)
  println(classOf[String].getDeclaredMethods.length)
```

Affected fixtures (~13): `tests/run/nothing-var.scala`,
`tests/run/null-var.scala`, `tests/run/unit-val.scala`,
`tests/run/unit-volatile-var.scala`, `tests/run/variable-pattern-access.scala`,
`tests/run/i21346.scala`, `tests/run/returned-context-function-signature.scala`,
`tests/run/i1284.scala`, `tests/run/i18612-a.scala`, `tests/run/i18612-c.scala`,
`tests/run/i4496b.scala`, `tests/run/safeThrowsStrawman2.scala`,
`tests/run/t7932.scala`.

## Output vs Expected Behaviour

```
AttributeError: getDeclaredFields__ALjava_dlang_dreflect_dField
AttributeError: getDeclaredMethods__ALjava_dlang_dreflect_dMethod
AttributeError: getField__Ljava_dlang_dString__Ljava_dlang_dreflect_dField
AttributeError: getMethods__ALjava_dlang_dreflect_dMethod
```

Expected: returns the declared field/method arrays as JVM reflection would.

## Quick Analysis

Pylib's `java.lang.Class` does not expose any reflective introspection
methods. Synthesizing `Field` and `Method` objects from class metadata is a
multi-day project: it requires storing the class's declared-field and
declared-method lists in the runtime class registration so they can be
walked at run time, plus implementing `Field` / `Method` wrapper classes.

For the affected fixtures, reflection is incidental (they typically use it
to print field names for assertion purposes). Most tests would be passing
otherwise — the reflection call is a side-channel.

**Recommended action: excludelist** with reason for each, until reflection
support is a deliberate priority. Add to
`py-compiler-tests/test/run-py-tests.excludelist`:

```
nothing-var.scala                          # uses Class.getDeclaredFields; reflection not supported
null-var.scala                             # uses Class.getDeclaredFields; reflection not supported
unit-val.scala                             # uses Class.getDeclaredFields; reflection not supported
unit-volatile-var.scala                    # uses Class.getDeclaredFields; reflection not supported
variable-pattern-access.scala              # uses Class.getDeclaredFields; reflection not supported
i21346.scala                               # uses Class.getDeclaredMethods; reflection not supported
returned-context-function-signature.scala  # uses Class.getDeclaredMethods; reflection not supported
i1284.scala                                # uses Class.getField; reflection not supported
i18612-a.scala                             # uses Class.getField; reflection not supported
i18612-c.scala                             # uses Class.getField; reflection not supported
i4496b.scala                               # uses Class.getField; reflection not supported
safeThrowsStrawman2.scala                  # uses Class.getField; reflection not supported
t7932.scala                                # uses Class.getMethods; reflection not supported
```

If reflection becomes a goal later, partial support could be added by
threading declared-method/field lists into runtime class metadata; the
runtime already maintains `_scpy_class` registration so the entry point
exists. Out of scope for v1.

Specialist report: `/tmp/pyrun-analysis/cat-d-attr-errors.md` (Cat E),
`/tmp/pyrun-analysis/cat-b-other-linker.md` (related linker excludelist
entries for `java.lang.reflect.Executable`, `Modifier`,
`SerializedLambda`, `MethodType`/`MethodHandle`).
