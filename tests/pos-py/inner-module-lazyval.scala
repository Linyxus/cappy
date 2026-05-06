// Regression guard: a `lazy val` inside an `object` nested in a
// trait used to trip a NameError on the bundle's first access.
//
// After `Flatten`, the inner module class `Outer.Inner$` has its
// owner moved to the package, so `isForwarderCandidate` accepted it
// and `emitWithStaticForwarders` synthesised a top-level
// `class Outer_Inner` whose method bodies referenced
// `_scpy_module_value(_scpy_mod_Outer_Inner__)`. That binding only
// exists for module classes with a no-arg constructor — inner
// modules carry an `_outer` argument and so are skipped by the
// `emitPreamble` singleton filter. Worse, the same
// `_scpy_mod_*_`-routed shape leaked into `PySelectStatic` for the
// `LazyVals`-lifted `_lzyHandle` field, so `compareAndSet` blew up
// with `NameError: name '_scpy_mod_Outer_Inner__' is not defined`
// the first time user code touched the lazy val.
//
// `tests/run/i13332a.scala` is the upstream Mirror-derivation
// shape that surfaced this; the fixture below is a direct repro.

trait Outer:
  object Inner:
    lazy val n: Int = 42

class Sub extends Outer

@main def innerModuleLazyVal(): Unit =
  val s = Sub()
  println(s.Inner.n)
