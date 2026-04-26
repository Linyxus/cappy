// Regression: List.apply(i) for i large enough that drop falls through
// to StrictOptimizedLinearSeqOps's static `loop$2` helper. The static
// helper is lifted onto the trait interface itself (no `$` companion
// ModuleClass), so the emitter can't route through `_scpy_mod_*_` —
// it must call `Class.method(...)` directly on the @staticmethod.
// See notes/issue-list-apply-missing-strictoptimizedlinearseqops-module.md.

@main def reproListApply(): Unit =
  val xs = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
  println(xs(8))
