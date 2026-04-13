import scala.python.*

// Phase 0 gap: a facade `def` with a `PyAny` parameter produces an IR method
// whose param type is `PyClassRef(scala.python.PyAny)`. PyLinker rejects it
// with "Unresolved class 'scala.python.PyAny'" because `scala.python.*` is
// not yet recognised as an opaque external. The linker visits the type in
// two places, so the same error is raised twice; we pin both with
// `anypos-error` to stay position-agnostic should the traversal shape change.
//
// Phase 1 should make this compile by treating `@extern`-owned types as
// linker-opaque. When fixed, this file stops producing errors and the neg
// test breaks - move it to tests-py at that point.
// anypos-error
// anypos-error
@extern("phase0_extern_def", "accept")
def phase0ExternDef(x: PyAny): Unit = ()
