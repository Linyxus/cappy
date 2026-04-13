import scala.python.*

// Phase 0 gap: PyLinker has no notion of facade opacity and genCompilationUnit
// does not yet skip `@extern`-annotated classes. The `extends PyAny` parent
// therefore reaches `PyLinker.requireClass`, which fails with
// "Unresolved interface 'scala.python.PyAny'".
//
// Phase 1 should make this compile by either:
//   (a) skipping `@extern` classes at genCompilationUnit (preferred), or
//   (b) teaching the linker to treat `scala.python.*` as opaque externals.
//
// When Phase 1 lands this file should stop producing the error and break
// the neg test - at that point move it to tests-py as a positive test.
@extern("phase0_facade_gap")
object phase0KnownGap extends PyAny // error
