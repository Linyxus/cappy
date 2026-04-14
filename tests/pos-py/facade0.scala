import scala.python.*

// Phase 0 intentionally checks only that the facade vocabulary is visible to
// typer and that a real `-scalapy` run reaches the backend symbol cache.
// It does not exercise facade lowering, linker opacity, annotation encoding,
// or callable/varargs support; the known backend gap is pinned in tests/py-neg.
type Phase0PyMarker = PyAny
type Phase0DynamicMarker = PyDynamic
type Phase0ExternMarker = scala.python.extern
type Phase0NameMarker = scala.python.name
type Phase0DynamicModuleMarker = Dynamic.type

@main def facade0(): Unit =
  println("phase0")
