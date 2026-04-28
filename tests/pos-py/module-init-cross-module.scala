// Cross-module init: `ModuleA` reads a field from `ModuleB` during
// its OWN top-level initializer. The lazy-module proxy must trigger
// `ModuleB`'s `__init__` when `ModuleA.__init__` reads
// `ModuleB.value`, otherwise the read sees the uninitialised
// class-level `None` default.
//
// Regression fence for the LazyModule access path documented at the
// top of `emitBundle` in PyIREmitter.scala.

object ModuleB:
  val value: String = "hello-from-B"
  val number: Int = 42

object ModuleA:
  // These read ModuleB during ModuleA's init. Without the LazyModule
  // proxy these would race: ModuleB might not have run __init__ yet.
  val derived: String = ModuleB.value + "-thru-A"
  val doubled: Int = ModuleB.number * 2

@main def moduleInitCrossModule(): Unit =
  println("A.derived=" + ModuleA.derived)
  println("A.doubled=" + ModuleA.doubled)
  println("B.value=" + ModuleB.value)
  println("B.number=" + ModuleB.number)
