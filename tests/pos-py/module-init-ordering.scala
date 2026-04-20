/*
 * Regression fence for the module-init-ordering fix in L3.3's emitter
 * rewrite. Scenario: an `object` whose initializer references a class
 * declared *later* in the source file. The emitter must emit every
 * class body first and only then run module singleton inits, otherwise
 * Python raises `NameError` when the object tries to construct
 * `LateClass` before its `class` statement has been evaluated.
 *
 * Previously this shape broke because each class's module singleton
 * got initialised right after its own class body; now the emitter
 * does a two-pass emission (all classes, then all module-class
 * singletons).
 *
 * KNOWN GAP: module-to-module init ordering is NOT topologically
 * sorted — if `ObjectA`'s init references `ObjectB`'s fields, `ObjectB`
 * must appear first in the emitted init pass. See
 * `notes/issue-module-init-ordering-module-dependency.md`. This test
 * only exercises module → later-class dependency, not module → module.
 */

// The object comes first in source order. Its initializer constructs
// `LateClass` which is declared further down.
object EarlyModule:
  val eagerInstance: LateClass = new LateClass("eager")
  val tag: String = eagerInstance.describe()

// Deliberately placed AFTER the object. In Python emission this class
// must be defined before `EarlyModule`'s singleton is instantiated.
// `describe` is a pure method — no module-level references to avoid
// the still-open module→module ordering bug.
class LateClass(prefix: String):
  def describe(): String = prefix + "-described"

@main def moduleInitOrdering(): Unit =
  println("early:" + EarlyModule.tag)
  println("late-instance:" + new LateClass("fresh").describe())
