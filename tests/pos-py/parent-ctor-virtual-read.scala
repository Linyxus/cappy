// Layer 5.7 follow-up regression guard.
// dotc's `Constructors` phase emits `copyParams ::: super ::: stats`. Layer
// 5.7 reordered super ahead of copyParams so an `override val` in the
// subclass wouldn't be clobbered by the parent's copyParams, but that
// reordering broke the i763 / Signals2 pattern: a parent's primary ctor
// virtually invokes a child accessor that reads a field set only by the
// subclass's copyParams. With copyParams running *only* after super, the
// child's field is still the zero-init default when the parent reads it.
//
// The backend now duplicates copyParams: once before the super call (so
// virtual dispatch into the child sees the right value) and once after
// (so the parent's copyParams can't overwrite the child's).

abstract class A:
  val s: Int
  // Parent reads a child-set field through a virtual accessor; without
  // pre-super copyParams this would observe the zero-init default `0`.
  assert(s == 1, s"expected s == 1, got $s")

class B(val s: Int) extends A

class Parent(val msg: String):
  override def toString: String = s"Parent($msg)"

// Mirrors override-val-ctor-order. Without the post-super copyParams,
// Parent's copyParam would clobber Child's `self.msg = childMsg`.
class Child(idx: Int, override val msg: String) extends Parent(s"parent-$idx"):
  override def toString: String = s"Child($idx, $msg)"

@main def run(): Unit =
  val b = B(1)
  println(b.s)

  val c = Child(7, "child-msg")
  println(c.msg)
  println(c)
  println((c: Parent).msg)
