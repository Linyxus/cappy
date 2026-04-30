// Layer 5.7 regression guard.
// dotc's `Constructors` phase emits `copyParams ::: super ::: stats`, so
// without reordering, the parent ctor's `self.msg = parentMsg` overwrites
// the subclass's `self.msg = childMsg`. JVM bytecode requires super first;
// our backend now reorders to match.
class Parent(val msg: String):
  override def toString: String = s"Parent($msg)"

class Child(idx: Int, override val msg: String) extends Parent(s"parent-$idx"):
  override def toString: String = s"Child($idx, $msg)"

@main def run(): Unit =
  val c = Child(7, "child-msg")
  println(c.msg)
  println(c)
  println((c: Parent).msg)
