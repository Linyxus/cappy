// Exercise constructor overloads with shared/numeric Python representations
// (Int and Long both look like Python int; Boolean is a subclass of int).
// The backend resolves overloads at codegen time by encoded signature, so
// each `new CtorOverload(...)` site goes to a distinct ctor helper.
class CtorOverload(val s: String):
  def this(n: Int)     = this("int:" + n.toString)
  def this(n: Long)    = this("long:" + n.toString)
  def this(b: Boolean) = this("bool:" + b.toString)

@main def run(): Unit =
  println(new CtorOverload("hi").s)
  println(new CtorOverload(42).s)
  println(new CtorOverload(7L).s)
  println(new CtorOverload(true).s)
