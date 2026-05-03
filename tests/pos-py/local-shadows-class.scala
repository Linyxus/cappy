// Local val with the same name as a top-level class. Scala has separate
// type/term namespaces so this is a legal source pattern. Without proper
// reservation in the Python codegen the local would shadow the class for
// Python's function-scope rule and `new opq()` would raise
// `UnboundLocalError`. See `notes/wave5-worklist/04-assert-unbound-local.md`
// (`i12914` cluster fixture).

class Foo:
  def bar: Int = 7

object holder:
  val Foo = 99

@main def Test: Unit =
  // Term-name `Foo` collides with the class name `Foo`. The right-hand
  // `new Foo` must read the *class*, not the to-be-bound local.
  val Foo: Foo = new Foo()
  println(Foo.bar)

  // Param-name `Foo` collides too. `new Foo()` inside the body must
  // still see the module-level class binding.
  def make(Foo: Int): Int =
    val x = new Foo()
    x.bar + Foo
  println(make(11))

  // Multiple shadowing locals in the same method.
  val obj = holder
  println(obj.Foo)
