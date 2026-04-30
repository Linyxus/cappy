// Regression guard for the PyMethodName `__`-prefix guard fix. A Scala
// member named `$` sanitizes to simple `_`; without the guard, the
// encoded method name is `___<sig>` (3 leading underscores), which
// Python's compile-time private-name mangling rewrites at the call
// site (inside `class Test_:`) to `_Test____<sig>`, breaking attribute
// lookup on `c.$`.
class C:
  val x: Int = 1
  object `$`:
    val y: Int = 42

@main def run(): Unit =
  val c = C()
  println(c.`$`.y)
