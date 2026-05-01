// Regression guard for `notes/issue-stockrun-timeout-cluster.md` Bug A.
// Prior to the fix, `library-py/.../ScalaRunTime.isArray`'s body
// `a.isInstanceOf[Array[?]]` was rewritten by TypeTestsCasts into
// `ScalaRunTime.isArray(a, 1)`, producing an infinite tail-call loop
// under the Python backend. Any user code reaching that helper hung.
@main def Test: Unit =
  val xs: Array[Long]  = Array(1L)
  val ys: Array[Int]   = Array(2, 3)
  val nested: Array[Array[Int]] = Array(Array(1), Array(2))
  val s: Any   = "hello"
  val n: Any   = null
  // Top-level cases — the rewrite path that used to loop:
  println(xs.isInstanceOf[Array[?]])
  println(ys.isInstanceOf[Array[?]])
  println(nested.isInstanceOf[Array[?]])
  println(s.isInstanceOf[Array[?]])
  println(n.isInstanceOf[Array[?]])
  // Direct dispatch through the runtime helper:
  println(scala.runtime.ScalaRunTime.isArray(xs))
  println(scala.runtime.ScalaRunTime.isArray(s))
  println(scala.runtime.ScalaRunTime.isArray(n))
  println(scala.runtime.ScalaRunTime.isArray(nested, 2))
  println(scala.runtime.ScalaRunTime.isArray(xs, 2))
