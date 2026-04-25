// Regression test: `Predef.intWrapper` (and sibling primitive wrappers)
// inlined inside instance methods used to be lowered as `self.intWrapper(...)`
// instead of a Predef-receiver dispatch. That broke any use of
// `intParam max c`-style code in instance bodies, including capacity-taking
// constructors of mutable buffers.
//
// See notes/issue-predef-intwrapper-instance-dispatch.md.

import scala.collection.mutable.{ArrayBuffer, ArrayDeque, StringBuilder}

class Box(val n: Int):
  // `n max 0` desugars to `intWrapper(n).max(0)`. Before the fix, `intWrapper`
  // was emitted as a virtual call on `self`, which fails at runtime because
  // `Box` does not extend `LowPriorityImplicits`.
  def upper: Int        = n max 0
  def lower: Int        = n min 100
  def dbl(d: Double): Double = d max 0.0
  def lng(l: Long): Long     = l max 0L

@main def predefWrapperInstanceDispatch(): Unit =
  val box = new Box(42)
  println("box:" + box.upper + ":" + box.lower + ":" + box.dbl(-3.5) + ":" + box.lng(-7L))

  val ab = new ArrayBuffer[Int](16)
  ab += 1
  ab += 2
  println("ab:" + ab.size)

  val ad = new ArrayDeque[Int](8)
  ad += 99
  println("ad:" + ad.size)

  val sb = new StringBuilder(16)
  sb.append("hi")
  println("sb:" + sb.length())
