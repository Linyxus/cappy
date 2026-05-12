// The value-position rewrite at GenPython.scala:3479–3487 turns
//   PyLabelReturn(label, v)
// in value position into
//   { tempLhs := v; PyLabelReturn(label, ()) }.
// `v` must appear exactly once in the lowered IR — if a future
// refactor duplicated it (e.g. put `v` into both the assignment AND
// the return slot, or evaluated it both before and after the
// rewrite), a side-effecting `v` would fire twice.
//
// We probe this by making `v` a counter-incrementing call, then
// asserting both the returned value and the counter agree with
// a single evaluation.

var calls = 0

def bump(tag: String, payload: Int): Int =
  calls += 1
  println(s"bump:$tag")
  payload

def viaMatchValuePosition(n: Int): Int =
  val r = n match
    case 0 => bump("zero", 10)
    case 1 => bump("one",  20)
    case _ => bump("other", 30)
  r * 2

def viaMatchStatementPosition(n: Int): Unit =
  n match
    case 0 => println("stmt:" + bump("s-zero", 100))
    case _ => println("stmt:" + bump("s-other", 200))

@main def labeledValueEvaluatedOnce(): Unit =
  calls = 0
  println("vp(0):" + viaMatchValuePosition(0))
  println("calls-after-vp0:" + calls)

  calls = 0
  println("vp(7):" + viaMatchValuePosition(7))
  println("calls-after-vp7:" + calls)

  calls = 0
  viaMatchStatementPosition(0)
  println("calls-after-sp0:" + calls)

  calls = 0
  viaMatchStatementPosition(5)
  println("calls-after-sp5:" + calls)
