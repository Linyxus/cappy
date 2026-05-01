// Regression guard for sequential `PyLabeled` blocks in one method:
// each pattern match below lowers to its own labelled escape; with
// the hoist, both `_scpy_lbl_1` and `_scpy_lbl_2` are declared at
// method scope (counter monotonic per method) and both must work
// correctly within one invocation.

def selector(n: Int): Int = n

@main def labeledSequentialBlocks(): Unit =
  // First match: produces a String via labelled escape.
  val s = selector(1) match
    case 0 => "zero"
    case 1 => "one"
    case _ => "other"

  // Second match: produces an Int via a separate labelled escape.
  val i = selector(2) match
    case 0 => 100
    case 2 => 200
    case _ => 999

  // Same method, both labels must be in scope and distinct.
  println(s"$s/$i")

  // Third: a method-tail match (peephole-eligible) — verifies the
  // peephole still fires in the presence of earlier hoisted labels.
  println(selector(0) match
    case 0 => "tail-zero"
    case _ => "tail-other"
  )
