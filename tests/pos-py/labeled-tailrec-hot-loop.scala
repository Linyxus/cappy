// Regression guard for the method-scope hoist of `_scpy_lbl_<n>` label
// classes: a tail-recursive method with a non-trivial iteration count
// must still produce the right result. Pre-hoist, the label class was
// redefined every iteration; post-hoist, it's defined once per call.
// 100k iterations are well under any harness timeout but big enough
// that pre-hoist regressions would surface.

import scala.annotation.tailrec

@tailrec
def countDown(i: Int, acc: Int): Int =
  if i <= 0 then acc
  else countDown(i - 1, acc + 1)

@main def labeledTailrecHotLoop(): Unit =
  println(countDown(100000, 0))
