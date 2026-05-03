// Regression guard for `scala.util.boundary` lowering.
//
// Before the fix, the labeled-block lowering in `genLabeledExpr`
// rewrote every explicit `Return(label, v)` to `temp = v; raise lbl`
// but never assigned the labeled body's *fallthrough* tail value
// to the temp. So a `boundary { if cond then break(a); b }` returned
// `null`/zero whenever `cond` was false because the fallthrough `b`
// path produced a value that was emitted as a bare statement and
// silently discarded. This fixture covers:
//   - The simple early-exit and no-break paths.
//   - A no-break path inside an inner try/catch (the
//     `otherRefs > 1` shape from `DropBreaks`, which keeps the
//     boundary's try/catch wrapping intact).
//   - A `Unit`-typed labeled block where no temp is needed.
//   - A nested boundary where the inner fallthrough flows into the
//     outer fallthrough.

import scala.util.boundary, boundary.break

object Cases:

  def simple(arg: Int): String =
    boundary:
      if arg < 0 then break("negative")
      "non-negative"

  // Forces `xs.map` to capture the label, so DropBreaks keeps the
  // outer try/catch (otherRefs > 1) and the fallthrough lives inside
  // the inner try block.
  def viaMap(xx: Int, xs: List[Int]): Int =
    boundary:
      if xx < 0 then break(-1)
      xs.map: y =>
        if y < 0 then break(-2)
        y
      xx + xs.sum

  def unitBoundary(arg: Int): Unit =
    boundary:
      if arg < 0 then break()
      ()  // fallthrough — Unit, no assignment needed

  def nested(arg: Int): Int =
    boundary:
      val inner =
        boundary:
          if arg == 0 then break(100)
          arg + 1
      inner * 2

@main def Test =
  import Cases.*
  assert(simple(1)  == "non-negative")
  assert(simple(-1) == "negative")
  assert(viaMap(1,  List(1, 2, 3)) == 7)
  assert(viaMap(-1, List(1, 2, 3)) == -1)
  assert(viaMap(1,  List(1, -2, 3)) == -2)
  unitBoundary(1)
  unitBoundary(-1)
  assert(nested(0) == 200)
  assert(nested(3) == 8)
  println("ok")
