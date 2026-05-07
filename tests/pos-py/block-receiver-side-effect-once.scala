// Pins the invariant that a `Block` used as the receiver of a chained
// call evaluates its side-effecting statements exactly once, even when
// the call site is processed by `genTupleCallOpt`'s Product-polyfill
// branch.
//
// Regression: the polyfill used to call `genExpr(receiver)` eagerly to
// build a candidate helper-call argument tree. When no `Product`
// method matched, it returned `None`, but the receiver's
// pendingLocalDefs had already been pushed. The regular dispatch path
// in `genNormalApply` then re-evaluated the receiver, doubling the
// pushes per chain level. A two-level chain
// `{stmt; recv}.middle().end()` therefore ran `stmt` four times
// instead of once (`tests/run/runtime.scala`'s
// `{Console.print(23); test1.bar.System}.out().println()`).

class End:
  def fire(): Unit = ()

class Middle:
  def go(): End = End()

class Outer:
  def step(): Middle = Middle()

@main def blockReceiverSideEffectOnce(): Unit =
  val outer = Outer()

  // 0-level chain: one method call directly on the Block result.
  var c0 = 0
  { c0 += 1; outer }.step()
  println(c0)

  // 1-level chain: one .step().go() — without the fix, the Block
  // re-evaluates twice.
  var c1 = 0
  { c1 += 1; outer }.step().go()
  println(c1)

  // 2-level chain: matches the original `runtime.scala` shape
  // (`.out().println()`). Without the fix this counter reaches 4.
  var c2 = 0
  { c2 += 1; outer }.step().go().fire()
  println(c2)
