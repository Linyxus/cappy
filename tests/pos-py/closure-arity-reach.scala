// Regression guard for Phase 3b of `notes/shrink-runtime.md`.
// After dropping `scala.Function*` from `providedClasses`, the
// closure-carrier emitter (`PyIREmitter.emitClosureCarriers`) only
// generates `_scpy_FnN` for arities whose `scala.FunctionN` is in
// `knownClasses`. The user code may build a closure of arity N
// without any other reference to `scala.FunctionN` as a nominal
// class — e.g. when the closure is the sole supertype-ctor argument
// to a `Bar(val ctor: Int => T)`-shape parent. Without seeding
// `scala.FunctionN` from the closure's arity in `PyReachability`,
// `_scpy_FnN` would be missing and the bundle NameErrors at module
// init.
//
// `tests/run/t2127.scala` is the upstream shape that surfaced this;
// this fixture is a smaller direct repro.

class Holder private (val v: Int)

abstract class Make(val ctor: Int => Holder)

object Holder extends Make(new Holder(_))

@main def closureArityReach(): Unit =
  // Arity-2 closure passed to a higher-order helper that stores it.
  // The `+`-of-Int closure body would otherwise inline; route it
  // through `Function.const`-like wrapper to force the carrier.
  val mul: (Int, Int) => Int = (x, y) => x * y
  println(mul(3, 7))                      // 21

  // Arity-3 closure as a real value that escapes into a `def`.
  val sum3: (Int, Int, Int) => Int = (a, b, c) => a + b + c
  def call3(f: (Int, Int, Int) => Int) = f(10, 20, 30)
  println(call3(sum3))                    // 60

  // Force Holder's module init (which uses an arity-1 closure as
  // the supertype ctor argument). If `_scpy_Fn1` was DCE-pruned
  // post-3b, this NameErrors.
  val h = Holder.ctor(42)
  println(h.v)                            // 42
