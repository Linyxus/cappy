// Regression test for `@scala.annotation.static` method dispatch on the
// Python backend.
//
// `MoveStatics` lifts `@static def`s from the module class onto the
// companion class, but it intentionally does not set the `JavaStatic`
// flag on the lifted symbol (see `compiler/src/dotty/tools/dotc/transform/
// MoveStatics.scala:50`). The Python backend used to gate "emit as
// `@staticmethod` / dispatch via `<Owner>.<m>(...)`" purely on the
// `JavaStatic` flag, which left `@static`-annotated members emitted as
// regular instance methods on the companion class while every call site
// routed through the module receiver. The mismatch surfaced at runtime
// as e.g. `AttributeError: 'Test_' object has no attribute 'square__I__I'`
// (the declaration is on `Holder`, the call site went through `Test_`).
//
// The fix mirrors the JVM/Scala.js backends' `isStaticMember` check
// (`JavaStatic` ∪ `isScalaStatic`) at every static-vs-instance branch,
// so the declaration site lands a `@staticmethod` on the companion class
// and the call site emits `Holder.square(...)` directly. This fixture
// exercises a method-only `@static def`; field handling for `@static
// val/var` is a separate follow-up.

import scala.annotation.static

class Holder

object Holder:
  @static def square(n: Int): Int = n * n

  @static def label(prefix: String, n: Int): String = s"$prefix=$n"

  @static def sumOfSquares(xs: List[Int]): Int =
    xs.map(x => x * x).sum

object Test:
  def main(args: Array[String]): Unit =
    println(Holder.square(7))
    println(Holder.label("answer", 42))
    println(Holder.sumOfSquares(List(1, 2, 3, 4)))
