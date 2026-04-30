// Layer 5.3 regression guard.
// `Mirror.SingletonProxy.fromProduct(p): MirroredMonoType` erases to
// `Object`, so the encoded method name is
// `fromProduct__Lscala_dProduct__Ljava_dlang_dObject`. Earlier the
// hand-written runtime declared `fromProduct__Lscala_dProduct__O`
// (where `O` was a placeholder for the abstract type member), which
// didn't match the call site's erased shape and surfaced as
// `AttributeError: 'Mirror_SingletonProxy' object has no attribute ...`
// in `tests/run/i13332intersection.scala`.
import scala.deriving.Mirror

case object Foo

@main def run(): Unit =
  val m = summon[Mirror.Of[Foo.type]]
  println(m.fromProduct(EmptyTuple))
