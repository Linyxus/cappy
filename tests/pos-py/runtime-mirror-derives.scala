// Phase-2 shrink-runtime regression guard.
// `notes/shrink-runtime.md` Category B drops the runtime stubs for
// `scala.deriving.Mirror`, `Mirror_Product`, `Mirror_Sum`,
// `Mirror_Singleton`, and `Mirror_SingletonProxy`. After the drop, the
// `library-py`-compiled `scala.deriving.Mirror.pyir` carries:
//   - abstract slots `Mirror_Product.fromProduct` / `Mirror_Sum.ordinal`
//     (concrete subclass overrides are emitted by user-derived mirrors)
//   - concrete `Mirror_Singleton.fromProduct` returning `this`
//   - concrete `Mirror_SingletonProxy.{ctor, value, fromProduct}`
//
// This fixture exercises the three runtime-relevant call shapes:
//   1. ProductOf  → user-synthesized mirror ⇒ override on case class
//   2. Of[Color]  → Sum via enum's own `ordinal__I` (mirror just routes)
//   3. SingletonProxy.fromProduct returning `value`

import scala.deriving.Mirror

case class Pair(x: Int, y: String)

enum Color:
  case Red, Green, Blue

@main def runtimeMirrorDerives(): Unit =
  // Product mirror, user-synthesized override path.
  val pm = summon[Mirror.ProductOf[Pair]]
  val p = pm.fromProduct((42, "ok"))
  println(p)

  // Sum mirror, enum ordinal dispatch.
  val sm = summon[Mirror.Of[Color]]
  println(sm.ordinal(Color.Red))
  println(sm.ordinal(Color.Green))
  println(sm.ordinal(Color.Blue))

  // SingletonProxy concrete ctor + fromProduct.
  val sp = new Mirror.SingletonProxy("hi")
  println(sp.value)
  println(sp.fromProduct(EmptyTuple))
