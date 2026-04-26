// Regression: `m(k) = m(k)` and `m(k) + m(k)` on a `HashMap[Int, Int]`
// lower the right-hand reads to `apply_mcII_sp__I__I`. Before the
// reachability fix in `PyReachability.scala` (and the runtime metaclass
// forwarder added to `Function0..22`), `MapOps.apply` was DCE'd and the
// runtime forwarder couldn't bridge the specialized form to it. See
// notes/issue-hashmap-apply-mcii-sp-missing.md.

import scala.collection.mutable.HashMap

@main def scalaHashMapIntRoundtrip(): Unit =
  val m = new HashMap[Int, Int]
  m(1) = 10
  m(2) = 20
  m(3) = 30

  // 1. idempotent overwrite — m(k) on the RHS used to throw
  //    `AttributeError: apply_mcII_sp__I__I` from the runtime forwarder.
  m(1) = m(1)
  println("roundtrip:" + m.get(1))

  // 2. arithmetic on `m(k)` — same lowering path, exercised twice.
  m(2) = m(2) + m(1)
  println("sum:" + m.get(2))

  // 3. explicit `.apply(k)` parses to the same specialized form.
  println("apply:" + m.apply(3))

  // 4. fold over keys reading via apply.
  var total = 0
  m.keys.foreach(k => total += m(k))
  println("foldRead:" + total)
