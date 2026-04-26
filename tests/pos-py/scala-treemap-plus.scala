// Regression test for `notes/issue-treemap-plus-self-recursive.md`.
//
// Scala collection traits like `MapOps` declare multiple `+` overloads
// (e.g. `+ (kv: (K, V))` and `+ (e1, e2, elems*)`). Both overloads used
// to encode to the Python dunder `__add__`, dropping the signature
// suffix, so all overloads collapsed onto one Python identifier and the
// synthesized forwarders became `return self.__add__(...)` self-calls.
// Any `tm + (k -> v)` then raised `RecursionError`.
//
// The fix removes the operator-to-dunder mapping in `PyEncoding`. Now
// each overload encodes to a unique `_$plus__<sig>__<ret>` identifier.
// Cover several map / set classes plus chained `+` and `-` to ensure
// the operator class is broadly unbroken.

import scala.collection.immutable.{TreeMap, HashMap, HashSet, ListMap}

@main def scalaTreemapPlus(): Unit =
  // 1. Basic `tm.+(kv)` — the bug minimal repro.
  val tm = TreeMap(1 -> 10, 2 -> 20)
  val tm1 = tm.+(3 -> 30)
  println("tm1:" + tm1.size + ":" + tm1.get(3).getOrElse(-1))

  // 2. Infix `tm + kv` chained.
  val tm2 = tm + (3 -> 30) + (4 -> 40)
  println("tm2:" + tm2.size + ":" + tm2.get(3).getOrElse(-1) + ":" + tm2.get(4).getOrElse(-1))

  // 3. `-` operator (also overloaded across the trait hierarchy).
  val tm3 = tm - 1
  println("tm3:" + tm3.size + ":" + tm3.get(1).getOrElse(-1) + ":" + tm3.get(2).getOrElse(-1))

  // 4. Same shape on `HashMap` (different inheritance chain).
  val hm = HashMap(1 -> 10, 2 -> 20)
  val hm1 = hm + (3 -> 30)
  println("hm1:" + hm1.size + ":" + hm1.get(3).getOrElse(-1))

  // 5. ListMap — yet another concrete map.
  val lm = ListMap(1 -> 10, 2 -> 20)
  val lm1 = lm + (3 -> 30)
  println("lm1:" + lm1.size + ":" + lm1.get(3).getOrElse(-1))

  // 6. HashSet `+` and `-` (Set also has multiple `+` overloads).
  val hs = HashSet(1, 2, 3)
  val hs1 = hs + 4
  val hs2 = hs1 - 2
  println("hs1:" + hs1.size + ":" + hs1.contains(4))
  println("hs2:" + hs2.size + ":" + hs2.contains(2) + ":" + hs2.contains(4))
