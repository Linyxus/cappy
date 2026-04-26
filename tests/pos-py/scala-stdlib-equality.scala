// `==` / `hashCode` / `sameElements` across the 12 stdlib types we already
// cover. Probes the equality semantics + hashCode bridge between Scala and
// Python. Each type prints `eq:`, `hashEq:`, `sameElems:` for a pair of
// equal-by-content collections.
//
// For Map/Set types `sameElements` either doesn't exist or has order-
// dependent semantics, so the third column reports `na` for those types.

import scala.collection.immutable.{HashMap as IMap, HashSet as ISet, TreeMap}
import scala.collection.mutable.{ArrayBuffer, ArrayDeque, ListBuffer, StringBuilder, HashMap as MMap, HashSet as MSet}

@main def scalaStdlibEquality(): Unit =
  // 1. List
  val l1 = List(1, 2, 3, 4)
  val l2 = List(1, 2, 3, 4)
  val l3 = List(1, 2, 9, 4)
  println("List:eq:" + (l1 == l2) + ":hashEq:" + (l1.hashCode == l2.hashCode) + ":sameElems:" + l1.sameElements(l2) + ":neq:" + (l1 == l3))

  // 2. Vector
  val v1 = Vector(1, 2, 3, 4)
  val v2 = Vector(1, 2, 3, 4)
  val v3 = Vector(1, 2, 9, 4)
  println("Vector:eq:" + (v1 == v2) + ":hashEq:" + (v1.hashCode == v2.hashCode) + ":sameElems:" + v1.sameElements(v2) + ":neq:" + (v1 == v3))

  // 3. LazyList
  val ll1 = LazyList(1, 2, 3, 4)
  val ll2 = LazyList(1, 2, 3, 4)
  val ll3 = LazyList(1, 2, 9, 4)
  println("LazyList:eq:" + (ll1 == ll2) + ":hashEq:" + (ll1.hashCode == ll2.hashCode) + ":sameElems:" + ll1.sameElements(ll2) + ":neq:" + (ll1 == ll3))

  // 4. ArrayBuffer
  val ab1 = ArrayBuffer(1, 2, 3, 4)
  val ab2 = ArrayBuffer(1, 2, 3, 4)
  val ab3 = ArrayBuffer(1, 2, 9, 4)
  println("ArrayBuffer:eq:" + (ab1 == ab2) + ":hashEq:" + (ab1.hashCode == ab2.hashCode) + ":sameElems:" + ab1.sameElements(ab2) + ":neq:" + (ab1 == ab3))

  // 5. ArrayDeque
  val ad1 = ArrayDeque(1, 2, 3, 4)
  val ad2 = ArrayDeque(1, 2, 3, 4)
  val ad3 = ArrayDeque(1, 2, 9, 4)
  println("ArrayDeque:eq:" + (ad1 == ad2) + ":hashEq:" + (ad1.hashCode == ad2.hashCode) + ":sameElems:" + ad1.sameElements(ad2) + ":neq:" + (ad1 == ad3))

  // 6. ListBuffer
  val lb1 = ListBuffer(1, 2, 3, 4)
  val lb2 = ListBuffer(1, 2, 3, 4)
  val lb3 = ListBuffer(1, 2, 9, 4)
  println("ListBuffer:eq:" + (lb1 == lb2) + ":hashEq:" + (lb1.hashCode == lb2.hashCode) + ":sameElems:" + lb1.sameElements(lb2) + ":neq:" + (lb1 == lb3))

  // 7. immutable HashMap — sameElements not meaningful (hash order); report na.
  val im1 = IMap(1 -> 10, 2 -> 20, 3 -> 30)
  val im2 = IMap(3 -> 30, 1 -> 10, 2 -> 20)
  val im3 = IMap(1 -> 10, 2 -> 99, 3 -> 30)
  println("ImmutableHashMap:eq:" + (im1 == im2) + ":hashEq:" + (im1.hashCode == im2.hashCode) + ":sameElems:na:neq:" + (im1 == im3))

  // 8. immutable HashSet — order-irrelevant equality.
  val is1 = ISet(1, 2, 3, 4)
  val is2 = ISet(4, 3, 2, 1)
  val is3 = ISet(1, 2, 3, 99)
  println("ImmutableHashSet:eq:" + (is1 == is2) + ":hashEq:" + (is1.hashCode == is2.hashCode) + ":sameElems:na:neq:" + (is1 == is3))

  // 9. mutable HashMap.
  val mm1 = MMap("a" -> 1, "b" -> 2, "c" -> 3)
  val mm2 = MMap("c" -> 3, "a" -> 1, "b" -> 2)
  val mm3 = MMap("a" -> 1, "b" -> 2, "z" -> 99)
  println("MutableHashMap:eq:" + (mm1 == mm2) + ":hashEq:" + (mm1.hashCode == mm2.hashCode) + ":sameElems:na:neq:" + (mm1 == mm3))

  // 10. mutable HashSet.
  val ms1 = MSet(1, 2, 3, 4)
  val ms2 = MSet(4, 3, 2, 1)
  val ms3 = MSet(1, 2, 3, 99)
  println("MutableHashSet:eq:" + (ms1 == ms2) + ":hashEq:" + (ms1.hashCode == ms2.hashCode) + ":sameElems:na:neq:" + (ms1 == ms3))

  // 11. immutable TreeMap.
  val tm1 = TreeMap(1 -> 10, 2 -> 20, 3 -> 30)
  val tm2 = TreeMap(3 -> 30, 1 -> 10, 2 -> 20)
  val tm3 = TreeMap(1 -> 10, 2 -> 99, 3 -> 30)
  println("TreeMap:eq:" + (tm1 == tm2) + ":hashEq:" + (tm1.hashCode == tm2.hashCode) + ":sameElems:na:neq:" + (tm1 == tm3))

  // 12. mutable StringBuilder — equality is reference-identity for builders;
  //     compare via .toString to keep semantics deterministic.
  val sb1 = new StringBuilder("hello")
  val sb2 = new StringBuilder("hello")
  val sb3 = new StringBuilder("world")
  println("StringBuilder:eq:" + (sb1.toString == sb2.toString) + ":hashEq:" + (sb1.toString.hashCode == sb2.toString.hashCode) + ":sameElems:" + sb1.sameElements(sb2) + ":neq:" + (sb1.toString == sb3.toString))
