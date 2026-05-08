// Pins `PyMap` iteration views, copy, merge, popItem, and bulk update.
// Sorted output keeps results deterministic regardless of dict
// insertion order or Python version.

import scala.python.PyMap

@main def pymapViews(): Unit =
  val m = PyMap.empty[String, Int]()
  m.update("b", 2)
  m.update("c", 3)
  m.update("a", 1)

  // Snapshot views materialise to Scala iterators.
  println(m.keys().toList.sorted.mkString(","))    // a,b,c
  println(m.values().toList.sorted.mkString(","))  // 1,2,3
  println(
    m.items().toList.map((k, v) => s"$k->$v").sorted.mkString(",")
  )                                                // a->1,b->2,c->3

  // Tuple destructuring through items() works because dict items
  // are wrapped as Scala tuples in `_scpy_dict_items_iter`.
  var sum = 0
  for (_, v) <- m.items() do sum += v
  println(sum)                                     // 6

  // copy is independent.
  val m2 = m.copy()
  m2.update("a", 99)
  println(m("a"))                                  // 1
  println(m2("a"))                                 // 99

  // merged returns a new dict; lhs is unchanged.
  val n = PyMap.empty[String, Int]()
  n.update("a", 10)
  n.update("d", 4)
  val merged = m.merged(n)
  println(merged("a"))                             // 10  (rhs wins)
  println(merged("b"))                             // 2
  println(merged("d"))                             // 4
  println(m("a"))                                  // 1   (lhs untouched)

  // mergeInPlace mutates lhs.
  m.mergeInPlace(n)
  println(m("a"))                                  // 10
  println(m("d"))                                  // 4

  // updateAll is the bulk-update form.
  val q = PyMap.empty[String, Int]()
  q.update("x", 100)
  q.updateAll(n)
  println(q.size())                                // 3
  println(q("x"))                                  // 100
  println(q("a"))                                  // 10

  // popItem removes and returns one (k, v); LIFO since 3.7.
  val solo = PyMap.empty[String, Int]()
  solo.update("only", 42)
  val (k, v) = solo.popItem()
  println(s"$k->$v")                               // only->42
  println(solo.size())                             // 0

  // PyMap toString is Scala-style Map(k -> v, ...).
  val one = PyMap.empty[String, Int]()
  one.update("k", 7)
  println(one)                                     // Map(k -> 7)

  // Empty PyMap renders as Map().
  println(PyMap.empty[String, Int]())              // Map()
