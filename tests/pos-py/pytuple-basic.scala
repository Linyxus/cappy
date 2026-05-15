// Pins `scala.python.PyTuple`'s read-only operations. A `PyTuple[T]`
// value at runtime IS a bare Python `tuple` — NOT the same thing as a
// Scala `Tuple{N}`, which lowers through `_scpy_ScalaTuple(...)`.

import scala.python.PyTuple

@main def pytupleBasic(): Unit =
  val empty = PyTuple.empty[Int]()
  println(empty.size())                // 0
  println(empty.isEmpty)               // true
  println(empty.nonEmpty)              // false

  val t = PyTuple(1, 2, 3)
  println(t.size())                    // 3
  println(t.isEmpty)                   // false
  println(t.nonEmpty)                  // true
  println(t(0))                        // 1
  println(t(2))                        // 3

  println(t.contains(2))               // true
  println(t.contains(99))              // false
  println(t.indexOf(2))                // 1
  println(t.indexOf(99))               // -1
  println(t.count(2))                  // 1

  val u = t.concat(PyTuple(4, 5))
  println(u.size())                    // 5
  println(u(3))                        // 4
  println(u(4))                        // 5

  val sl = t.slice(0, 2)
  println(sl.size())                   // 2
  println(sl(0))                       // 1
  println(sl(1))                       // 2

  // single-element tuple — proves the trailing-comma case works
  val one = PyTuple(7)
  println(one.size())                  // 1
  println(one(0))                      // 7
