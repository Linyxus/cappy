// Corner cases for `scala.python.runtime.PyTuple`: 1-tuple trailing comma,
// Python slicing/indexing edges, identity under concat (immutable
// tuple semantics), and stringification.

import scala.python.runtime.PyTuple
import scala.python.runtime.PyMap

@main def pytupleCorners(): Unit =
  // ---------------------------------------------------------------
  // construction — empty, single, multi; 1-tuple needs trailing comma
  // ---------------------------------------------------------------
  println(PyTuple[Int]().size())                         // 0
  println(PyTuple.empty[Int]().size())                   // 0
  println(PyTuple[Int]().isEmpty)                        // true

  val t1 = PyTuple(42)
  println(t1.size())                                     // 1
  println(t1(0))                                         // 42

  val t0z = PyTuple(0)
  println(t0z.isEmpty)                                   // false  (single 0 is non-empty)
  println(t0z.nonEmpty)                                  // true

  val t3 = PyTuple(1, 2, 3)
  println(t3.size())                                     // 3
  println(t3(0))                                         // 1
  println(t3(-1))                                        // 3      (Python negative index)
  println(t3(-3))                                        // 1

  // direct method on literal
  println(PyTuple("only")(0))                            // only

  // type ascription
  println((PyTuple(1, 2): PyTuple[Int]).size())          // 2

  // 100-element tuple round-trip
  val big = PyTuple(
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
    20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
    40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
    60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
    80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99
  )
  println(big.size())                                    // 100
  println(big(99))                                       // 99

  // ---------------------------------------------------------------
  // concat: read-only — original unchanged
  // ---------------------------------------------------------------
  val a = PyTuple(1, 2)
  val b = PyTuple(3, 4)
  val ab = a.concat(b)
  println(a.size())                                      // 2
  println(b.size())                                      // 2
  println(ab.size())                                     // 4
  println(ab(0))                                         // 1
  println(ab(3))                                         // 4

  // concat with empty
  println(PyTuple.empty[Int]().concat(PyTuple(1, 2)).size())  // 2
  println(PyTuple(1, 2).concat(PyTuple.empty[Int]()).size())  // 2

  // 1-tuple concat
  val c12 = PyTuple(1).concat(PyTuple(2))
  println(c12.size())                                    // 2
  println(c12(0))                                        // 1

  // ---------------------------------------------------------------
  // slice — Python clipping semantics
  // ---------------------------------------------------------------
  val sl = PyTuple(10, 20, 30, 40, 50)
  println(sl.slice(0, 100).size())                       // 5
  println(sl.slice(-2, 100).size())                      // 2
  println(sl.slice(0, 0).size())                         // 0
  println(sl.slice(3, 2).size())                         // 0
  println(sl.slice(1, 3)(0))                             // 20
  println(sl.slice(1, 3)(1))                             // 30

  // ---------------------------------------------------------------
  // search edges (read-only)
  // ---------------------------------------------------------------
  val rep = PyTuple(1, 1, 2, 3, 3, 3)
  println(rep.indexOf(3))                                // 3  (first match)
  println(rep.indexOf(99))                               // -1
  println(rep.count(3))                                  // 3
  println(rep.count(99))                                 // 0
  println(rep.contains(1))                               // true
  println(rep.contains(99))                              // false

  // ---------------------------------------------------------------
  // alias of immutable tuple is harmless
  // ---------------------------------------------------------------
  val orig = PyTuple(1, 2, 3)
  val ali  = orig
  println(orig.size())                                   // 3
  println(ali.size())                                    // 3

  // ---------------------------------------------------------------
  // tuples are hashable — PyTuple works as a PyMap key
  // ---------------------------------------------------------------
  val keyed = PyMap.empty[PyTuple[Int], String]()
  keyed.update(PyTuple(1, 2), "first")
  keyed.update(PyTuple(3, 4), "second")
  println(keyed.size())                                  // 2
  println(keyed(PyTuple(1, 2)))                          // first
  println(keyed(PyTuple(3, 4)))                          // second
  // structurally equal key lookups also work
  println(keyed.contains(PyTuple(1, 2)))                 // true

  // ---------------------------------------------------------------
  // println — Python repr style (not Scala-style); 1-tuple keeps comma
  // ---------------------------------------------------------------
  println(PyTuple.empty[Int]())                          // ()
  println(PyTuple(42))                                   // (42,)
  println(PyTuple(1, 2, 3))                              // (1, 2, 3)

  // ---------------------------------------------------------------
  // heterogeneous PyTuple[Any]
  // ---------------------------------------------------------------
  val het = PyTuple[Any](1, "x", true, null)
  println(het.size())                                    // 4
  println(het(0))                                        // 1
  println(het(1))                                        // x
  println(het(2))                                        // true
  println(het(3))                                        // null
