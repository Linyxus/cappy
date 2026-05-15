// Corner cases for `scala.python.PyList`: construction edges, Python
// indexing/slicing semantics, aliasing/mutation, side-effect ordering,
// search edges, and stringification. These behaviours are intentional
// (PyList is a thin handle on a Python `list`) — pin them so future
// refactors notice if they shift.

import scala.python.PyList
import scala.python.PyMap

@main def pylistCorners(): Unit =
  // ---------------------------------------------------------------
  // construction
  // ---------------------------------------------------------------

  // `PyList()` (zero-arg varargs) and `PyList.empty[T]()` are both empty.
  println(PyList[Int]().size())                          // 0
  println(PyList.empty[Int]().size())                    // 0

  // single-element literal
  val one = PyList(42)
  println(one.size())                                    // 1
  println(one(0))                                        // 42

  // direct method on literal (no intermediate val)
  println(PyList(10, 20, 30)(1))                         // 20
  println(PyList(10, 20, 30).size())                     // 3

  // type ascription does not break the optimizer
  println((PyList(1, 2): PyList[Int]).size())            // 2

  // varargs evaluate elements left-to-right
  val ord = StringBuilder()
  def tag(s: String, v: Int): Int = { ord.append(s); v }
  val abc = PyList(tag("a", 1), tag("b", 2), tag("c", 3))
  println(ord.toString)                                  // abc
  println(abc(2))                                        // 3

  // construction in a closure is re-evaluated per call
  val f: () => Int = () => PyList(1, 2, 3).size()
  println(f())                                           // 3
  println(f())                                           // 3

  // 100-element literal — wide varargs round-trip
  val big = PyList(
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
    20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
    40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
    60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
    80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99
  )
  println(big.size())                                    // 100
  println(big(0))                                        // 0
  println(big(99))                                       // 99

  // ---------------------------------------------------------------
  // indexing / slicing — Python semantics leak through
  // ---------------------------------------------------------------

  val xs = PyList(10, 20, 30, 40, 50)

  // negative indices read from the tail
  println(xs(-1))                                        // 50
  println(xs(-5))                                        // 10

  // slice clips to the list boundary (no exception)
  println(xs.slice(0, 100).size())                       // 5
  println(xs.slice(-2, 100).size())                      // 2
  println(xs.slice(0, 0).size())                         // 0
  println(xs.slice(3, 2).size())                         // 0   (a > b)

  // insert at size == append; insert past size clamps to append
  val ys = PyList(1, 2, 3)
  ys.insert(ys.size(), 99)
  println(ys.size())                                     // 4
  println(ys(3))                                         // 99
  ys.insert(100, 77)
  println(ys(ys.size() - 1))                             // 77   (clamped)
  ys.insert(0, 0)
  println(ys(0))                                         // 0    (prepend via insert)

  // removeAt returns the removed value; remove returns Boolean
  val zs = PyList(1, 2, 3, 2, 1)
  println(zs.removeAt(0))                                // 1
  println(zs.remove(2))                                  // true (removes first)
  println(zs.remove(99))                                 // false
  println(zs.size())                                     // 3

  // search edges
  val ss = PyList(1, 1, 2, 3, 3, 3)
  println(ss.indexOf(3))                                 // 3    (first match)
  println(ss.indexOf(99))                                // -1
  println(ss.count(3))                                   // 3
  println(ss.count(99))                                  // 0
  println(ss.contains(1))                                // true
  println(ss.contains(99))                               // false

  // ---------------------------------------------------------------
  // aliasing and mutation
  // ---------------------------------------------------------------

  // val a = b shares the same underlying Python list
  val a = PyList(1, 2)
  val b = a
  b.append(3)
  println(a.size())                                      // 3

  // copy() makes an independent shallow copy
  val c = a.copy()
  c.append(99)
  println(a.size())                                      // 3
  println(c.size())                                      // 4

  // self-extend doubles the list (CPython snapshots length)
  val se = PyList(1, 2)
  se.extend(se)
  println(se.size())                                     // 4
  println(se(2))                                         // 1
  println(se(3))                                         // 2

  // self-concat creates a new list of double length
  val sc1 = PyList(1, 2)
  val sc2 = sc1.concat(sc1)
  println(sc1.size())                                    // 2
  println(sc2.size())                                    // 4

  // concat decouples from later mutation of the original
  val d1 = PyList(1, 2)
  val d2 = d1.concat(d1)
  d1.append(99)
  println(d2.size())                                     // 4

  // clear in place; alias sees the cleared state
  val e1 = PyList(1, 2, 3)
  val e2 = e1
  e1.clear()
  println(e2.size())                                     // 0
  e1.append(7)
  println(e2.size())                                     // 1

  // sort / reverse mutate in place
  val rs = PyList(3, 1, 4, 1, 5, 9, 2, 6)
  rs.sort()
  println(rs(0))                                         // 1
  println(rs(rs.size() - 1))                             // 9
  rs.reverse()
  println(rs(0))                                         // 9

  // self-cycle does not crash; size reflects the single appended ref
  val cyc = PyList.empty[Any]()
  cyc.append(1)
  cyc.append(cyc)
  println(cyc.size())                                    // 2
  // not printing cyc itself — Python repr uses `[...]` for the cycle
  // but the exact spacing is not worth pinning.

  // ---------------------------------------------------------------
  // side-effect ordering of receiver and arguments
  // ---------------------------------------------------------------

  // receiver is evaluated exactly once
  def mkList(): PyList[Int] = { println("mk"); PyList(1, 2) }
  println(mkList().size())                               // mk \n 2

  // update: receiver, then arg0, then arg1, then mutation
  val u = PyList(0, 0, 0)
  val log = StringBuilder()
  def tagU(s: String, v: Int): Int = { log.append(s); v }
  u.update(tagU("a", 1), tagU("b", 42))
  println(log.toString)                                  // ab
  println(u(1))                                          // 42

  // ---------------------------------------------------------------
  // heterogeneous PyList[Any]
  // ---------------------------------------------------------------

  val het = PyList[Any](1, "x", true, null)
  println(het.size())                                    // 4
  println(het.contains(null))                            // true
  println(het.contains("x"))                             // true
  // Python: `1 == True`, so `[1, ..., True].index(True)` returns 0,
  // not 2. Pinned to document this leak through `__eq__`.
  println(het.indexOf(true))                             // 0

  // ---------------------------------------------------------------
  // println output — Python `str(list)` repr (not Scala-style)
  // ---------------------------------------------------------------
  println(PyList.empty[Int]())                           // []
  println(PyList(1, 2, 3))                               // [1, 2, 3]

  // ---------------------------------------------------------------
  // store PyList in PyMap value — mutation through map reflects
  // ---------------------------------------------------------------
  val m = PyMap.empty[String, PyList[Int]]()
  m.update("k", PyList(1, 2))
  m("k").append(3)
  println(m("k").size())                                 // 3
