// Pins `scala.python.runtime.PyList`'s single-shot operations. A `PyList[T]`
// value at runtime IS a Python `list`; every method below is
// intercepted in `GenPython.genPyListInstanceCall` and lowered to a
// `_scpy_list_*` runtime helper or a Python list literal.

import scala.python.runtime.PyList

@main def pylistBasic(): Unit =
  val xs = PyList.empty[Int]()
  println(xs.size())                   // 0
  println(xs.isEmpty)                  // true
  println(xs.nonEmpty)                 // false

  xs.append(1)
  xs.append(2)
  xs.append(3)
  println(xs.size())                   // 3
  println(xs.isEmpty)                  // false
  println(xs.nonEmpty)                 // true

  println(xs(0))                       // 1
  println(xs(2))                       // 3
  xs.update(1, 20)
  println(xs(1))                       // 20

  xs.prepend(0)
  println(xs(0))                       // 0
  println(xs.size())                   // 4
  xs.insert(2, 99)
  println(xs(2))                       // 99
  println(xs.size())                   // 5

  println(xs.contains(99))             // true
  println(xs.contains(999))            // false
  println(xs.indexOf(99))              // 2
  println(xs.indexOf(999))             // -1
  println(xs.count(99))                // 1

  println(xs.removeAt(2))              // 99
  println(xs.size())                   // 4
  println(xs.remove(20))               // true
  println(xs.remove(999))              // false
  println(xs.size())                   // 3

  // varargs construction
  val ys = PyList(10, 20, 30)
  println(ys.size())                   // 3
  println(ys(0))                       // 10
  println(ys(2))                       // 30

  // copy / concat / slice
  val cp = xs.copy()
  cp.append(42)
  println(cp.size())                   // 4
  println(xs.size())                   // 3   (copy was independent)

  val zs = xs.concat(ys)
  println(zs.size())                   // 6
  println(zs(3))                       // 10

  val sl = ys.slice(0, 2)
  println(sl.size())                   // 2
  println(sl(0))                       // 10
  println(sl(1))                       // 20

  // sort / reverse
  val rs = PyList(3, 1, 2)
  rs.sort()
  println(rs(0))                       // 1
  println(rs(2))                       // 3
  rs.reverse()
  println(rs(0))                       // 3
  println(rs(2))                       // 1

  // extend / clear
  xs.extend(ys)
  println(xs.size())                   // 6
  xs.clear()
  println(xs.size())                   // 0
  println(xs.isEmpty)                  // true
