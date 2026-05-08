// Pins `scala.python.PyMap`'s single-shot operations. A `PyMap[K, V]`
// value at runtime IS a Python `dict`; every method below is
// intercepted in `GenPython.genPyMapInstanceCall` and lowered to a
// `_scpy_dict_*` runtime helper or a Python dict literal.

import scala.python.PyMap

@main def pymapBasic(): Unit =
  val m = PyMap.empty[String, Int]()
  println(m.size())                    // 0
  println(m.isEmpty)                   // true
  println(m.nonEmpty)                  // false

  m.update("a", 1)
  m.update("b", 2)
  m.update("c", 3)
  println(m.size())                    // 3
  println(m.isEmpty)                   // false
  println(m.nonEmpty)                  // true

  println(m.contains("a"))             // true
  println(m.contains("z"))             // false
  println(m("a"))                      // 1
  println(m.get("z"))                  // null
  println(m.getOrElse("z", 99))        // 99
  println(m.getOrElse("a", 99))        // 1

  println(m.setDefault("a", 100))      // 1   (already present)
  println(m.setDefault("d", 4))        // 4   (inserted)
  println(m.contains("d"))             // true

  m.delete("d")
  println(m.contains("d"))             // false

  println(m.pop("c"))                  // 3
  println(m.contains("c"))             // false
  println(m.popOrElse("c", -1))        // -1  (already removed)
  println(m.popOrElse("b", -1))        // 2

  m.clear()
  println(m.size())                    // 0
  println(m.isEmpty)                   // true
