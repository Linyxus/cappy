// Iterator semantics for `PyList` / `PyTuple`. Key invariant:
// `_scpy_list_iter` snapshots the list to a tuple BEFORE returning the
// iterator, so mutations to the underlying list after `iterator()` are
// invisible. Scala `Iterator` defaults (`toList`, `map`, `foldLeft`,
// `length`, …) come from the inherited `AbstractIterator`.

import scala.python.runtime.PyList
import scala.python.runtime.PyTuple

@main def pylistPytupleIter(): Unit =
  // empty iterators
  println(PyList.empty[Int]().iterator().hasNext)         // false
  println(PyList.empty[Int]().iterator().toList)          // List()
  println(PyTuple.empty[Int]().iterator().hasNext)        // false
  println(PyTuple.empty[Int]().iterator().toList)         // List()

  // single-element
  println(PyList(7).iterator().toList)                    // List(7)
  println(PyTuple(7).iterator().toList)                   // List(7)

  // basic traversal
  println(PyList(1, 2, 3).iterator().toList)              // List(1, 2, 3)
  println(PyTuple(1, 2, 3).iterator().toList)             // List(1, 2, 3)

  // two iterators on the same list are independent
  val xs = PyList(1, 2, 3)
  val it1 = xs.iterator()
  val it2 = xs.iterator()
  println(it1.next())                                     // 1
  println(it2.next())                                     // 1
  println(it1.next())                                     // 2
  println(it2.next())                                     // 2

  // snapshot semantics: appending after iterator() is invisible
  val ys = PyList(1, 2, 3)
  val itSnap = ys.iterator()
  ys.append(99)
  println(itSnap.toList)                                  // List(1, 2, 3)

  // snapshot semantics: clear after iterator() is invisible
  val zs = PyList(1, 2, 3)
  val itClear = zs.iterator()
  zs.clear()
  println(itClear.toList)                                 // List(1, 2, 3)

  // exhaustion: second toList drains an empty iterator
  val ws = PyList(1, 2, 3)
  val itEx = ws.iterator()
  println(itEx.toList)                                    // List(1, 2, 3)
  println(itEx.toList)                                    // List()

  // for-comprehension over iterator
  val sb = StringBuilder()
  for x <- PyList(1, 2, 3).iterator() do sb.append(x).append(",")
  println(sb.toString)                                    // 1,2,3,

  // standard Scala Iterator operations on inherited defaults
  println(PyList(1, 2, 3).iterator().map(_ * 2).toList)   // List(2, 4, 6)
  println(PyList(1, 2, 3, 4).iterator().filter(_ % 2 == 0).toList) // List(2, 4)
  println(PyList(1, 2, 3, 4).iterator().foldLeft(0)(_ + _)) // 10
  println(PyList(1, 2, 3).iterator().mkString(", "))      // 1, 2, 3
  println(PyList(1, 2, 3).iterator().sum)                 // 6

  // length / size on an iterator is consuming — second call sees empty
  val itLen = PyList(1, 2, 3).iterator()
  println(itLen.length)                                   // 3
  println(itLen.toList)                                   // List()

  // heterogeneous traversal
  val sb2 = StringBuilder()
  PyList[Any](1, "x", true, null).iterator().foreach(v => sb2.append(v).append("|"))
  println(sb2.toString)                                   // 1|x|true|null|

  // PyTuple iterator with concat
  val tcat = PyTuple(1, 2).concat(PyTuple(3, 4))
  println(tcat.iterator().toList)                         // List(1, 2, 3, 4)

  // PyTuple slice iterator
  println(PyTuple(1, 2, 3, 4, 5).slice(1, 4).iterator().toList) // List(2, 3, 4)
