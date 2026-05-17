// Cross-container interop: PyList of PyTuple, PyTuple of PyList,
// PyMap with PyList / PyTuple as values, mutating an inner PyList
// through an outer immutable PyTuple, and deep nesting.

import scala.python.runtime.PyList
import scala.python.runtime.PyTuple
import scala.python.runtime.PyMap

@main def pylistPytupleMixed(): Unit =
  // PyList of PyTuple
  val listOfTup = PyList(PyTuple(1, 2), PyTuple(3, 4, 5))
  println(listOfTup.size())                                  // 2
  println(listOfTup(0).size())                               // 2
  println(listOfTup(1).size())                               // 3
  println(listOfTup(1)(2))                                   // 5

  // PyTuple of PyList — outer is immutable but inner list is mutable
  val tupOfList = PyTuple(PyList(1, 2), PyList(3))
  println(tupOfList.size())                                  // 2
  tupOfList(0).append(99)
  println(tupOfList(0).size())                               // 3
  println(tupOfList(0)(2))                                   // 99

  // nested PyList
  val nested = PyList(PyList(1, 2), PyList(3, 4, 5))
  println(nested.size())                                     // 2
  println(nested(0).size())                                  // 2
  println(nested(1)(2))                                      // 5
  nested(0).append(7)
  println(nested(0).size())                                  // 3

  // nested PyTuple
  val ntup = PyTuple(PyTuple(1, 2), PyTuple(3, 4, 5))
  println(ntup(1).size())                                    // 3
  println(ntup(0)(0))                                        // 1

  // PyMap with PyList values, mutation propagates
  val m1 = PyMap.empty[String, PyList[Int]]()
  m1.update("a", PyList(1, 2))
  m1.update("b", PyList.empty[Int]())
  m1("a").append(3)
  m1("b").append(99)
  println(m1("a").size())                                    // 3
  println(m1("b").size())                                    // 1

  // PyMap with PyTuple values
  val m2 = PyMap.empty[String, PyTuple[Int]]()
  m2.update("k", PyTuple(1, 2, 3))
  println(m2("k").size())                                    // 3
  println(m2("k")(1))                                        // 2

  // PyList of PyMap
  val lm = PyList(PyMap.empty[String, Int](), PyMap.empty[String, Int]())
  lm(0).update("x", 1)
  lm(1).update("y", 2)
  println(lm(0)("x"))                                        // 1
  println(lm(1)("y"))                                        // 2

  // 5-level deep PyList
  val deep = PyList(PyList(PyList(PyList(PyList(0)))))
  println(deep.size())                                       // 1
  println(deep(0)(0)(0)(0)(0))                               // 0

  // 5-level deep PyTuple
  val dtup = PyTuple(PyTuple(PyTuple(PyTuple(PyTuple(7)))))
  println(dtup(0)(0)(0)(0)(0))                               // 7

  // tuple containing a list — list survives mutation visible inside tuple
  val tx = PyTuple(PyList(10), PyList(20))
  tx(0).append(11)
  tx(1).append(21)
  println(tx(0).size())                                      // 2
  println(tx(1).size())                                      // 2
  println(tx(0)(1))                                          // 11
  println(tx(1)(1))                                          // 21
