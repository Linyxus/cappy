// Pattern matching on `PyList[?]` and `PyTuple[?]`. The runtime
// distinguishes:
//   * Python `list`   matches `case _: PyList[?]`
//   * bare Python `tuple` (NOT a `_scpy_ScalaTuple` subclass) matches
//     `case _: PyTuple[?]`
//   * `_scpy_ScalaTuple` matches `case _: Tuple` / `case _: TupleN`
//     but NOT `case _: PyTuple[?]`
//   * a bare `PyTuple` does NOT match `case _: Tuple` (the recent fix
//     in `PyIRRuntime.scala` excludes non-`_scpy_ScalaTuple` tuples
//     from `scala.Tuple*` membership)

import scala.python.PyList
import scala.python.PyTuple

def tagOf(x: Any): String = x match
  case _: PyList[?]  => "pylist"
  case _: PyTuple[?] => "pytuple"
  case _: Tuple      => "tuple"
  case _             => "other"

def isTuple(x: Any): Boolean = x match
  case _: Tuple => true
  case _        => false

def isTuple2(x: Any): Boolean = x match
  case _: Tuple2[?, ?] => true
  case _               => false

def isPyTuple(x: Any): Boolean = x match
  case _: PyTuple[?] => true
  case _             => false

def isPyList(x: Any): Boolean = x match
  case _: PyList[?] => true
  case _            => false

@main def pylistPytuplePattern(): Unit =
  // positive matches
  println(tagOf(PyList(1, 2, 3)))                           // pylist
  println(tagOf(PyTuple(1, 2, 3)))                          // pytuple
  println(tagOf((1, 2)))                                    // tuple
  println(tagOf("string"))                                  // other
  println(tagOf(42))                                        // other

  // PyTuple must NOT match `case _: Tuple` (after the runtime fix)
  println(isTuple(PyTuple(1, 2, 3)))                        // false
  println(isTuple(PyTuple(1)))                              // false
  println(isTuple(PyTuple.empty[Int]()))                    // false

  // PyTuple must NOT match `case _: Tuple2`
  println(isTuple2(PyTuple(1, 2)))                          // false

  // Scala tuples still match `Tuple` and `Tuple2`
  println(isTuple((1, 2)))                                  // true
  println(isTuple((1, 2, 3)))                               // true
  println(isTuple2((1, 2)))                                 // true
  println(isTuple2((1, 2, 3)))                              // false (arity mismatch)

  // PyList vs PyTuple — disjoint
  println(isPyList(PyList(1)))                              // true
  println(isPyList(PyTuple(1)))                             // false
  println(isPyTuple(PyTuple(1)))                            // true
  println(isPyTuple(PyList(1)))                             // false

  // Scala tuple is NOT a PyTuple
  println(isPyTuple((1, 2)))                                // false

  // empty containers still pattern-match
  println(isPyList(PyList.empty[Int]()))                    // true
  println(isPyTuple(PyTuple.empty[Int]()))                  // true

  // pattern match inside iteration over heterogeneous list
  val mixed = PyList[Any](1, "x", true, null, PyList(7), PyTuple(8))
  val sb = StringBuilder()
  mixed.iterator().foreach { v =>
    val tag = v match
      case _: Int        => "int"
      case _: String     => "str"
      case _: Boolean    => "bool"
      case null          => "null"
      case _: PyList[?]  => "plist"
      case _: PyTuple[?] => "ptup"
      case _             => "?"
    sb.append(tag).append(",")
  }
  println(sb.toString)                                      // int,str,bool,null,plist,ptup,
