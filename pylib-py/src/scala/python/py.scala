package scala.python

/** Convenience aliases for the most common `scala.python.runtime` handles.
 *
 *  These are pure aliases — `py.List[Int]` is the same type as
 *  `scala.python.runtime.PyList[Int]`, and `py.List(1, 2, 3)` resolves to
 *  `PyList.apply(1, 2, 3)`. The compiler still intercepts every call site
 *  through the underlying `PyList` / `PyMap` / `PyTuple` symbols. */
object py:
  type List[T] = scala.python.runtime.PyList[T]
  val  List: scala.python.runtime.PyList.type = scala.python.runtime.PyList

  type Map[K, V] = scala.python.runtime.PyMap[K, V]
  val  Map: scala.python.runtime.PyMap.type = scala.python.runtime.PyMap

  type Tuple[T] = scala.python.runtime.PyTuple[T]
  val  Tuple: scala.python.runtime.PyTuple.type = scala.python.runtime.PyTuple
