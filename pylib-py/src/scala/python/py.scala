package scala.python

object py:
  type List[T] = scala.python.runtime.PyList[T]
  val  List: scala.python.runtime.PyList.type = scala.python.runtime.PyList

  type Map[K, V] = scala.python.runtime.PyMap[K, V]
  val  Map: scala.python.runtime.PyMap.type = scala.python.runtime.PyMap

  type Tuple[T] = scala.python.runtime.PyTuple[T]
  val  Tuple: scala.python.runtime.PyTuple.type = scala.python.runtime.PyTuple

  val builtins: scala.python.runtime.PyBuiltins.type = scala.python.runtime.PyBuiltins
  export scala.python.runtime.PyBuiltins.importModule
