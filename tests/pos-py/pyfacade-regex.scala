import scala.python.runtime.PyRegex

@main def pyfacadeRegex(): Unit =
  val compiled = PyRegex.compileWithFlags("(?<word>[A-Za-z]+)", PyRegex.Version1Flag)
  val iter = compiled.findIter("one 22 two", 0)
  println("iter-len:" + iter.length)
  println("iter-0:" + iter.get(0).matched())
  println("iter-1-name:" + iter.get(1).groupByName("word"))
  println("search-from:" + compiled.search("11 two 33", 3).groupByName("word"))
  println("sub:" + compiled.sub("x", "one two"))
  val subn = compiled.subn("x", "one two", 1)
  println("subn:" + subn.text + ":" + subn.count)
