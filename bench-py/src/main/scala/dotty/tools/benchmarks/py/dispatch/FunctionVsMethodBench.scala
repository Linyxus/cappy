package dotty.tools.benchmarks.py.dispatch

/** Function1 closure dispatch vs user virtual-method dispatch for the same
 *  arithmetic. Each side has a bimorphic and a monomorphic variant so the
 *  closure-carrier overhead can be compared directly against a plain virtual
 *  method call. */
trait Transform:
  def apply(x: Int): Int

class MulTransform(val factor: Int) extends Transform:
  def apply(x: Int): Int = x * factor

class AddTransform(val delta: Int) extends Transform:
  def apply(x: Int): Int = x + delta

class FunctionVsMethodBench:
  var size: Int = 0
  var fns: Array[Int => Int] = Array.empty
  var methods: Array[Transform] = Array.empty
  var monoFn: Array[Int => Int] = Array.empty
  var monoMethod: Array[Transform] = Array.empty

  def setup(size: Int): Unit =
    this.size = size
    fns = Array.tabulate(size) { i =>
      if i % 2 == 0 then (x: Int) => x * 3 else (x: Int) => x + 7
    }
    methods = Array.tabulate(size) { i =>
      if i % 2 == 0 then MulTransform(3) else AddTransform(7)
    }
    monoFn = Array.tabulate(size)(_ => (x: Int) => x * 3)
    monoMethod = Array.tabulate(size)(_ => MulTransform(3))

  val operations: Map[String, () => Any] = Map(
    "closureBimorphic" -> { () =>
      var s = 0L
      var i = 0
      while i < size do
        s += fns(i)(i % 97)
        i += 1
      s
    },
    "methodBimorphic" -> { () =>
      var s = 0L
      var i = 0
      while i < size do
        s += methods(i).apply(i % 97)
        i += 1
      s
    },
    "closureMono" -> { () =>
      var s = 0L
      var i = 0
      while i < size do
        s += monoFn(i)(i % 97)
        i += 1
      s
    },
    "methodMono" -> { () =>
      var s = 0L
      var i = 0
      while i < size do
        s += monoMethod(i).apply(i % 97)
        i += 1
      s
    },
  )

@main def main(): Unit = ()
