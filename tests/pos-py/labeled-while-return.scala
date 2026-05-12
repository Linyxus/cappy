// Method-scope `return` from inside a `while` loop body, plus a
// loop that completes normally. Exercises whatever PyIR shape `while`
// lowers to in this backend — the label escape has to leave the loop
// without the loop's own control structure short-circuiting it, and
// the normal-completion path has to still produce the right value.

def firstAbove(xs: Array[Int], threshold: Int): Int =
  var i = 0
  while i < xs.length do
    if xs(i) > threshold then return xs(i)
    i += 1
  -1

def sumAll(xs: Array[Int]): Int =
  var i = 0
  var acc = 0
  while i < xs.length do
    acc += xs(i)
    i += 1
  acc

@main def labeledWhileReturn(): Unit =
  val a = Array(1, 2, 3, 9, 4)
  val b = Array(1, 2, 3)
  println("first(a,2):" + firstAbove(a, 2))
  println("first(a,99):" + firstAbove(a, 99))
  println("first(b,0):" + firstAbove(b, 0))
  println("sum(a):" + sumAll(a))
  println("sum(b):" + sumAll(b))
