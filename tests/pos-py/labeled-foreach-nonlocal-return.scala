// Non-local `return` from inside a `xs.foreach { ... }` callback —
// the very common Scala idiom that motivated method-scope labelled
// returns in the first place. The label escape leaves the closure
// object, crosses `foreach`'s internal call frame, and lands in
// the outer method's labelled catch.
//
// Catches: regression where the function-object body emits the
// label-escape raise but the closure invoker's frame swallows or
// re-wraps the exception class.

def findFirstNegative(xs: List[Int]): Int =
  xs.foreach: x =>
    if x < 0 then return x
  0

def sumUntilSentinel(xs: List[Int], sentinel: Int): Int =
  var total = 0
  xs.foreach: x =>
    if x == sentinel then return total
    total += x
  total

@main def labeledForeachNonlocalReturn(): Unit =
  println("ffn[1,2,3]:" + findFirstNegative(List(1, 2, 3)))
  println("ffn[1,-2,3]:" + findFirstNegative(List(1, -2, 3)))
  println("ffn[-7]:" + findFirstNegative(List(-7)))
  println("sus[1,2,9,3]:" + sumUntilSentinel(List(1, 2, 9, 3), 9))
  println("sus[1,2,3]:" + sumUntilSentinel(List(1, 2, 3), 9))
