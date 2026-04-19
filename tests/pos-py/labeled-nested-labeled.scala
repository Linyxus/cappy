// Nested Labeled: a method-scope `return` triggered inside an inner
// `match` must not be caught by the outer `match`'s label. With the
// per-label `BaseException` subclass emission, each `Labeled` has its
// own class, so the inner `except _scpy_lbl_k:` does not catch the
// method-level `return` that Scala emits as a raise of a different
// class.

def allTail(n: Int): Int =
  // Both inner and outer matches exit at tail position. The outer
  // match is an expression (its value becomes `v`), and inside one arm
  // we exit via a method-scope return. The other arm returns a nested
  // match result.
  val v = n match
    case 0 =>
      "x" match
        case "x" => 1
        case _   => 2
    case _ => return -7 // jumps past both matches, directly out
  v * 10

def mixed(n: Int): Int =
  // Inner match returns a value to its own boundary; outer match
  // returns a value to its own boundary. No method-scope return here —
  // exercises nested expression-position labels with distinct targets.
  val v = n match
    case 0 =>
      "x" match
        case "x" => 11
        case _   => 22
    case 1 =>
      "y" match
        case "y" => 33
        case _   => 44
    case _ => 99
  v + 1

@main def labeledNestedLabeled(): Unit =
  println("allTail(0):" + allTail(0))
  println("allTail(9):" + allTail(9))
  println("mixed(0):" + mixed(0))
  println("mixed(1):" + mixed(1))
  println("mixed(7):" + mixed(7))
