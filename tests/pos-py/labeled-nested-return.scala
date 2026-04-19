def foo(n: Int): Int =
  val x = n match
    case 0 => 42
    case _ => return 99  // method-scope return from inside a match arm
  x + 1000

@main def labeledNestedReturn(): Unit =
  println("f0:" + foo(0))
  println("f5:" + foo(5))
