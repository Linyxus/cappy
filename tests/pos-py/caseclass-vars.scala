object CaseclassVars:
  case class Counter(var value: Int, label: String)
  case class Pair(var left: Int, var right: Int)

@main def run(): Unit =
  val first = CaseclassVars.Counter(1, "hits")
  val second = CaseclassVars.Counter(1, "hits")
  println(first == second)

  first.value = 2
  println(first)
  println(first == second)

  second.value = 2
  println(first == second)
  println(first.copy(label = "views"))
  println(first.productElement(0))

  val pair = CaseclassVars.Pair(3, 4)
  println(pair)
  pair.left = 10
  pair.right = 20
  println(pair)
  println(pair.copy(right = 99))
