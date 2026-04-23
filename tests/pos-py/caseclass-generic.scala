object CaseclassGeneric:
  case class Pair[A, B](left: A, right: B)
  object Nest:
    case class Box[A](value: A, count: Int = 1)

  def swap[A, B](pair: Pair[A, B]): Pair[B, A] =
    Pair(pair.right, pair.left)

  def describe[A](box: Nest.Box[A]): String = box match
    case Nest.Box(value, count) => s"$value:$count"

@main def run(): Unit =
  val pair = CaseclassGeneric.Pair(1, "x")
  println(pair)
  println(CaseclassGeneric.swap(pair))
  println(pair == CaseclassGeneric.Pair(1, "x"))
  println(pair.copy(right = "y"))

  val nested = CaseclassGeneric.Nest.Box(CaseclassGeneric.Pair(2, "z"))
  println(nested)
  println(CaseclassGeneric.describe(nested))
  println(nested.copy(count = 3))
  println(CaseclassGeneric.Nest.Box("plain", 2))
