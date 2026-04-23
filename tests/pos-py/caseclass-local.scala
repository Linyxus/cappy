object CaseclassLocal:
  def makeLine(seed: Int): String =
    case class Local(x: Int, y: Int = seed + 1)

    val first = Local(seed)
    val second = Local(seed)
    s"$first:${first == second}:${first.copy(y = seed + 2)}:${first.productPrefix}:${first.productElementName(1)}"

  def matchLocal(seed: Int): String =
    case class Pair(left: Int, right: Int)

    Pair(seed, seed + 1) match
      case Pair(left, right) => s"$left->$right"

@main def run(): Unit =
  println(CaseclassLocal.makeLine(3))
  println(CaseclassLocal.makeLine(7))
  println(CaseclassLocal.matchLocal(10))
