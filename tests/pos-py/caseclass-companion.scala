object CaseclassCompanion:
  case class Token(value: String, id: Int = 0)
  object Token:
    def fromInt(n: Int): Token = Token("tok", n)
    def join(a: Token, b: Token): Token = Token(a.value + "+" + b.value, a.id + b.id)

  case class Packet(token: Token, size: Int)

  def describe(value: Any): String = value match
    case Token(name, id)          => s"token:$name:$id"
    case Packet(Token(name, id), size) => s"packet:$name:$id:$size"
    case _                        => "other"

@main def run(): Unit =
  val first = CaseclassCompanion.Token.fromInt(7)
  val second = CaseclassCompanion.Token("zip", 2)
  val packet = CaseclassCompanion.Packet(second, 9)

  println(first)
  println(CaseclassCompanion.Token.join(first, second))
  println(CaseclassCompanion.describe(first))
  println(CaseclassCompanion.describe(packet))
  println(CaseclassCompanion.Token.unapply(first))
  println(packet.copy(token = first))
  println(packet == CaseclassCompanion.Packet(CaseclassCompanion.Token("zip", 2), 9))
