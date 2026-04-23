object CaseclassMethods:
  case class Fraction(numerator: Int, denominator: Int):
    def normalized: Fraction =
      if denominator < 0 then Fraction(-numerator, -denominator) else this

    def add(other: Fraction): Fraction =
      Fraction(
        numerator * other.denominator + other.numerator * denominator,
        denominator * other.denominator
      ).normalized

  case class LabeledFraction(value: Fraction, label: String):
    def render: String = s"$label=$value"

@main def run(): Unit =
  val half = CaseclassMethods.Fraction(1, 2)
  val third = CaseclassMethods.Fraction(1, 3)
  val labeled = CaseclassMethods.LabeledFraction(half, "x")

  println(half.add(third))
  println(CaseclassMethods.Fraction(1, -2).normalized)
  println(labeled.render)
  println(labeled.copy(value = third))
  println(labeled == CaseclassMethods.LabeledFraction(CaseclassMethods.Fraction(1, 2), "x"))
  println(labeled.value.productArity)
