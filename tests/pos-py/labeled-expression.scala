class Chooser(x: Int, tag: String)

// Match in super-call argument position — what the scala-js javalib
// uses heavily, e.g. `InvalidPropertiesFormatException`. Before the
// Labeled-in-expression fix this crashed with "unhandled expression
// form Labeled".
class Wrapped(primary: Any)
    extends Chooser(
      primary match
        case n: Int => n
        case s: String => s.length
        case _ => -1,
      primary match
        case _: Int => "int"
        case _: String => "str"
        case _ => "other"
    ):
  val x0 = primary

@main def labeledExpression(): Unit =
  // 1. match as val rhs (common pattern).
  val a: String = 1 match
    case 1 => "one"
    case 2 => "two"
    case _ => "other"
  println("val-rhs:" + a)

  // 2. match as argument in a method call.
  def describe(s: String): String = "describe:" + s
  val b = describe(
    42 match
      case 0 => "zero"
      case 42 => "ans"
      case _ => "?"
  )
  println("arg:" + b)

  // 3. match inside string concatenation (expression as sub-expression).
  val c = "label=" + (
    7 match
      case n if n < 0 => "neg"
      case 0 => "zero"
      case _ => "pos"
  )
  println("concat:" + c)

  // 4. Nested match inside an arm.
  val d = 1 match
    case 1 =>
      "a" match
        case "a" => "inner-a"
        case _   => "inner-other"
    case _ => "outer-fallthrough"
  println("nested:" + d)

  // 5. Match-in-super-call-arg — the original Throwables.scala failure.
  val w = new Wrapped("hello")
  println("wrapped:" + w.x0)

  // 6. Match returning different types merged by LUB.
  val e: Any =
    "hi" match
      case "hi" => 42
      case _ => "bye"
  println("lub:" + e)

  // 7. Match guard that doesn't fall into the matchEnd — the
  //    last-case `_` catches. Ensures the synthetic fallthrough works.
  val f = 10 match
    case n if n > 100 => "big"
    case n if n > 5   => "mid"
    case _            => "small"
  println("guard:" + f)
