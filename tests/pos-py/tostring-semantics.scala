class Rendered(val value: Int):
  override def toString(): String =
    "Rendered(" + value + ")"

class Plain

object Plain:
  override def toString(): String =
    "plain companion"

case class Pair(x: Int, y: String)

@main def tostringSemantics(): Unit =
  val rendered = Rendered(7)
  val anyRendered: Any = rendered
  println("custom-direct:" + rendered.toString)
  println("custom-any:" + anyRendered.toString)
  println("custom-plus:" + rendered)

  val ch: Char = 'A'
  val bool = true
  val n = 42
  val unit = ()
  println("primitives:" + ch.toString + ":" + bool.toString + ":" + n.toString + ":" + unit.toString)
  println("primitive-plus:" + ch + ":" + bool + ":" + n + ":" + unit)

  println("case:" + Pair(1, "x").toString)
  println("case-plus:" + Pair(2, "y"))

  val xs = List(0, 1, 2)
  val ys = xs ++ xs.reverse
  println("list-direct:" + ys.toString)
  println("list-plus:" + ys)

  val plain = Plain()
  val plainText = plain.toString
  println("companion:" + Plain.toString + ":" + (plainText == "plain companion") + ":" + plainText.contains("Plain@"))
