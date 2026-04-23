object CaseclassInheritance:
  sealed trait Shape:
    def area: Int
    def describe: String

  case class Rect(width: Int, height: Int) extends Shape:
    def area: Int = width * height
    def describe: String = s"rect:$width:$height"

  case class Square(size: Int) extends Shape:
    def area: Int = size * size
    def describe: String = s"square:$size"

  case class Tagged[A](value: A, tag: String):
    def render: String = s"$tag=$value"

  def summarize(shape: Shape): String = shape match
    case Rect(width, height) => s"rect:${width * height}"
    case Square(size)        => s"square:${size * size}"

@main def run(): Unit =
  val rect = CaseclassInheritance.Rect(2, 3)
  val square = CaseclassInheritance.Square(4)
  val thin = CaseclassInheritance.Rect(1, 5)

  println(rect.describe + "|" + square.describe + "|" + thin.describe)
  println(rect.area + square.area + thin.area)
  println(CaseclassInheritance.summarize(CaseclassInheritance.Rect(3, 7)))
  println(rect == CaseclassInheritance.Rect(2, 3))
  println(square == CaseclassInheritance.Square(5))

  val tagged = CaseclassInheritance.Tagged(rect, "shape")
  println(tagged.render)
  println(tagged.copy(value = square))
