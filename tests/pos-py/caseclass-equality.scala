object CaseclassEquality:
  case class Name(value: String)
  case class Wrap(a: Name, b: Name)
  object Store:
    case class Box[A](value: A)

@main def run(): Unit =
  val x = CaseclassEquality.Wrap(CaseclassEquality.Name("a"), CaseclassEquality.Name("b"))
  val y = CaseclassEquality.Wrap(CaseclassEquality.Name("a"), CaseclassEquality.Name("b"))
  val z = CaseclassEquality.Wrap(CaseclassEquality.Name("a"), CaseclassEquality.Name("c"))

  val anyX: Any = x
  val anyY: Any = y
  val prodX: Product = x

  println(x == y)
  println(x.equals(y))
  println(anyX == anyY)
  println(prodX == y)
  println(x == z)
  println(x.hashCode == y.hashCode)
  println(x.hashCode == z.hashCode)
  println(x.canEqual(y))

  val box1 = CaseclassEquality.Store.Box("x")
  val box2 = CaseclassEquality.Store.Box("x")
  val box3 = CaseclassEquality.Store.Box("y")
  val anyBox1: Any = box1
  val anyBox2: Any = box2
  println(box1 == box2)
  println(box1.equals(box2))
  println(anyBox1 == anyBox2)
  println(box1 == box3)
  println(box1.hashCode == box2.hashCode)
  println(box1.copy(value = "z"))
