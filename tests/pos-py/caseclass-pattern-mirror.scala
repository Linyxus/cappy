import scala.deriving.Mirror

object CaseclassMatchMirror:
  case class User(name: String, age: Int)
  case class Pair(a: Int, b: String)
  object Store:
    case class Box[A](value: A)

  final class PairProduct(first: Int, second: String) extends Product:
    def canEqual(that: Any): Boolean = true
    def productArity: Int = 2
    def productElement(n: Int): Any = n match
      case 0 => first
      case 1 => second
      case _ => throw new IndexOutOfBoundsException(n.toString)
    override def productElementName(n: Int): String = n match
      case 0 => "a"
      case 1 => "b"
      case _ => throw new IndexOutOfBoundsException(n.toString)
    override def productPrefix: String = "PairProduct"

  def describe(u: User): String = u match
    case User("Ada", age) => s"Ada:$age"
    case User(name, age) if age >= 40 => s"senior:$name:$age"
    case User(name, age) => s"$name:$age"

  def nestedBoxText(box: Store.Box[User]): String = box match
    case Store.Box(User(name, age)) => s"$name@$age"

@main def run(): Unit =
  println(CaseclassMatchMirror.describe(CaseclassMatchMirror.User("Ada", 37)))
  println(CaseclassMatchMirror.describe(CaseclassMatchMirror.User("Bob", 41)))
  println(CaseclassMatchMirror.describe(CaseclassMatchMirror.User("Cid", 19)))

  val good = CaseclassMatchMirror.Store.Box(CaseclassMatchMirror.User("Nia", 28))
  println(CaseclassMatchMirror.nestedBoxText(good))
  println(good == CaseclassMatchMirror.Store.Box(CaseclassMatchMirror.User("Nia", 28)))
  println(good.copy(value = CaseclassMatchMirror.User("Mia", 30)))

  val pairMirror = summon[Mirror.ProductOf[CaseclassMatchMirror.Pair]]
  val pair1 = pairMirror.fromProduct((42, "ok"))
  val pair2 = pairMirror.fromProduct(new CaseclassMatchMirror.PairProduct(42, "ok"))
  println(pair1)
  println(pair2)
  println(pair1 == CaseclassMatchMirror.Pair(42, "ok"))
  println(pair1 == pair2)
