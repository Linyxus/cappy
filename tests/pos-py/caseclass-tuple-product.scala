import scala.deriving.Mirror

object CaseclassTupleProduct:
  case class Triple(a: Int, b: String, c: Boolean)
  case class Wrapper[A](value: A, count: Int)

  final class TripleProduct(first: Int, second: String, third: Boolean) extends Product:
    def canEqual(that: Any): Boolean = true
    def productArity: Int = 3
    def productElement(n: Int): Any = n match
      case 0 => first
      case 1 => second
      case 2 => third
      case _ => throw new IndexOutOfBoundsException(n.toString)
    override def productElementName(n: Int): String = n match
      case 0 => "a"
      case 1 => "b"
      case 2 => "c"
      case _ => throw new IndexOutOfBoundsException(n.toString)
    override def productPrefix: String = "TripleProduct"

@main def run(): Unit =
  val triple = CaseclassTupleProduct.Triple(1, "two", true)
  println(Tuple.fromProductTyped(triple))
  println(triple.productElementNames.mkString("|"))
  println(triple.productIterator.mkString("|"))

  val mirror = summon[Mirror.ProductOf[CaseclassTupleProduct.Triple]]
  val rebuilt = mirror.fromProduct(new CaseclassTupleProduct.TripleProduct(1, "two", true))
  println(rebuilt)
  println(rebuilt == triple)

  val wrapped = CaseclassTupleProduct.Wrapper(triple, 2)
  println(Tuple.fromProductTyped(wrapped))
  println(wrapped.copy(count = 5))
