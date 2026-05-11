import scala.python.*

@extern("adder", "Adder")
class Adder(seed: Int) extends PyAny:
  def add(a: Int, b: Int): Int = native

@main def adderPy(): Unit =
  val adder = Adder(42)
  println(adder.add(100, 1))
