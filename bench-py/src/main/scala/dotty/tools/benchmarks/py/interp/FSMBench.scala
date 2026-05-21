package dotty.tools.benchmarks.py.interp

/** A finite state machine driven by a generated symbol stream. The hot
 *  loop matches on a `(State, Sym)` tuple of two sealed-trait values and
 *  allocates a fresh state object per transition. */
sealed trait State
case class S0() extends State
case class S1() extends State
case class S2() extends State
case class S3() extends State
case class S4() extends State

sealed trait Sym
case class A()     extends Sym
case class B()     extends Sym
case class C()     extends Sym
case class Reset() extends Sym

class FSMBench:
  var size: Int = 0
  var input: Array[Sym] = Array.empty

  def setup(size: Int): Unit =
    this.size = size
    val in = new Array[Sym](size)
    var i = 0
    while i < size do
      in(i) =
        if i % 8 == 7 then Reset()
        else (i % 3) match
          case 0 => A()
          case 1 => B()
          case _ => C()
      i += 1
    input = in

  val operations: Map[String, () => Any] = Map(
    "run" -> { () =>
      var state: State = S0()
      var accepts = 0
      var i = 0
      while i < input.length do
        state = (state, input(i)) match
          case (S0(), A())  => S1()
          case (S1(), B())  => S2()
          case (S2(), C())  => accepts += 1; S3()
          case (S3(), A())  => S1()
          case (_, Reset()) => S0()
          case _            => S4()
        i += 1
      accepts
    },
  )

@main def main(): Unit = ()
