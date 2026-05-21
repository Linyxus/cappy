package dotty.tools.benchmarks.py.interp

/** A stack-machine VM executing a generated, stack-balanced instruction
 *  array. The hot loop is one big sealed-trait `match` dispatching over
 *  the instruction at the program counter. */
sealed trait Instr
case class Push(value: Int) extends Instr
case class Add()            extends Instr
case class Sub()            extends Instr
case class Mul()            extends Instr
case class Dup()            extends Instr
case class Pop()            extends Instr
case class Negate()         extends Instr

class StackVMBench:
  var size: Int = 0
  var program: Array[Instr] = Array.empty

  def setup(size: Int): Unit =
    this.size = size
    val buf = new Array[Instr](size * 6)
    var i = 0
    var j = 0
    while i < size do
      // A stack-balanced block: net stack delta is zero.
      buf(j) = Push(i % 17);   j += 1
      buf(j) = Dup();          j += 1
      buf(j) = Add();          j += 1
      buf(j) = Push(i % 5 + 1); j += 1
      buf(j) = Mul();          j += 1
      buf(j) = Pop();          j += 1
      i += 1
    program = buf

  val operations: Map[String, () => Any] = Map(
    "run" -> { () =>
      val stack = new Array[Int](64)
      var sp = 0
      var i = 0
      while i < program.length do
        program(i) match
          case Push(v)  => stack(sp) = v; sp += 1
          case Add()    => sp -= 1; stack(sp - 1) += stack(sp)
          case Sub()    => sp -= 1; stack(sp - 1) -= stack(sp)
          case Mul()    => sp -= 1; stack(sp - 1) = stack(sp - 1) * stack(sp)
          case Dup()    => stack(sp) = stack(sp - 1); sp += 1
          case Pop()    => sp -= 1
          case Negate() => stack(sp - 1) = -stack(sp - 1)
        i += 1
      if sp > 0 then stack(0) else 0
    },
  )

@main def main(): Unit = ()
