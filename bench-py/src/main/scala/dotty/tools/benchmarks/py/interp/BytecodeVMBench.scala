package dotty.tools.benchmarks.py.interp

/** A register/stack bytecode VM dispatching on `Int` opcode literals
 *  in a flat instruction array. Contrasts with `StackVMBench` to isolate
 *  Int-literal `match` dispatch from sealed-trait dispatch. */
object Op:
  val PUSH  = 0
  val ADD   = 1
  val SUB   = 2
  val MUL   = 3
  val DUP   = 4
  val POP   = 5
  val NEG   = 6
  val LOAD  = 7
  val STORE = 8

class BytecodeVMBench:
  var size: Int = 0
  var bytecode: Array[Int] = Array.empty

  def setup(size: Int): Unit =
    this.size = size
    // Worst-case width: every step is a 2-word op.
    val buf = new Array[Int](size * 2)
    var i = 0
    var j = 0
    while i < size do
      (i % 7) match
        case 0 => buf(j) = Op.PUSH;  j += 1; buf(j) = i % 13; j += 1
        case 1 => buf(j) = Op.ADD;   j += 1
        case 2 => buf(j) = Op.MUL;   j += 1
        case 3 => buf(j) = Op.STORE; j += 1; buf(j) = i % 16;  j += 1
        case 4 => buf(j) = Op.LOAD;  j += 1; buf(j) = i % 16;  j += 1
        case 5 => buf(j) = Op.SUB;   j += 1
        case _ => buf(j) = Op.DUP;   j += 1
      i += 1
    bytecode = java.util.Arrays.copyOf(buf, j)

  val operations: Map[String, () => Any] = Map(
    "run" -> { () =>
      val stack = new Array[Int](128)
      val regs  = new Array[Int](16)
      var sp = 0
      var ip = 0
      while ip < bytecode.length do
        val op = bytecode(ip); ip += 1
        op match
          case Op.PUSH  => val v = bytecode(ip); ip += 1; stack(sp) = v; sp += 1
          case Op.ADD   => if sp >= 2 then { sp -= 1; stack(sp - 1) += stack(sp) }
          case Op.MUL   => if sp >= 2 then { sp -= 1; stack(sp - 1) = stack(sp - 1) * stack(sp) }
          case Op.SUB   => if sp >= 2 then { sp -= 1; stack(sp - 1) -= stack(sp) }
          case Op.DUP   => if sp >= 1 then { stack(sp) = stack(sp - 1); sp += 1 }
          case Op.POP   => if sp >= 1 then sp -= 1
          case Op.NEG   => if sp >= 1 then stack(sp - 1) = -stack(sp - 1)
          case Op.LOAD  => val r = bytecode(ip); ip += 1; stack(sp) = regs(r); sp += 1
          case Op.STORE => val r = bytecode(ip); ip += 1; if sp >= 1 then { sp -= 1; regs(r) = stack(sp) }
          case _        => ()
      var acc = if sp > 0 then stack(0) else 0
      var k = 0
      while k < regs.length do { acc += regs(k); k += 1 }
      acc
    },
  )

@main def main(): Unit = ()
